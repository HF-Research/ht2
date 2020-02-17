library(data.table)
library(magrittr)
library(sp)
library(rgeos)
library(maptools)
library(sf)
library(leaflet)
library(dplyr)
# library(spdep)
edu <- fread("data/edu_description.csv", encoding = "UTF-8")
edu <- edu[, lapply(.SD, enc2native)]
# OUTCOME DATA ------------------------------------------------------------
data_files <-
  list.files(path = "data/",
             pattern = "export_diag",
             full.names = TRUE)
export_diag <- lapply(data_files, fread, encoding = "UTF-8") %>% rbindlist()

export_opr <- fread("data/export_opr.txt", encoding = "UTF-8")
export_med <- fread("data/export_med.txt", encoding = "UTF-8")

preProccess <- function(export_dat) {
  dat <- split(export_dat, by = "outcome") %>%
    lapply(., split, by = "aggr_level")
  
  lapply(dat, function(outcome) {
    out <- lapply(outcome, function(aggr_level) {
      aggr_level[, `:=` (outcome = NULL, aggr_level = NULL)]
      
      # !!!!! DO NOT CHANGE !!! unless you have checked with it does not
      # interfere with cbind operation in dtCast()
      setkey(aggr_level, sex, grouping, year)
    })
    
    # Change edu to factor to ensure correct ordering
    out$edu[, grouping := factor(tolower(grouping),
                                 levels = tolower(c(
                                   "Basic",
                                   "Secondary",
                                   "Tertiary",
                                   "Postgraduate",
                                   "Unknown"
                                 )))]
    out
  })
}

dat_opr <- preProccess(export_opr)
dat_med <-  preProccess(export_med)
dat_diag <-  preProccess(export_diag)
shiny_dat_en <- c(dat_opr, dat_med, dat_diag)

cleanGeoData <- function(x) {
  # Remove unknowns from Region. and remove Christiansoe
  lapply(x, function(outcome) {
    outcome$region <- outcome$region[grouping != "Unknown", ]
    outcome$kom <- outcome$kom[grouping != "Christiansø", ]
    outcome
  })
  
}

setNAtoZero <- function(x) {
  lapply(x, function(outcome) {
    lapply(outcome, function(aggr_level) {
      data_vars <- grep("count|rate", colnames(aggr_level), value = TRUE)
      aggr_level[, (data_vars) := lapply(.SD, function(i) {
        i[is.na(i)] <- 0L
        i
      }),
      .SDcols = data_vars]
      
    })
  })
}


shiny_dat_en <- cleanGeoData(shiny_dat_en)
shiny_dat_en <- setNAtoZero(shiny_dat_en)
saveRDS(shiny_dat_en, file = "data/shiny_dat_en.rds")

# DANISH LANGUAGE SUPPORT -------------------------------------------------

# Make english lowercase for matching
edu[, edu_name_en := tolower(edu_name_en)]
makeDanish <- function(dat) {
  # Change english education labels to Danish labels
  lapply(dat, function(outcome) {
    outcome$edu <-
      merge(
        outcome$edu,
        edu[, .(edu_name_dk, edu_name_en)],
        by.x = "grouping",
        by.y = "edu_name_en",
        all.x = TRUE
      )
    outcome$edu[, `:=` (grouping = edu_name_dk)]
    outcome$edu[, `:=` (edu_name_dk = NULL)]
    
    # Turn DK edu into factor
    outcome$edu[, `:=` (grouping = factor(
      grouping,
      levels = c(
        edu[, edu_name_dk]
      )
    ))]
    setkey(outcome$edu, sex, grouping, year)
    outcome
  })
}
shiny_dat_en$d10$edu$grouping

shiny_dat_dk <- makeDanish(shiny_dat_en)
saveRDS(shiny_dat_dk, file = "data/shiny_dat_dk.rds")



# SF APPROACH -------------------------------------------------------------
l2 <- readRDS("data/DNK_adm2.rds")

l2 <- st_as_sf(l2, coords = c("x", "y"))
l2 <-
  l2 %>%
  dplyr::select(OBJECTID, NAME_1, NAME_2) %>%
  rename(id = OBJECTID,
         name_dk = NAME_2,
         region = NAME_1)



l2[l2$name_dk == "Århus", ]$name_dk <- "Aarhus"
l2[l2$name_dk == "Vesthimmerland", ]$name_dk <- "Vesthimmerlands"
l2[l2$region == "Midtjylland", ]$region <- "Midtjydlland"

l2$name_dk
# Delete Christiansoe polygon
l2 <- l2[l2$name_dk != "Christiansø", ]
l2$name_dk

# Move Bornholm
bornholm <- l2 %>% filter(name_dk == "Bornholm")
b.geo <- st_geometry(bornholm) # Subset geometry of of object
b.geo <- b.geo + c(-2.6, 1.35) # Move object
st_geometry(bornholm) <- b.geo # Re-assign geometry to object

# Replace bornholm in main sf object
l2[l2$name_dk == "Bornholm", ] <- bornholm

# Union kommune to regions
regions <- unique(l2$region)
geo_tmp <- list() # hold unioned regions
attr_tmp <- list() # hold attributes for regions
out_sf <- list()
for (reg in regions) {
  geo_tmp[[reg]] <- l2 %>% filter(region == reg) %>% st_union()
  attr_tmp[[reg]] <-
    l2 %>% filter(region == reg) %>% .[1, c("id", "region")] %>% st_drop_geometry()
  out_sf[[reg]] <- st_as_sf(merge(geo_tmp[[reg]], attr_tmp[[reg]]))
  
}

l1 <- do.call("rbind", out_sf)
# plot(l1)
# plot(l2)

# Formatting columns
colnames(l1) <- c("id", "name_dk", "geometry")
l2$region <- NULL



# mini-map lines
#
x_min <- 12.03
x_max <- 12.63
y_min <- 56.31
y_max <- 56.68


bottom_right <- c(x_max, y_min)
bottom_left <- c(x_min, y_min)
top_left <- c(x_min, y_max)
top_right <- c(x_max, y_max)
mini_map_lines <-
  data.frame(rbind(bottom_right, bottom_left, top_left, top_right, bottom_right))
mini_map_lines$name <- row.names(mini_map_lines)
# leaflet() %>% addPolygons(data = l1) %>% addPolylines(data = mini_map_lines, lng = ~ X1, lat = ~ X2)

# Convert to sp obj for performance with leaflet
l1 <- as(l1, "Spatial")
l2 <- as(l2, "Spatial")

l1 <- rmapshaper::ms_simplify(l1)
l2 <- rmapshaper::ms_simplify(l2)



dk_sp_data <- list(l1 = l1,
                   l2 = l2,
                   mini_map_lines = mini_map_lines)



saveRDS(dk_sp_data, file = "data/dk_sp_data.rds")


# CHD ---------------------------------------------------------------------
chd_agg_levels <- c("age", "sex", "totals")

f.load <-
  list.files(path = "data/chd/",
             pattern = "export_chd_*",
             full.names = TRUE)
chd <- sapply(f.load, fread)

names(chd) <- chd_agg_levels
chd$age <- dcast(
  chd$age,
  formula = sex + age_adult + ht.code+n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
)

# For some reason, pltly requires year to be in the correct plotting order in
# the input data. And for some reason the year is out of order in data
setorder(chd$age, ht.code, sex, age_adult, year)


chd$sex <- dcast(
  chd$sex,
  formula = sex  + ht.code++n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
)


chd$totals <- dcast(
  chd$totals,
  formula = ht.code++n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
)


# Remove extra__ in colnames
lapply(chd, function(l1){
  setnames(l1, gsub("__", "_", colnames(l1)))
})

# Round rates
chd <- lapply(chd, function(l1){
  l1[, rate_strat_incidence := round(rate_strat_incidence, digits = 1)]
  l1[, rate_strat_prevalence := round(rate_strat_prevalence, digits = 1)]
  l1
})



# NOTE this has different structure than data list for main HT.
# This list is agrr_level -> outcome, while HT is outcome -> aggr_level
shiny_dat_chd <- lapply(chd, split, by = "ht.code", keep.by = TRUE)
saveRDS(shiny_dat_chd, file ="data/chd/shiny_dat_chd.rds")
saveRDS(chd_agg_levels, file  = "data/chd/aggr_levels_chd.rds")
