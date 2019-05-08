library(data.table)
library(magrittr)
library(sp)
library(rgeos)
library(maptools)
library(sf)
library(leaflet)
library(dplyr)
# library(spdep)
edu <- fread("data/edu_description.csv")

# OUTCOME DATA ------------------------------------------------------------

export_opr <- fread("data/export_opr.txt", encoding = "UTF-8")
export_med <- fread("data/export_med.txt", encoding = "UTF-8")
export_diag <- fread("data/export_diag1.txt", encoding = "UTF-8")
export_diag2 <- fread("data/export_diag2.txt", encoding = "UTF-8")
export_diag <- rbind(export_diag, export_diag2)

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
    out$edu[, grouping := factor(grouping,
                                 levels = c(
                                   "Basic",
                                   "Secondary",
                                   "Tertiary",
                                   "Postgraduate",
                                   "Unknown"
                                 ))]
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
shiny_dat_en$b1$age


# DANISH LANGUAGE SUPPORT -------------------------------------------------

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





shiny_dat_dk <- makeDanish(shiny_dat_en)
saveRDS(shiny_dat_dk, file = "data/shiny_dat_dk.rds")
# SF APPROACH -------------------------------------------------------------
l1 <- readRDS("data/DNK_adm1.rds")
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
b.geo <- b.geo + c(-3.2, 1.88) # Move object
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
x_min <- 11.40
x_max <- 12.0
y_min <- 56.82
y_max <- 57.2


bottom_right <- c(x_max, y_min)
bottom_left <- c(x_min, y_min)
top_left <- c(x_min, y_max)
mini_map_lines <-
  data.frame(rbind(bottom_right, bottom_left, top_left))
mini_map_lines$name <- row.names(mini_map_lines)

# Convert to sp obj for performance with leaflet
l1 <- as(l1, "Spatial")
l2 <- as(l2, "Spatial")

l1 <- rmapshaper::ms_simplify(l1)
l2 <- rmapshaper::ms_simplify(l2)

# leaflet() %>% addPolygons(data = l1)

dk_sp_data <- list(l1 = l1,
                   l2 = l2,
                   mini_map_lines = mini_map_lines)



saveRDS(dk_sp_data, file = "data/dk_sp_data.rds")
