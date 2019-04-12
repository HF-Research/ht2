library(data.table)
library(magrittr)
library(sp)
library(rgeos)
library(maptools)
library(sf)
library(leaflet)
library(dplyr)
# library(spdep)
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
    lapply(outcome, function(aggr_level) {
      aggr_level[, `:=` (outcome = NULL, aggr_level = NULL)]
      
      # !!!!! DO NOT CHANGE !!! unless you have checked with it does not
      # interfere with cbind operation in dtCast()
      setkey(aggr_level, sex, grouping, year)
      
    })
  })
}

dat_opr <- preProccess(export_opr)
dat_med <-  preProccess(export_med)
dat_diag <-  preProccess(export_diag)

shiny_dat <- c(dat_opr, dat_med, dat_diag)
# saveRDS(shiny_list, file = "data/shiny_list.rds")
saveRDS(shiny_dat, file = "data/shiny_dat.rds")
# SPATIAL DATA PRE PROCESSING --------------------------------------

# Script only run when updating the polygons for Danish kommunes, regions, etc.
# Outputs a geoJSON for use by leaflet app.
#
# Data not allowed for commercial use - but academic publishing allowed:
# http://www.gadm.org/download


# Data is in WGS84 and Lat/Lon. Leaflet uses this ()
# l0 <- readRDS("data/DNK_adm0.rds")
l1 <- readRDS("data/DNK_adm1.rds")
l2 <- readRDS("data/DNK_adm2.rds")

# Check projection
proj4string(l1) == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Remove un-needed attributes
# # NOTE need to keep id variable for merges
l1@data <-
  l1@data %>%
  dplyr::select(OBJECTID, NAME_1, VARNAME_1) %>%
  dplyr::rename(old_id = OBJECTID,
                name_dk = NAME_1,
                name_en = VARNAME_1)
l2@data <-
  l2@data %>%
  dplyr::select(OBJECTID, NAME_2, NAME_1) %>%
  dplyr::rename(old_id = OBJECTID,
                name_dk = NAME_2)


# Rename problematic names in spatial file. The dst version contains the correct
# names
setDT(l2@data)
l2@data[name_dk == "Århus", name_dk := "Aarhus"]
l2@data[name_dk == "Vesthimmerland", name_dk := "Vesthimmerlands"]
l2@data$name_en <- l2@data$name_dk


setDT(l1@data)
l1@data[name_dk == "Midtjylland", name_dk := "Midtjydlland"]


# Simplify poloygons because load time is too high with original resolution.
# gSimplify remove @data, so will need to re-add that from original. See:
# https://goo.gl/RXBpZn
l1_data <- l1@data
lx <- rgeos::gSimplify(l1, .001, topologyPreserve = TRUE)
l1 <- SpatialPolygonsDataFrame(lx, data = l1@data)
l1 <- l1[order(l1$name_dk),] # Re-order based on order found in shiny data

l2_data <- l2@data
lx <- rgeos::gSimplify(l2, .001, topologyPreserve = TRUE)
l2 <- SpatialPolygonsDataFrame(lx, data = l2@data)

# Move Bornholm to better part of mapÆ
bornholm <- l2[4, ]
x <- (elide(bornholm, shift = c(-3.2, 1.88)))
l2@polygons[4] <- x@polygons[1]
plot(l2)

elide(l1[1, ], shift = c(-3.2, 1.88))


# Add vertices for mini-map graphical lines
range(l2@polygons[4][[1]]@Polygons[[1]]@coords[,1])
range(l2@polygons[4][[1]]@Polygons[[1]]@coords[,2])

x_min <- 11.40
x_max <- 12.0
y_min <- 56.82
y_max <- 57.2


bottom_right <- c(x_max, y_min)
bottom_left <- c(x_min, y_min)
top_left <- c(x_min, y_max)
mini_map_lines <- data.frame(rbind(bottom_right, bottom_left, top_left)) 
mini_map_lines$name <- row.names(mini_map_lines)
leaflet::leaflet() %>% addTiles() %>%  addPolygons(data = x) %>%
  addPolylines(data = mini_map_lines, lng = ~X1, lat = ~X2,color = "grey", weight = 6)


# SF APPROACH -------------------------------------------------------------
x <- st_as_sf(l2, coords = c("x", "y"))
x <- x %>% dplyr::select(OBJECTID, NAME_1, NAME_2) %>% rename(id = OBJECTID,
                                                              name_dk = NAME_2,
                                                              region = NAME_1)
z <- x %>% filter(name_dk == "Bornholm")

st_combine(x)

z + c(-3.2, 1.8)

x_simple <- st_cast(x, "POLYGON")
plot(x_simple)



xgeo <- st_geometry(x)
plot(xgeo)
x2 <- (xgeo  + c(-3.2, 1.88))



# Check names
kom_names_dst <- unique(shiny_dat$d1$kom$grouping)
kom_names_geo <- unique(l2@data$name_dk)

kom_names_geo[!kom_names_geo %in% kom_names_dst]
kom_names_dst[!kom_names_dst %in% kom_names_geo]

reg_names_dst <- unique(shiny_dat$d1$region$grouping)
reg_names_geo <- unique(l1@data$name_dk)

reg_names_geo[!reg_names_geo %in% reg_names_dst]
reg_names_dst[!reg_names_dst %in% reg_names_geo]


dk_sp_data <- list(l1 = l1,
                   l2 = l2,
                   mini_map_lines = mini_map_lines)

saveRDS(dk_sp_data, file = "data/dk_sp_data.rds")


# STANDARD POPULATION WEIGHTS ---------------------------------------------


# Prep european standard population
esp <- fread("data/european_standard_population.txt")
pop_summary_age <- fread(file = "data/pop_summary_age.txt")

labs <-
  c("35 - 44", "45 - 54", "55 - 64", "65 - 74", "75 - 84", "85+")
labs_age_5yr <-
  c(
    "35 - 39",
    "40 - 44",
    "45 - 49",
    "50 - 54",
    "55 - 59",
    "60 - 64",
    "65 - 69",
    "70 - 74",
    "75 - 79",
    "80 - 84",
    "85+"
  )

# Group into poplulation groups needed for our analysis
std_pop <- esp[,
               sum(esp2013),
               by = .(sex,
                      cut(
                        esp$age_group,
                        breaks = c(34, 44, 54, 64, 74, 84, 105),
                        include.lowest = TRUE,
                        labels = labs
                      ))]
colnames(std_pop) <- c("sex", "age_group", "std_pop")


# For 5yr pop, aggregate those above 85 into one age group
std_pop5yr <- esp[age_group >= 35 , ] %>%
  .[age_group >= 85, esp2013 := sum(esp2013), by = sex] %>%
  .[age_group <= 85, ] %>%
  .[, age := labs_age_5yr] %>%
  .[, age_group := NULL] %>%
  set_colnames(c("sex", "std_pop", "age_group"))
setcolorder(std_pop5yr, c("sex", "age_group", "std_pop"))


# Find weights
std_pop <- std_pop[!is.na(age_group)]
std_pop[, tot_pop_sex := sum(std_pop), by = .(sex)]
std_pop[, weight := std_pop / tot_pop_sex]
std_pop[, tot_pop_sex := NULL]
std_pop[, std_pop := NULL]

std_pop5yr <- std_pop5yr[!is.na(age_group)]
std_pop5yr[, tot_pop_sex := sum(std_pop), by = .(sex)]
std_pop5yr[, weight := std_pop / tot_pop_sex]
std_pop5yr[, tot_pop_sex := NULL]
std_pop5yr[, std_pop := NULL]



pop_summary_age <-
  merge(
    pop_summary_age,
    std_pop,
    by.x = c("sex", "grouping"),
    by.y = c("sex", "age_group")
  )
write.csv(pop_summary_age,
          file = "data/pop_summary_weighted.csv",
          row.names = FALSE)
