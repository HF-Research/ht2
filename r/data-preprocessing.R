library(data.table)
library(magrittr)
library(sp)
library(rgeos)


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
      setkey(aggr_level, year)
      aggr_level
    })
  })
}
dat_opr <- preProccess(export_opr)
dat_med <-  preProccess(export_med)
dat_diag <-  preProccess(export_diag)


## Add lists together (med, diag, opr) and save output as one massive list
# shiny_list <-
#   list(opr_dat = dat_opr,
#        med_dat = dat_med,
#        diag_dat = dat_diag)
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
  dplyr::rename(id = OBJECTID,
                name_dk = NAME_1,
                name_en = VARNAME_1)
l2@data <-
  l2@data %>%
  dplyr::select(OBJECTID, NAME_2) %>%
  dplyr::rename(id = OBJECTID,
                name_dk = NAME_2)


# Rename problematic names in spatial file. The dst version contains the correct
# names
setDT(l2@data)
l2@data[name_dk == "Århus", name_dk := "Aarhus"]
l2@data[name_dk == "Vesthimmerland", name_dk := "Vesthimmerlands"]
l2@data$name_en <- l2@data$name_dk
setkey(l2@data, name_dk)

setDT(l1@data)
l1@data[name_dk == "Midtjylland", name_dk := "Midtjydlland"]
setkey(l1@data, name_dk)
# Any duplicated values within the same outcome/aggr_level have to be jittered so that


# Simplify poloygons because load time is too high with original resolution.
# gSimplify remove @data, so will need to re-add that from original. See:
# https://goo.gl/RXBpZn
l1_data <- l1@data
lx <- rgeos::gSimplify(l1, .001, topologyPreserve = TRUE)
l1 <- SpatialPolygonsDataFrame(lx, data = l1@data)

l2_data <- l2@data
lx <- rgeos::gSimplify(l2, .001, topologyPreserve = TRUE)
l2 <- SpatialPolygonsDataFrame(lx, data = l2@data)


# Move Bornholm to better part of mapÆ




dk_sp_data <- list(l1 = l1,
                   l2 = l2)

# Check names
kom_names_dst <- unique(shiny_dat$d1$kom$grouping)
kom_names_geo <- unique(l2@data$name_dk)

kom_names_geo[!kom_names_geo %in% kom_names_dst]
kom_names_dst[!kom_names_dst %in% kom_names_geo]

reg_names_dst <- unique(shiny_dat$d1$region$grouping)
reg_names_geo <- unique(l1@data$name_dk)

reg_names_geo[!reg_names_geo %in% reg_names_dst]
reg_names_dst[!reg_names_dst %in% reg_names_geo]



saveRDS(dk_sp_data, file = "data/dk_sp_data.rds")
