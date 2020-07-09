library(data.table)
library(magrittr)
library(sp)
library(rgeos)
library(maptools)
library(sf)
library(leaflet)
library(dplyr)
library(fst)
library(Hmisc)
# devtools::install_github('HF-Research/HTData', force = TRUE)
library(HTData)
source("R/helperFunctions.R")

# DATA TO FST -------------------------------------------------------------
# Data that needs to be loaded for each session should be in fst format for performance reasons
outcome_descriptions <-
  fread(file = "data/outcome_descriptions.csv",
        encoding = "UTF-8",
        header = TRUE)
# load(file = "data/variable_ui.Rdata")
variable_ui <-
  fread(file = "data/variable_ui.csv",
        encoding = "UTF-8",
        header = TRUE)
edu <- fread(file = "data/edu_description.csv", encoding = "UTF-8")
ui_about_text <-
  fread(file = "data/ui_about_text.csv", encoding = "UTF-8")

# Encode to native
# cols_to_convert <- colnames(outcome_descriptions)
# cols_to_convert <- cols_to_convert[1:12]
# outcome_descriptions <-
#   outcome_descriptions[, lapply(.SD, enc2native), .SDcols = cols_to_convert]
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]


write_fst(outcome_descriptions, path = "data/outcome_descriptions.fst", compress = 1)
write_fst(variable_ui, path = "data/variable_ui.fst", compress = 1)
write_fst(edu, path = "data/edu_description.fst", compress = 1)
write_fst(ui_about_text, path = "data/ui_about_text.fst", compress = 1)

# OUTCOME DATA ------------------------------------------------------------
export_diag <- copy(as.data.table(HTData::export_diag))

export_opr <- copy(HTData::export_opr)
export_med <- copy(HTData::export_med)

setnames(export_diag, "aggr_level", "agCVD")
setnames(export_med, "aggr_level", "agCVD")
setnames(export_opr, "aggr_level", "agCVD")

preProccess <- function(export_dat) {
  dat <- split(export_dat, by = "outcome") %>%
    lapply(., split, by = "agCVD")

  lapply(dat, function(outcome) {
    out <- lapply(outcome, function(agCVD) {
      agCVD[, `:=` (outcome = NULL, agCVD = NULL)]

      # !!!!! DO NOT CHANGE !!! unless you have checked with it does not
      # interfere with cbind operation in dtCast()
      setkey(agCVD, sex, grouping, year)
    })

    # Make sure regions start with capital letter
    out$region[, grouping := capitalize(grouping)]
    out$region[grouping == "Midtjydlland", grouping := "Midtjylland"]

    out$ethnicity[, grouping := capWord(grouping)]

    # Change edu to factor to ensure correct ordering
    out$edu[, grouping := factor(tolower(grouping),
                                 levels = tolower(
                                   c(
                                     "Basic",
                                     "Secondary",
                                     "Tertiary",
                                     "Postgraduate",
                                     "Unknown"
                                   )
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
    outcome$region <- outcome$region[grouping != "Unknown",]
    outcome$kom <- outcome$kom[grouping != "Christiansø",]
    outcome
  })

}

setNAtoZero <- function(x) {
  lapply(x, function(outcome) {
    lapply(outcome, function(agCVD) {
      data_vars <- grep("count|rate", colnames(agCVD), value = TRUE)
      agCVD[, (data_vars) := lapply(.SD, function(i) {
        i[is.na(i)] <- 0L
        i
      }),
      .SDcols = data_vars]

    })
  })
}
eduToFactorEnglish <- function(dat) {
  # Change english education labels to Danish labels
  lapply(dat, function(outcome) {
    # Turn edu into factor
    outcome$edu[, `:=` (grouping = factor(grouping,
                                          levels = c(edu[, edu_name_en])))]
    setkey(outcome$edu, sex, grouping, year)
    outcome
  })
}


shiny_dat_en <- cleanGeoData(shiny_dat_en)
shiny_dat_en <- setNAtoZero(shiny_dat_en)
edu[, edu_name_en := tolower(edu_name_en)]
shiny_dat_en <- eduToFactorEnglish(shiny_dat_en)
saveRDS(shiny_dat_en, file = "data/shiny_dat_en.rds")

# Valid outcome combinations
valid_output_combos <- fread("data/valid_output_combos.txt")
valid_output_combos[, id := NULL]
valid_output_combos[, var := paste0("count_", var)]
valid_output_combos <-
  merge(
    valid_output_combos,
    variable_ui[, shiny_code, code_name],
    by.x = "var",
    by.y = "code_name",
    all.x = TRUE
  )
setnames(valid_output_combos, "aggr_level", "agCVD")
saveRDS(valid_output_combos, file = "data/valid_output_combos.rds")

# DANISH LANGUAGE SUPPORT -------------------------------------------------


ethnicity <- fread(file = "data/ethnicity.csv", encoding = "UTF-8")
ethnicity[, `:=` (group_dk = capWord(group_dk), group_en = capWord(group_en))]
ethnicity <- ethnicity[, lapply(.SD, enc2native)]
ethnicity_short <- unique(ethnicity, by = "group_en")[, .(group_dk, group_en)]

makeDanish <- function(dat) {
  # Change english education + ethnicity labels to Danish labels
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
    outcome$edu[, `:=` (grouping = factor(grouping,
                                          levels = c(edu[, edu_name_dk])))]
    setkey(outcome$edu, sex, grouping, year)

    outcome$ethnicity <-
      merge(
        outcome$ethnicity,
        ethnicity_short,
        by.x = "grouping",
        by.y = "group_en",
        all.x = TRUE
        )
    outcome$ethnicity[, `:=`(grouping = group_dk)]
    setkey(outcome$ethnicity, sex, grouping, year)
    outcome$ethnicity[, grouping := capWord(grouping)]

    outcome
  })
}
shiny_dat_dk <- makeDanish(shiny_dat_en)
saveRDS(shiny_dat_dk, file = "data/shiny_dat_dk.rds")



# UI MAIN LANG PREP ---------------------------------------------------
# DK
lang <- "dk"
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)

name_lang_var <- paste0("name_", lang)
keep_vars <- c("hjertetal_code", name_lang_var, "display_order")
col_names <- c("hjertetal_code", "name", "display_order")
outcome_names_treatment <-
  merge(data.table(hjertetal_code = grep("b", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_treatment) <- col_names
outcome_names_treatment[, type := "treatment"]
setorder(outcome_names_treatment, display_order)

outcome_names_med <-
  merge(data.table(hjertetal_code = grep("m", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_med) <- col_names
outcome_names_med[, type := "med"]
setorder(outcome_names_med, display_order)

outcome_names_diag <-
  merge(data.table(hjertetal_code = grep("d", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_diag) <-
  col_names
outcome_names_diag[, type := "diag"]
setorder(outcome_names_diag, display_order)


outcomes_all <-
  rbind(outcome_names_diag,
        outcome_names_treatment,
        outcome_names_med)
file_name <- paste0("data/outcomes_all_", lang, ".rds")
saveRDS(outcomes_all, file = file_name)

col_names <- colnames(outcome_descriptions)
cols_delete <- grep("_en", col_names, value = TRUE)
outcome_descriptions_dk <- outcome_descriptions[, -..cols_delete]
setnames(
  outcome_descriptions_dk,
  c(
    "hjertetal_code",
    "name",
    "name_long",
    "desc",
    "link",
    "code_simple",
    "grep_string",
    "diag_type",
    "pat_type",
    "display_order"
  )
)

saveRDS(outcome_descriptions_dk, file = "language/outcome_descriptions_dk.rds")

# EN
lang <- "en"
data_path <- file.path(paste0("data/shiny_dat_", lang, ".rds"))
shiny_dat <- readRDS(file = data_path)

name_lang_var <- paste0("name_", lang)
keep_vars <- c("hjertetal_code", name_lang_var, "display_order")
col_names <- c("hjertetal_code", "name", "display_order")
outcome_names_treatment <-
  merge(data.table(hjertetal_code = grep("b", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_treatment) <- col_names
outcome_names_treatment[, type := "treatment"]
setorder(outcome_names_treatment, display_order)

outcome_names_med <-
  merge(data.table(hjertetal_code = grep("m", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_med) <- col_names
outcome_names_med[, type := "med"]
setorder(outcome_names_med, display_order)

outcome_names_diag <-
  merge(data.table(hjertetal_code = grep("d", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, ..keep_vars]
colnames(outcome_names_diag) <-
  col_names
outcome_names_diag[, type := "diag"]
setorder(outcome_names_diag, display_order)



outcomes_all <-
  rbind(outcome_names_diag,
        outcome_names_treatment,
        outcome_names_med)
file_name <- paste0("data/outcomes_all_", lang, ".rds")
saveRDS(outcomes_all, file = file_name)

col_names <- colnames(outcome_descriptions)
cols_delete <- grep("_dk", col_names, value = TRUE)
outcome_descriptions_en <- outcome_descriptions[, -..cols_delete]
setnames(
  outcome_descriptions_en,
  c(
    "hjertetal_code",
    "name",
    "name_long",
    "desc",
    "link",
    "code_simple",
    "grep_string",
    "diag_type",
    "pat_type",
    "display_order"
  )
)
saveRDS(outcome_descriptions_en, file = "language/outcome_descriptions_en.rds")

stopifnot(length(colnames(outcome_descriptions_dk)) == length(colnames(outcome_descriptions_en)))


# UI ABOUT LANG PREP ------------------------------------------------------
about_ui <- fread(file = "data/ui_about_text.csv", encoding = "UTF-8")
about_ui <- about_ui[, lapply(.SD, enc2native)]
countries <- fread(file = "data/ethnicity.csv", encoding = "UTF-8")

# Danish
lang <- "dk"
col_names <- colnames(about_ui)
cols_delete <- grep("_en", col_names, value = TRUE)
about_ui_tmp <- about_ui[, -..cols_delete]
new_col_names <- c(
  "code",
  "title_text",
  "desc_text",
  "desc_text_2"
  )
setnames(about_ui_tmp, new_col_names)
saveRDS(about_ui_tmp, file = "language/about_ui_dk.rds")

col_names <- colnames(countries)
cols_delete <- grep("_en", col_names, value = TRUE)
tmp <- countries[, -..cols_delete]
new_col_names <- c(
  "code",
  "country",
  "group"
)
setnames(tmp, new_col_names)
setkey(tmp, country)
tmp[, group := capWord(group)]

saveRDS(tmp, file = "language/country_grps_dk.rds")




# English
lang <- "en"
col_names <- colnames(about_ui)
cols_delete <- grep("_dk", col_names, value = TRUE)
about_ui_tmp <- about_ui[, -..cols_delete]
new_col_names <- c(
  "code",
  "title_text",
  "desc_text",
  "desc_text_2"
)
setnames(about_ui_tmp, new_col_names)
saveRDS(about_ui_tmp, file = "language/about_ui_en.rds")

col_names <- colnames(countries)
cols_delete <- grep("_dk", col_names, value = TRUE)
tmp <- countries[, -..cols_delete]
new_col_names <- c(
  "code",
  "country",
  "group"
)
setnames(tmp, new_col_names)
setkey(tmp, country)
tmp[, group := capWord(group)]
saveRDS(tmp, file = "language/country_grps_en.rds")




# CHD ---------------------------------------------------------------------

# This has to be in the same order the files are loaded in (f.load object)
chd_agg_levels <- c("age", "age_sex", "sex", "totals")

f.load <-
  list.files(path = "data/chd/",
             pattern = "export_chd_*",
             full.names = TRUE)
chd <- sapply(f.load, fread)

names(chd) <- chd_agg_levels

# Rename "opr" to "oprs" to make pattern matching easier
lapply(chd, function(i){
  
  i[variable == "opr", variable := "oprs"]
})

chd$age_sex <- dcast(
  chd$age_sex,
  formula = sex + age_adult + ht.code + n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>%
  . [, id_var := paste0(sex, age_adult)] %>%
  # For some reason, pltly requires year to be in the correct plotting order in
  # the input data. And for some reason the year is out of order in data
  setorder(ht.code, sex, age_adult, year)


chd$age <- dcast(
  chd$age,
  formula = age_adult + ht.code + n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>%
  setorder(ht.code, age_adult, year)


chd$sex <- dcast(
  chd$sex,
  formula = sex  + ht.code + n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>%
setorder(ht.code, sex, year)


chd$totals <- dcast(
  chd$totals,
  formula = ht.code + n_denom + year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>% setorder(ht.code, year)

# Remove extra__ in colnames
lapply(chd, function(l1) {
  setnames(l1, gsub("__", "_", colnames(l1)))
})

# Round rates
chd <- lapply(chd, function(l1) {
  l1[, rate_strat_incidence := round(rate_strat_incidence, digits = 1)]
  l1[, rate_strat_prevalence := round(rate_strat_prevalence, digits = 1)]
  l1[, rate_strat_opr_patients := round(rate_strat_opr_patients, digits = 1)]
  l1[, rate_strat_opr := round(rate_strat_oprs, digits = 1)]
  
  l1
})




# NOTE this has different structure than data list for main HT.
# This list is agrr_level -> outcome, while HT is outcome -> agCVD
shiny_dat_chd <- lapply(chd, split, by = "ht.code", keep.by = TRUE)

# Set key for faster subsetting during live shiny use
shiny_dat_chd$age_sex <- lapply(shiny_dat_chd$age_sex, setkey, "id_var")


saveRDS(shiny_dat_chd, file = "data/chd/shiny_dat_chd.rds")
saveRDS(chd_agg_levels, file  = "data/chd/aggr_levels_chd.rds")



# CHD LANG PREP -----------------------------------------------------------

outcome_desc_chd <- fread(file = "data/chd/outcome_descriptions_chd.csv", encoding = "UTF-8")
var_desc_chd <- fread(file = "data/chd/variable_ui_chd.csv", encoding = "UTF-8")
var_desc_chd <- var_desc_chd[, lapply(.SD, enc2native)]



# Danish
lang <- "dk"
col_names <- colnames(outcome_desc_chd)
cols_delete <- grep("_en", col_names, value = TRUE)
outcome_descriptions_dk <- outcome_desc_chd[, -..cols_delete]
new_col_names <- c(
  "ht.code",
  "name",
  "icd8",
  "icd10",
  "diag_type",
  "pat_type",
  "grade",
  "desc",
  "link"
 )
setnames(outcome_descriptions_dk, new_col_names)


col_names <- colnames(var_desc_chd)
cols_delete <- grep("_en", col_names, value = TRUE)
var_desc_chd_dk <- var_desc_chd[, -..cols_delete]
var_col_names <- c(
  "code_name",
  "name",
  "name_count",
  "name_rate",
  "name_rate_standardized",
  "desc_general",
  "desc_count",
  "desc_stratified",
  "desc_standardized"

)
setnames(var_desc_chd_dk, var_col_names)


about_chd <- fread("data/chd/ui_about_text_chd.csv", encoding = "UTF-8")
col_names <- colnames(about_chd)
cols_delete <- grep("_en", col_names, value = TRUE)
about_chd_dk <- about_chd[, -..cols_delete]
about_chd_new_names <- c(
  "code",
  "title_text",
  "desc_text",
  "desc_text_2"
)
setnames(about_chd_dk, about_chd_new_names)

saveRDS(outcome_descriptions_dk, file = "language/outcome_descriptions_chd_dk.rds")
saveRDS(var_desc_chd_dk, file = "language/variable_descriptions_chd_dk.rds")
saveRDS(about_chd_dk, file = "language/ui_about_text_chd_dk.rds")


# English
lang <- "en"
col_names <- colnames(outcome_desc_chd)
cols_delete <- grep("_dk", col_names, value = TRUE)
outcome_descriptions_en <- outcome_desc_chd[, -..cols_delete]
setnames(outcome_descriptions_en, new_col_names)

col_names <- colnames(var_desc_chd)
cols_delete <- grep("_dk", col_names, value = TRUE)
var_desc_chd_en <- var_desc_chd[, -..cols_delete]
setnames(var_desc_chd_en, var_col_names)

col_names <- colnames(about_chd)
cols_delete <- grep("_dk", col_names, value = TRUE)
about_chd_en <- about_chd[, -..cols_delete]
about_chd_new_names <- c(
  "code",
  "title_text",
  "desc_text",
  "desc_text_2"
)
setnames(about_chd_en, about_chd_new_names)

saveRDS(outcome_descriptions_en, file = "language/outcome_descriptions_chd_en.rds")
saveRDS(var_desc_chd_en, file = "language/variable_descriptions_chd_en.rds")
saveRDS(about_chd_en, file = "language/ui_about_text_chd_en.rds")

# GEO DATA ------------------------------------------------------------------
l2 <- readRDS("data/DNK_adm2.rds")

l2 <- st_as_sf(l2, coords = c("x", "y"))
l2 <-
  l2 %>%
  dplyr::select(OBJECTID, NAME_1, NAME_2) %>%
  rename(id = OBJECTID,
         name_kom = NAME_2,
         region = NAME_1)


l2$name_kom <- enc2native(l2$name_kom)
l2$region <- enc2native(l2$region)
l2[l2$name_kom == "Århus",]$name_kom <- "Aarhus"
l2[l2$name_kom == "Vesthimmerland",]$name_kom <- "Vesthimmerlands"


l2$name_kom
# Delete Christiansoe polygon
l2 <- l2[l2$name_kom != "Christiansø",]
l2$name_kom

# Move Bornholm
bornholm <- l2 %>% filter(name_kom == "Bornholm")
b.geo <- st_geometry(bornholm) # Subset geometry of of object
b.geo <- b.geo + c(-2.6, 1.35) # Move object
st_geometry(bornholm) <- b.geo # Re-assign geometry to object

# Replace bornholm in main sf object
l2[l2$name_kom == "Bornholm",] <- bornholm

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
colnames(l1) <- c("id", "name_kom", "geometry")
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
l1_tmp <- as(l1, "Spatial")
l2_tmp <- as(l2, "Spatial")

l1_tmp <- rmapshaper::ms_simplify(l1_tmp)
l2_tmp <- rmapshaper::ms_simplify(l2_tmp)

l1_tmp$name_kom <- l1$name_kom
l2_tmp$name_kom <- l2$name_kom

dk_sp_data <- list(l1 = l1_tmp,
                   l2 = l2_tmp,
                   mini_map_lines = mini_map_lines)



saveRDS(dk_sp_data, file = "data/dk_sp_data.rds")