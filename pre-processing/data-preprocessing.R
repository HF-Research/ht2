library(data.table)
library(magrittr)
library(sp)
library(sf)
library(dplyr)
library(fst)
library(rsconnect)
library(flextable)
library(ggthemes)
library(leaflet)
library(manipulateWidget)
library(officer)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
#devtools::install_github('HF-Research/HTData', force = TRUE)
library(HTData)
source("r/helperFunctions.R")
files <- list.files(path = "r/", full.names = TRUE)
nix <- sapply(files, source)


# INTRO -------------------------------------------------------------------

# There is a large amount of processing that can be done before the app is
# deployed. This includes data munging on the health data and geographic data,
# but also getting the UI ready for both Danish and English. Some of the
# pre-processing is done to improve performance of app launch or of app usage
# (i.e. using nested lists instead of a large data.table to improve performance
# of data subsettin when drilling down).


# DATA TO FST -------------------------------------------------------------
# Data that needs to be loaded for each session should be in fst format for performance reasons
outcome_descriptions <- fread(file = "input_ui_text/outcome_descriptions.csv",
                              encoding = "UTF-8",
                              header = TRUE)
# load(file = "data/variable_ui.Rdata")
variable_ui <- fread(file = "input_ui_text/variable_ui.csv",
                     encoding = "UTF-8",
                     header = TRUE)
edu <-
  fread(file = "input_ui_text/edu_description.csv", encoding = "UTF-8")
ui_about_text <-
  fread(file = "input_ui_text/ui_about_text.csv", encoding = "UTF-8")

# Encode to native
# cols_to_convert <- colnames(outcome_descriptions)
# cols_to_convert <- cols_to_convert[1:12]
# outcome_descriptions <-
#   outcome_descriptions[, lapply(.SD, enc2native), .SDcols = cols_to_convert]
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2utf8)]


write_fst(outcome_descriptions, path = "data/outcome_descriptions.fst", compress = 1)
write_fst(variable_ui, path = "data/variable_ui.fst", compress = 1)
write_fst(edu, path = "data/edu_description.fst", compress = 1)
write_fst(ui_about_text, path = "data/ui_about_text.fst", compress = 1)

# OUTCOME DATA ------------------------------------------------------------
# The data that we ingest needs to be transformed to allow better performance
# in a Shiny app. Some additional housekeeping changes (re-naming variables,
# etc) are done here, because the Shiny app and the code that produces the
# data have diverged during development.
format_data <- function(x, variable_translation) {
  setnames(
    x,
    old = c(
      "ht.code",
      "aggr_level",
      "aggr_level_val",
      "rate",
      "n.events"
      
    ),
    new = c("outcome",
            "agCVD",
            "grouping",
            "rate",
            "count")
  )
  x[, event := gsub("\\.", "_", event)] %>% unique()
  
  # Capitalize first word in aggr_grouping names
  x[, grouping := Hmisc::capitalize(grouping)]
  
  out <-
    merge(
      x,
      variable_translation,
      by.x = "event",
      by.y = "code_name",
      all.x = TRUE
    )
  stopifnot(NROW(out[is.na(shiny_code)]) == 0)
  setnames(out, old = "shiny_code", new = "variable_code")
  
  # Set NAs to 0s
  out[is.na(count), count := 0]
  out[is.na(rate), rate := 0]
  
  setkey(out, year, outcome)
  out
  
}


format_geo_data <- function(x, geo_data, geo_type) {
  # Replaces kommune and region numeric codes with their names (as characters of
  # course)
  
  cols_start <- names(x)
  
  geo_cols <- c(paste0(geo_type, "_name"), geo_type)
  
  # Make sure geo name starts with capital letter
  
  
  # Grouping == 9 are "unknowns", we want to remove those
  x <- x[!(agCVD == geo_type & grouping == 9)]
  x <- x[!(agCVD == geo_type & grouping == "Unknown")]
  out <-
    merge(x[agCVD == geo_type],
          geo_data[, ..geo_cols],
          by.x = "grouping",
          by.y = geo_type,
          all.x = TRUE)
  
  # Check that everything matches
  stopifnot(NROW(out[is.na(get(geo_cols[1]))]) == 0)
  
  # Ensure row order is same in both x and out (and verify this is true)
  setkey(out, sex, year, agCVD, grouping, outcome)
  setkey(x, sex, year, agCVD, grouping, outcome)
  x[agCVD == geo_type, idx := paste0(sex, year, outcome, grouping)]
  out[, idx := paste0(sex, year, outcome, grouping)]
  stopifnot(all(x[agCVD == geo_type]$idx == out$idx))
  
  # Replace number ID with region name
  x[agCVD == geo_type, grouping := out[[geo_cols[1]]]]
  x[, idx := NULL]
  cols_end <- names(x)
  
  # Remove Christiansoe
  if (geo_type == "kom") {
    x <- x[grouping != "Christiansø"]
  }
  
  
  stopifnot(all(cols_start == cols_end))
  
  # Ensure all matching occured
  stopifnot(NROW(x[agCVD == geo_type & is.na(grouping)]) == 0)
  
  return(x)
  
}
geo_codes <-
  fread("data/kommune_codes.csv", encoding = "UTF-8") %>%
  setnames(c("kom", "kom_name", "region_name", "region")) %>%
  .[, kom := as.character(kom)] %>%
  .[, region := as.character(region)] %>%
  .[, region_name := Hmisc::capitalize(region_name)] %>%
  .[, kom_name := Hmisc::capitalize(kom_name)]
region_codes <-
  geo_codes[, .(region_name, region)] %>%
  unique(by = "region")
export_diag <-
  HTData::export_diag %>% copy() %>%
  format_data(variable_translation = variable_ui[, .(shiny_code, code_name)]) %>%
  format_geo_data(geo_data = region_codes, geo_type = "region") %>%
  format_geo_data(geo_data = geo_codes, geo_type = "kom")

export_opr <- HTData::export_opr %>% copy() %>%
  format_data(variable_translation = variable_ui[, .(shiny_code, code_name)]) %>%
  format_geo_data(geo_data = region_codes, geo_type = "region") %>%
  format_geo_data(geo_data = geo_codes, geo_type = "kom")
export_med <- HTData::export_med %>% copy() %>%
  format_data(variable_translation = variable_ui[, .(shiny_code, code_name)]) %>%
  format_geo_data(geo_data = region_codes, geo_type = "region") %>%
  format_geo_data(geo_data = geo_codes, geo_type = "kom")

edu_levels <- tolower(edu$edu_name_en)

toList <- function(x) {
  # Split into lists
  l1 <- split(x, by = "outcome") %>%
    lapply(., function(l2) {
      split(l2, by = "agCVD") %>%
        lapply(., function(l3) {
          split(l3, by = "variable_code") %>%
            # !!!!! DO NOT CHANGE !!! unless you have checked with it does not
            # interfere with cbind operation in dtCast()
            lapply(., setkeyv, cols = c("sex", "grouping", "year"))
        })
    })
  
  # Change edu to factor to ensure correct ordering in output graphs
  l1 <- lapply(l1, function(l2) {
    l2$edu <- lapply(l2$edu, function(dat) {
      dat[, grouping := factor(tolower(grouping),
                               levels = edu_levels,
                               labels = edu$edu_name_en)]
    })
    l2$ethnicity <- lapply(l2$ethnicity, function(dat) {
      dat[, grouping := capWord(grouping)]
    })
    l2
  })
}


dat_opr <- toList(export_opr)
dat_med <-  toList(export_med)
dat_diag <-  toList(export_diag)
shiny_dat_en <- c(dat_opr, dat_med, dat_diag)




saveRDS(shiny_dat_en, file = "data/shiny_dat_en.rds")

# VALID OUTPUT COMBINDATIONS ----------------------------------------------

# Some combinations of inputs are not allowed because there are too few data for
# those combinations.
valid_output_combos <- fread("data/valid_output_combos_new.txt")
valid_output_combos[, id := NULL]
valid_output_combos[, var := gsub("\\.", "_", var)]
valid_output_combos <-
  merge(
    valid_output_combos,
    variable_ui[, shiny_code, code_name],
    by.x = "var",
    by.y = "code_name",
    all.x = TRUE
  )
setnames(valid_output_combos, "aggr_level", "agCVD")
saveRDS(valid_output_combos, file = "data/valid_output_combos_new.rds")



# DANISH LANGUAGE SUPPORT -------------------------------------------------
ethnicity <-
  fread(file = "input_ui_text/ethnicity.csv", encoding = "UTF-8")
ethnicity[, `:=` (group_dk = capWord(group_dk),
                  group_en = capWord(group_en))]
ethnicity <- ethnicity[, lapply(.SD, enc2native)]
ethnicity_short <-
  unique(ethnicity, by = "group_en")[, .(group_dk, group_en)]


makeDKEdu <- function(dat) {
  dat <- lapply(dat, function(x) {
    x <- merge(x,
               edu[, .(edu_name_dk, edu_name_en)],
               by.x = "grouping",
               by.y = "edu_name_en",
               all.x = TRUE)
    x[, `:=` (grouping = edu_name_dk)]
    x[, `:=` (edu_name_dk = NULL)]
    
    # Turn DK edu into factor
    x[, `:=` (grouping = factor(grouping,
                                levels = c(edu[, edu_name_dk])))]
    setkey(x, sex, grouping, year)
    x
  })
  dat
}


makeDKEthnicity <- function(dat) {
  dat <- lapply(dat, function(x) {
    x <-
      merge(
        x,
        ethnicity_short,
        by.x = "grouping",
        by.y = "group_en",
        all.x = TRUE
      )
    x[, `:=`(grouping = group_dk)]
    setkey(x, sex, grouping, year)
    x[, grouping := capWord(grouping)]
    setkey(x, sex, grouping, year)
    x
  })
  dat
}

shiny_dat_dk <- shiny_dat_en %>% lapply(., function(l2) {
  l2$edu <- makeDKEdu(l2$edu)
  l2$ethnicity <- makeDKEthnicity(l2$ethnicity)
  l2
})

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
outcome_descriptions_dk[grepl("^d", hjertetal_code), desc := paste0(desc, " ICD koder: ", code_simple, ".")]
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
outcome_descriptions_en[grepl("^d", hjertetal_code), desc := paste0(desc, " ICD codes: ", code_simple, ".")]
saveRDS(outcome_descriptions_en, file = "language/outcome_descriptions_en.rds")

stopifnot(length(colnames(outcome_descriptions_dk)) == length(colnames(outcome_descriptions_en)))


# UI ABOUT LANG PREP ------------------------------------------------------
about_ui <-
  fread(file = "input_ui_text/ui_about_text.csv", encoding = "UTF-8")
about_ui <- about_ui[, lapply(.SD, enc2native)]
countries <-
  fread(file = "input_ui_text/ethnicity.csv", encoding = "UTF-8")
updates <-
  fread(file = 'input_ui_text/update_descriptions.csv',
        encoding = 'UTF-8',
        colClasses = 'character')
updates <- updates[, lapply(.SD, enc2native)]

# Danish
lang <- "dk"
col_names <- colnames(about_ui)
cols_delete <- grep("_en", col_names, value = TRUE)
about_ui_tmp <- about_ui[, -..cols_delete]
new_col_names <- c("code",
                   "title_text",
                   "desc_text",
                   "desc_text_2")
setnames(about_ui_tmp, new_col_names)
saveRDS(about_ui_tmp, file = "language/about_ui_dk.rds")

col_names <- colnames(countries)
cols_delete <- grep("_en", col_names, value = TRUE)
tmp <- countries[, -..cols_delete]
new_col_names <- c("code",
                   "country",
                   "group")
setnames(tmp, new_col_names)
setkey(tmp, country)
tmp[, group := capWord(group)]

saveRDS(tmp, file = "language/country_grps_dk.rds")

# Updates
tmp <- updates[order(display_order)]
tmp[, date := lubridate::ymd(date)]
tmp[, date := format(date, format = '%d-%b-%Y')]

col_names <- colnames(tmp)
cols_delete <-
  c(grep("_en", col_names, value = TRUE), 'display_order')
tmp <- tmp[, -..cols_delete]
new_col_names <- c("date",
                   "desc")
setnames(tmp, new_col_names)
saveRDS(tmp, file = "language/updates_dk.rds")



# English
lang <- "en"
col_names <- colnames(about_ui)
cols_delete <- grep("_dk", col_names, value = TRUE)
about_ui_tmp <- about_ui[, -..cols_delete]
new_col_names <- c("code",
                   "title_text",
                   "desc_text",
                   "desc_text_2")
setnames(about_ui_tmp, new_col_names)
saveRDS(about_ui_tmp, file = "language/about_ui_en.rds")

col_names <- colnames(countries)
cols_delete <- grep("_dk", col_names, value = TRUE)
tmp <- countries[, -..cols_delete]
new_col_names <- c("code",
                   "country",
                   "group")
setnames(tmp, new_col_names)
setkey(tmp, country)
tmp[, group := capWord(group)]
saveRDS(tmp, file = "language/country_grps_en.rds")

# Updates
tmp <- updates[order(display_order)]
tmp[, date := lubridate::ymd(date)]
tmp[, date := format(date, format = '%d-%b-%Y')]

col_names <- colnames(tmp)
cols_delete <-
  c(grep("_dk", col_names, value = TRUE), 'display_order')
tmp <- tmp[, -..cols_delete]
new_col_names <- c("date","desc")
setnames(tmp, new_col_names)
saveRDS(tmp, file = "language/updates_en.rds")




# CHD ---------------------------------------------------------------------

# This has to be in the same order the files are loaded in (f.load object)
chd_agg_levels <- c("age", "age_sex", "sex", "totals")
chd = list(HTData::export_chd_age,
           export_chd_age_sex,
           export_chd_sex,
           export_chd_totals)
names(chd) <- chd_agg_levels



# TMP REMOVE 2018 ---
# Until we have the question of operations sorted out
# chd <- lapply(chd, function(i) {
#   i <- i[year != 2018]
#   i
# })


# Rename "opr" to "oprs" to make pattern matching easier
lapply(chd, function(i) {
  i[variable == "opr", variable := "oprs"]
})

chd$age_sex <- dcast(
  chd$age_sex,
  formula = sex + age_adult + ht.code +  year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>%
  . [, id_var := paste0(sex, age_adult)] %>%
  # For some reason, pltly requires year to be in the correct plotting order in
  # the input data. And for some reason the year is out of order in data
  setorder(ht.code, sex, age_adult, year)



chd$age <- chd$age %>%
  dcast(
    formula = age_adult + ht.code + year ~ variable,
    value.var = c("count_n_", "rate_strat")
  ) %>%
  setorder(ht.code, age_adult, year)


chd$sex <- dcast(
  chd$sex,
  formula = sex  + ht.code + year ~ variable,
  value.var = c("count_n_", "rate_strat")
) %>%
  setorder(ht.code, sex, year)


chd$totals <- dcast(
  chd$totals,
  formula = ht.code + year ~ variable,
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
  l1[, rate_strat_oprs := round(rate_strat_oprs, digits = 1)]
  
  l1
})




# NOTE this has different structure than data list for main HT.
# This list is agrr_level -> outcome, while HT is outcome -> agCVD
shiny_dat_chd <- lapply(chd, split, by = "ht.code", keep.by = TRUE)

# Set key for faster subsetting during live shiny use
shiny_dat_chd$age_sex <-
  lapply(shiny_dat_chd$age_sex, setkey, "id_var")


saveRDS(shiny_dat_chd, file = "data/chd/shiny_dat_chd.rds")
saveRDS(chd_agg_levels, file  = "data/chd/aggr_levels_chd.rds")



# CHD LANG PREP -----------------------------------------------------------

outcome_desc_chd <-
  fread(file = "input_ui_text/outcome_descriptions_chd.csv", encoding = "UTF-8")
var_desc_chd <-
  fread(file = "input_ui_text/variable_ui_chd.csv", encoding = "UTF-8")
var_desc_chd <- var_desc_chd[, lapply(.SD, enc2native)]



# Danish
lang <- "dk"
col_names <- colnames(outcome_desc_chd)
cols_delete <- grep("_en", col_names, value = TRUE)
outcome_descriptions_dk <- outcome_desc_chd[, -..cols_delete]
new_col_names <- c("ht.code",
                   "name",
                   "icd8",
                   "icd10",
                   "diag_type",
                   "pat_type",
                   "grade",
                   "desc",
                   "link")
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


about_chd <-
  fread("input_ui_text/ui_about_text_chd.csv", encoding = "UTF-8")
col_names <- colnames(about_chd)
cols_delete <- grep("_en", col_names, value = TRUE)
about_chd_dk <- about_chd[, -..cols_delete]
about_chd_new_names <- c("code",
                         "title_text",
                         "desc_text",
                         "desc_text_2")
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
about_chd_new_names <- c("code",
                         "title_text",
                         "desc_text",
                         "desc_text_2")
setnames(about_chd_en, about_chd_new_names)

saveRDS(outcome_descriptions_en, file = "language/outcome_descriptions_chd_en.rds")
saveRDS(var_desc_chd_en, file = "language/variable_descriptions_chd_en.rds")
saveRDS(about_chd_en, file = "language/ui_about_text_chd_en.rds")

# GEO DATA ------------------------------------------------------------------

# Here we perpare the base map and ensure that names are aligned to allow
# merging with the health data in the Shiny app.

# Additionally The island of Bornholm makes a map of Denmark awkward. To get
# around this we mimic on inset map by moving the coordinates of Bornholm, then
# putting inset lines around it
l2 <- readRDS("data/DNK_adm2.rds")

l2 <- st_as_sf(l2, coords = c("x", "y")) %>%
  st_transform(crs = st_crs(4326))
st_crs(l2) <- 4326
l2 <-
  l2 %>%
  dplyr::select(OBJECTID, NAME_1, NAME_2) %>%
  rename(id = OBJECTID,
         name_kom = NAME_2,
         region = NAME_1)


l2$name_kom <- enc2native(l2$name_kom)
l2$region <- enc2native(l2$region)
l2[l2$name_kom == "Århus", ]$name_kom <- "Aarhus"
l2[l2$name_kom == "Vesthimmerland", ]$name_kom <- "Vesthimmerlands"



# Delete Christiansoe polygon
l2 <- l2[l2$name_kom != "Christiansø", ]

# Move Bornholm
bornholm <- l2 %>% filter(name_kom == "Bornholm")
b.geo <- st_geometry(bornholm) # Subset geometry of of object
b.geo <- b.geo + c(-2.6, 1.35) # Move object
st_geometry(bornholm) <- b.geo # Re-assign geometry to object

# Replace bornholm in main sf object
l2[l2$name_kom == "Bornholm", ] <- bornholm

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
colnames(l1) <- c("id", "name_kom", "geometry")
l2$region <- NULL



# Make map inset
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


# Convert to sp obj for performance with leaflet
l1_sf <- rmapshaper::ms_simplify(l1)
l2_sf <- rmapshaper::ms_simplify(l2)


dk_sf_data <- list(
  l1 = l1_sf %>% as.data.table(),
  l2 = l2_sf %>% as.data.table(),
  mini_map_lines = mini_map_lines
)


saveRDS(dk_sf_data, file = "data/dk_sf_data.rds")



# STARTING CONDITIONS -----------------------------------------------------
# valid_output_combos <-
#   readRDS(file = "data/valid_output_combos.rds")
# variables_not_used <-
#   variable_ui[grepl("n_ambulatory|n_bed", code_name), shiny_code]
# 
# starting_vals_dk <- validate_selected_vars(
#   aggr_selected = "national",
#   outcome_code = "d1",
#   variables_not_used = variables_not_used,lang = "dk",selected_var = "v1"
# )
# 
# starting_vals_en <- validate_selected_vars(
#   aggr_selected = "national",
#   outcome_code = "d1",
#   variables_not_used = variables_not_used,lang = "en",selected_var = "v1"
# )
# 
# saveRDS(starting_vals_dk, file = "data/starting_vals_dk")
# saveRDS(starting_vals_en, file = "data/starting_vals_en")
# CSS PREPERATION ---------------------------------------------------------

# In order to have a common CSS template for all  Shiny apps in the Danish Heart
# Foundation, we create use a common css template with base styling. Any styling
# specific to an app goes in the app_specific css file.
css_common <-
  readLines(con = "https://raw.githubusercontent.com/HF-Research/ht2/refs/heads/master/www/css-app-specifc.css")
css_app_specific <-
  readLines(con = "https://raw.githubusercontent.com/HF-Research/ht2/refs/heads/master/www/css-app-specifc.css")

writeLines(text = c(css_common, css_app_specific),
           con = "www/css-app-specifc.css")

