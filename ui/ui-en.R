# INTRO -------------------------------------------------------------------
###########################
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
###########################

# Use hjertetal_code to merge names and descriptions of outcomes. This will be
# in seperate script run once - not on every launch.
outcome_descriptions <-
  fread(file = "data/outcome_descriptions.csv", encoding = "UTF-8", header = TRUE)
# load(file = "data/variable_ui.Rdata")
variable_ui <-
  read_fst(path = "data/variable_ui.fst", as.data.table = TRUE
  )
edu <- read_fst(path = "data/edu_description.fst", as.data.table = TRUE)



# Encode to native
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]

file_name <- paste0("data/outcomes_all_", lang, ".rds")
outcomes_all <- readRDS(file_name)




# ALL PANELS --------------------------------------------------------------
ui_sex <- enc2utf8("Sex")
ui_year <- enc2utf8("Year")
ui_sex_levels <- enc2utf8(c("Women", "Men"))
choose_year <- enc2utf8("Choose year:")
ui_download <- "Download"
ui_download_graph <- "Download figure"
ui_the <- ""

# MAIN PANEL -----------------------------------------------
# Everything below here will need to be changed for the english verison

# Outcome dropdown, broken up into sections
outcome_choices <- c(list(
  "Illness" = enc2utf8(outcomes_all[type == "diag"]$name),
  "Procedure" = enc2utf8(outcomes_all[type == "treatment"]$name),
  "Medicine" = enc2utf8(outcomes_all[type == "med"]$name)
))


choose_outcome <- enc2utf8("Choose illness or treatment")
# choose_theme <- enc2utf8("Vælge emne")

choose_aggr_lv <- enc2utf8("Choose stratification:")
choose_var <- enc2utf8("Choose metric:")
choose_rate_count <- enc2utf8("Choose rates/counts")

aggr_choices <-
  data.table(
    label = c("Year",
              "Age groups",
              "Education",
              "Municipality",
              "Region"
    ),
    label_long = c("Year",
                   "Age groups",
                   "Education",
                   "Municipality",
                   "Region"
    ),
    name_ht = c("national",
                "age", "edu", "kom", "region")
  )
row.names(aggr_choices) <- aggr_choices$label

count_rate_choices <- list("Rates" = as.integer(2),
                           "Counts" = as.integer(1))

ui_main_title <- enc2utf8("Cardiovascular diseases")
ui_age <- enc2utf8("Age")
ui_edu <- enc2utf8("Education")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")
ui_count_rate <-
  enc2utf8(c("Counts", "Age-specific rate", "Age-standardized rate"))
ui_read_more <- enc2utf8("Read more")
ui_percent <- enc2utf8("proportion")
ui_edu_age_range <- "35 - 84 years"
ui_moving_avg_desc <- "3-year moving average"

# Strings to place inside variable descriptions
replace_type_string_opr <- "fik foretaget en"
replace_type_string_diag <- "blev diagnosticeret med"
replace_type_string_med <- "fik udskrevet"
replace_allCVD_string <- "en hjerte-kar-sygdom"

# Tab names
ui_map <- "Map"
ui_d3_figures <- "Graph"
ui_data <- "Tabels"

# ABOUT MAIN PANEL -------------------------------------------------------------
ui_about_text <-
  read_fst(path = "data/ui_about_text.fst", as.data.table = TRUE)

ui_about_title <- "Methodds: cardiovascular diseases"
about_selection <- "Choose definition"

about_dat_diag <-
  merge(data.table(hjertetal_code = grep("d", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")


about_dat_opr <-
  merge(data.table(hjertetal_code = grep("b", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")

about_dat_med <-
  merge(data.table(hjertetal_code = grep("m", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")


about_choices <- list(
  "Illness" = "def_diag",
  "Procedures" = "def_opr",
  "Medicines" = "def_med",
  "Metrics" = "def_variables",
  "Population" = "def_populations",
  "Education" = "def_edu"
)
col_names_diag <-
  c("Illness",
    "Description",
    "ICD-code",
    "Diagnosis type",
    "Patient type")

col_names_opr <-
  c("Procedure",
    "Description",
    "ICD-code")

col_names_med <-
  c("Medicine type",
    "Description",
    "ATC code")

col_names_edu <- enc2utf8(c(
  "Education level (short)",
  "Education level (long)",
  "DISCED-15 code"
))

col_names_pop <- enc2utf8(c("Year", "Sex", "Age group", "Population"))


def_diag_title <- "Definition of illness"
def_opr_title <- "Definition of procedures"
def_med_title <- "Definition of medicine"
def_variables_title <- "Definition of metrics"
def_population_title <- "Definition of population"
def_stratas_title <- "Definition stratifications"

# CHD PANEL ---------------------------------------------------------------
outcome_descriptions_chd <-
  fread(file = "data/chd/outcome_descriptions_chd.csv", encoding = "UTF-8")

ui_chd_title <- enc2utf8("Medfødt hjertefejl")
choose_outcome_chd <- enc2utf8("Vælge medfødt hjetefjel:")
outcome_choices_chd <- c(enc2utf8(outcome_descriptions_chd$name_dk))

choose_var_chd <- enc2utf8("Vælg statistik:")
var_choices_chd <- fread("data/chd/variable_ui_chd.csv", encoding = "UTF-8")

ui_var_choices_chd <- var_choices_chd$code_name
names(ui_var_choices_chd) <- enc2utf8(var_choices_chd$var_dk)

aggr_levels_chd <- readRDS("data/chd/aggr_levels_chd.rds")
aggr_levels_chd_pretty <- c("Alder", "Køn", "Total")

aggr_levels_chd <- rev(aggr_levels_chd)
aggr_levels_chd_pretty <- rev(aggr_levels_chd_pretty)
choose_aggr_chd <-  enc2utf8("Opdelt efter:")

ui_replace_all_chd <- "en medfødt hjertefjel"

# ABOUT CHD PANEL ---------------------------------------------------------

var_descriptions_chd <- fread("data/chd/variable_ui_chd.csv ", encoding = "UTF-8")


ui_about_title_chd <- "Metoder: medfødt hjertefjel"
about_selection_chd <- "Vælg definition"
ui_about_text <-
  fread(file = "data/chd/ui_about_text_chd.csv", encoding = "UTF-8")


about_choices_chd <- list(
  "Sygdomme" = "def_diag"
)

col_names_diag_about_chd <-
  c("Sygdomme",
    "Beskrivlse",
    "ICD-8 kode",
    "ICD-10 kode",
    "Diagnose type",
    "Patient type")


# CODE PANEL --------------------------------------------------------------
ui_code_title <- "Code"

