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
outcome_descriptions <-
  outcome_descriptions[, lapply(.SD, enc2utf8)]
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]

file_name <- paste0("data/outcomes_all_", lang, ".rds")
outcomes_all <- readRDS(file_name)




# ALL PANELS --------------------------------------------------------------
ui_sex <- enc2utf8("Køn")
ui_year <- enc2utf8("År")
ui_sex_levels <- enc2utf8(c("Kvinde", "Mand"))
choose_year <- enc2utf8("Vælg år:")
ui_download <- "Hent"
ui_download_graph <- "Hent figur"


# MAIN PANEL -----------------------------------------------
# Everything below here will need to be changed for the english verison

# Outcome dropdown, broken up into sections
outcome_choices <- c(list(
  "Sygdomme" = enc2utf8(outcomes_all[type == "diag"]$name),
  "Behandling" = enc2utf8(outcomes_all[type == "treatment"]$name),
  "Medicin" = enc2utf8(outcomes_all[type == "med"]$name)
))


choose_outcome <- enc2utf8("Vælge sygdom eller behandling:")
# choose_theme <- enc2utf8("Vælge emne")

choose_aggr_lv <- enc2utf8("Opdelt efter:")
choose_var <- enc2utf8("Vælg statistik:")
choose_rate_count <- enc2utf8("Vælg rater/antal:")

aggr_choices <-
  data.table(
    label = c("År",
                "Alder",
                "Uddannelse",
                "Kommune",
                "Region"
),
    label_long = c("År",
                     "Aldersgruppe",
                     "Uddannelsesgruppe",
                     "Kommune",
                     "Region"
                     ),
name_ht = c("national",
            "age", "edu", "kom", "region")
  )
row.names(aggr_choices) <- aggr_choices$label

count_rate_choices <- list("Rate" = as.integer(2),
                           "Antal" = as.integer(1))

ui_main_title <- enc2utf8("Hjerte-kar sygdomme")
ui_age <- enc2utf8("Aldre")
ui_edu <- enc2utf8("Uddannelse")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")
ui_count_rate <-
  enc2utf8(c("Antal", "Aldersspecifikke rate", "Aldersstandardiserede rate"))
ui_read_more <- enc2utf8("Læse mere")
ui_percent <- enc2utf8("andel")
ui_edu_age_range <- "35 - 84 årige"
ui_moving_avg_desc <- "3-år glidende gennemsnit"

# Strings to place inside variable descriptions
replace_type_string_opr <- "fik foretaget en"
replace_type_string_diag <- "blev diagnosticeret med"
replace_type_string_med <- "fik udskrevet"
replace_allCVD_string <- "en hjerte-kar-sygdom"

# Tab names
ui_map <- "Kort"
ui_d3_figures <- "Grafer"
ui_data <- "Tabeller"

# ABOUT MAIN PANEL -------------------------------------------------------------
ui_about_text <-
  read_fst(path = "data/ui_about_text.fst", as.data.table = TRUE)

ui_about_title <- "Metoder: hjerte-kar sygdomme"
about_selection <- "Vælg definition"

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
  "Sygdomme" = "def_diag",
  "Procedurer" = "def_opr",
  "Medicin" = "def_med",
  "Statistik" = "def_variables",
  "Befolkninger" = "def_populations",
  "Uddannelse" = "def_edu"
)
col_names_diag <-
  c("Sygdomme",
    "Beskrivlse",
    "ICD-kode",
    "Diagnose type",
    "Patient type")

col_names_opr <-
  c("Sygdomme",
    "Beskrivlse",
    "ICD-kode")

col_names_med <-
  c("Medicin type",
    "Beskrivlse",
    "ATC kode")

col_names_edu <- enc2utf8(c(
  "Uddannelsesniveau (kort)",
  "Uddannelsesniveau (lang)",
  "DISCED-15 kode"
))

col_names_pop <- enc2utf8(c("År", "Sex", "Ælder grupper", "Befolkningen"))


def_diag_title <- "Definitioner af sygdomme"
def_opr_title <- "Definitioner af procedurer"
def_med_title <- "Definitioner af medicin"
def_variables_title <- "Definitioner af statistiker"
def_population_title <- "Definitioner af befolkninger"
def_stratas_title <- "Definitioner af stratifikationer"

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

