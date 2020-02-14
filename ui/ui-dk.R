# INTRO -------------------------------------------------------------------
###########################
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
###########################

# Use hjertetal_code to merge names and descriptions of outcomes. This will be
# in seperate script run once - not on every launch.
outcome_descriptions <-
  fread(file = "data/outcome_descriptions.csv", encoding = "UTF-8")
# load(file = "data/variable_ui.Rdata")
variable_ui <-
  fread(file = "data/variable_ui.csv",
        encoding = "UTF-8",
        header = TRUE)
edu <- fread(file = "data/edu_description.csv", encoding = "UTF-8")
ui_about_text <-
  fread(file = "data/ui_about_text.csv", encoding = "UTF-8")


# Encode to native
outcome_descriptions <-
  outcome_descriptions[, lapply(.SD, enc2native)]
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]


outcome_names_treatment <-
  merge(data.table(hjertetal_code = grep("b", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_treatment) <-
  c("hjertetal_code", "name_dk", "name_en")
outcome_names_med <-
  merge(data.table(hjertetal_code = grep("m", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_med) <-
  c("hjertetal_code", "name_dk", "name_en")
outcome_names_diag <-
  merge(data.table(hjertetal_code = grep("d", names(shiny_dat), value = TRUE)),
        outcome_descriptions,
        by = "hjertetal_code")[, .(hjertetal_code, name_dk, name_en)]
colnames(outcome_names_diag) <-
  c("hjertetal_code", "name_dk", "name_en")



outcomes_all <-
  rbind(outcome_names_diag,
        outcome_names_treatment,
        outcome_names_med)



outcome_descriptions_chd <-
  fread(file = "data/chd/outcome_descriptions_chd.csv", encoding = "UTF-8")
var_descriptions_chd <- fread("data/chd/variable_ui_chd.csv ", encoding = "UTF-8")



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
  "Sygdomme" = enc2utf8(outcome_names_diag$name_dk),
  "Behandling" = enc2utf8(outcome_names_treatment$name_dk),
  "Medicin" = enc2utf8(outcome_names_med$name_dk)
))


dropdown_tooltip = enc2utf8("Click to choose data")
choose_outcome <- enc2utf8("Vælge sygdom eller behandling:")
# choose_theme <- enc2utf8("Vælge emne")

choose_aggr_lv <- enc2utf8("Opdelt efter:")
choose_var <- enc2utf8("Vælg statistik:")
choose_rate_count <- enc2utf8("Vælg rater/antal:")

aggr_choices <-
  data.table(
    name_dk = c("Alder",
                "Uddannelse",
                "Kommune",
                "Region",
                "År"),
    name_dk_long = c("Aldersgruppe",
                     "Uddannelsesgruppe",
                     "Kommune",
                     "Region",
                     "År"),
    name_ht = c("age", "edu", "kom", "region", "national")
  )
row.names(aggr_choices) <- aggr_choices$name_dk

count_rate_choices <- list("Rate" = as.integer(2),
                           "Antal" = as.integer(1))

ui_main_title <- enc2utf8("Hjemme")
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

# ABOUT PANEL -------------------------------------------------------------
ui_about_title <- "Metoder"
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
ui_chd_title <- "CHD"
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

# CODE PANEL --------------------------------------------------------------
ui_code_title <- "Code"

