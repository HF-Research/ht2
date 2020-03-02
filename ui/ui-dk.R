# INTRO -------------------------------------------------------------------
###########################
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
###########################

# ALL PANELS --------------------------------------------------------------
ui_sex <- enc2utf8("Køn")
ui_year <- enc2utf8("År")
ui_sex_levels <- enc2utf8(c("Kvinde", "Mand"))
choose_year <- enc2utf8("Vælg år:")
ui_download <- "Hent"
ui_download_graph <- "Hent figur"
ui_the <- "Den"

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
                "Region",
              "Etnicitet"
),
    label_long = c("År",
                     "Aldersgruppe",
                     "Uddannelsesgruppe",
                     "Kommune",
                     "Region",
                   "Etnicitet"
                     ),
name_ht = c("national",
            "age", "edu", "kom", "region", "ethnicity")
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

ui_about_title <- "Metoder: hjerte-kar sygdomme"
about_selection <- "Vælg definition"



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
file.path <- paste0("language/outcome_descriptions_chd_", lang, ".rds")
outcome_descriptions_chd <-
  readRDS(file =  file.path)

ui_chd_title <- enc2utf8("Medfødt hjertefejl")
choose_outcome_chd <- enc2utf8("Vælge medfødt hjetefjel:")


choose_var_chd <- enc2utf8("Vælg statistik:")



aggr_levels_chd_pretty <- c( "Total", "Køn", "Alder")

choose_aggr_lv_chd <-  enc2utf8("Opdelt efter:")

ui_replace_all_chd <- "en medfødt hjertefjel"

# ABOUT CHD PANEL ---------------------------------------------------------


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

