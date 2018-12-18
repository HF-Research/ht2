library(heaven)
source("r/data-preprocessing.R")
# Use hjertetal_code to merge names and descriptions of outcomes. This will be
# in seperate script run once - not on every launch.
load(file = "data/shiny_list.rda")
load(file = "data/codes_tables.rda")
load(file = "data/export_med.Rdata")


outcome_descriptions <-
  fread(file = "data/descriptions.csv", encoding = "UTF-8")
variable_names <- fread(file = "data/variable_ui_text.csv", encoding = "UTF-8")

code_tables$behandling
outcome_names_treatment <-
  merge(data.table(hjertetal_code = names(shiny_list$opr_dat)),
        code_tables$behandling,
        by = "hjertetal_code")[, .(hjertetal_code, opr_name_dk, opr_name_en)]
colnames(outcome_names_treatment) <- c("hjertetal_code", "name_dk", "name_en")
outcome_names_med <-
  merge(data.table(hjertetal_code = names(export_med)),
        code_tables$med,
        by = "hjertetal_code")[, .(hjertetal_code, med_name_dk, med_name_en)]
colnames(outcome_names_med) <- c("hjertetal_code", "name_dk", "name_en")
outcomes_all <- rbind(outcome_names_treatment, outcome_names_med)




# Add blank initial choice for dropdowns - so forces user to choose actively
outcome_choices <- c(
  list(
    "Behandling" = outcome_names_treatment$name_dk,
    "Medicin" = outcome_names_med$name_dk
  )
)

dropdown_tooltip = enc2utf8("Click to choose data")

choose_outcome <- enc2utf8("Vælge sygdome eller behandling:")
choose_theme <- enc2utf8("Vælge emne")
choose_year <- enc2utf8("Vælge år")
choose_aggr_lv <- enc2utf8("Vælge metric")
choose_var <- enc2utf8("Vælge variable")

theme_names <- enc2utf8(
  c(
    "Prevalence/incidence" = "cases",
    "Dødlighed" = "mortality",
    "Hospital" = "hospital"
  )
)
theme_names <- c(theme_names)

aggr_choices <-
  list(
    "Alder" = "age",
    "Uddannelse" = "edu",
    "Region" = "kom",
    "National" = "national"
  )

variable_choices_opr <- list(
  "Antal patienter" = "n_patients",
  "Antal procedurer" = "n_oprs",
  "Antal døde: 1 år" = "mort_1y"
)

variable_choices_med <- list(
  "Antal patienter" = "n_patients",
  "Antal procedurer" = "n_oprs",
  "Antal døde: 30 dage" = "n_dead_30",
  "Antal døde: 1 år" = "n_dead_1yr"
)



ui_age <- enc2utf8("Aldre")
ui_edu <- enc2utf8("Uddannelse")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")
ui_sex <- enc2utf8("Køn")
ui_year <- enc2utf8("År")
ui_sex_levels <- enc2utf8(c("Kvinde", "Mand"))


var_names_opr_pretty <- sapply(names(variable_choices_opr), c)
ui_colnames_cases_age <-
  enc2utf8(c(ui_sex,
             "Alder",
             var_names_opr_pretty))

ui_colnames_cases_edu <-
  enc2utf8(c(ui_sex,
             "Uddannelse",
             var_names_opr_pretty))

ui_colnames_cases_region <-
  enc2utf8(c(ui_sex,
             "Region",
             var_names_opr_pretty))

ui_colnames_cases_national <-
  enc2utf8(c(ui_sex,
             "År",
             var_names_opr_pretty))
