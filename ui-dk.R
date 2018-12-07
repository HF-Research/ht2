library(heaven)

# Use hjertetal_code to merge names and descriptions of outcomes
load(file = "data/export_summaries_opr.Rdata")
load(file = "data/codes_tables.rda")
code_tables$behandling
outcome_names_treatment <- data.table(hjertetal_code = names(export))
merge(code_tables$behandling, outcome_names_treatment, by = "hjertetal_code")




# Add blank initial choice for dropdowns - so forces user to choose actively
outcome_choices <- c("",
                     list(
                       "Sygdom" = c(),
                       "Behandling" = outcome_names_treatment,
                       "Medicin" = c()
                     ))

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
theme_names <- c("", theme_names)

aggr_choices <-
  list(
    "Alder" = "age",
    "Uddannelse" = "edu",
    "Region" = "region",
    "National" = "national"
  )

variable_choices_opr <- list(
  "Antal patienter" = "n_patients",
  "Antal procedurer" = "n_oprs",
  "Antal døde: 30 dage" = "n_dead_30",
  "Antal døde: 1 år" = "n_dead_1yr"
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



var_names_opr_pretty <- sapply(names(variable_choices_opr), c)
ui_colnames_cases_age <-
  enc2utf8(
    c(
      "Sex",
      "Alder",
      var_names_opr_pretty
    )
  )

ui_colnames_cases_edu <-
  enc2utf8(
    c(
      "Sex",
      "Uddannelse",
      var_names_opr_pretty
    )
  )

ui_colnames_cases_region <-
  enc2utf8(
    c(
      "Sex",
      "Region",
      var_names_opr_pretty
    )
  )

ui_colnames_cases_national <-
  enc2utf8(
    c(
      "Sex",
      "År",
      var_names_opr_pretty
    )
  )
