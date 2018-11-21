


choose_outcome <- enc2utf8("Vælge sygdome eller behandling:")
choose_theme <- enc2utf8("Vælge emne")
choose_year <- enc2utf8("Vælge år")
choose_aggr_lv <- enc2utf8("Vælge metric")


theme_names <- enc2utf8(
  c(
    "Prevalence/incidence" = "cases",
    "Dødlighed" = "mortality",
    "Hospital" = "hospital"
  )
)

aggr_choices <-
  list(
    "Aldre" = "age",
    "Uddannelse" = "edu",
    "Region" = "region",
    "National" = "national"
  )



ui_age <- enc2utf8("Aldre")
ui_edu <- enc2utf8("Uddannelse")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")




ui_colnames_cases_age <-
  enc2utf8(
    c(
      
      "Sex",
      "Alder",
      "Antal patienter",
      "Antal procedurer",
      "Antal døde efter 30 dage",
      "Antal døde efter 1 år"
    )
  )

ui_colnames_cases_edu <-
  enc2utf8(
    c(
      
      "Sex",
      "Uddannelse",
      "Antal patienter",
      "Antal procedurer",
      "Antal døde efter 30 dage",
      "Antal døde efter 1 år"
    )
  )

ui_colnames_cases_region <-
  enc2utf8(
    c(
      
      "Sex",
      "Region",
      "Antal patienter",
      "Antal procedurer",
      "Antal døde efter 30 dage",
      "Antal døde efter 1 år"
    )
  )

ui_colnames_cases_national <-
  enc2utf8(
    c(
      "År",
      "Sex",
      "Region",
      "Antal patienter",
      "Antal procedurer",
      "Antal døde efter 30 dage",
      "Antal døde efter 1 år"
    )
  )
