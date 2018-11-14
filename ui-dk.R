

choose_outcome <- enc2utf8("Vælge sygdome eller behandling:")
choose_theme <- enc2utf8("Vælge emne")
choose_year <- enc2utf8("Vælge år")

theme_names <- enc2utf8(
  c(
    "Prevalence/incidence" = "cases",
    "Dødlighed" = "mortality",
    "Hospital" = "hospital"
  )
)


ui_age <- enc2utf8("Aldre")
ui_edu <- enc2utf8("Uddannelse")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")




ui_colnames_cases <-
  enc2utf8(
    c(
      "År",
      "Sex",
      "Alder",
      "Antal patienter",
      "Antal procedurer",
      "Antal døde efter 30 dage",
      "Antal døde efter 1 år"
    )
  )
