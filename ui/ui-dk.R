# INTRO -------------------------------------------------------------------
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
# ALL PANELS --------------------------------------------------------------
ui_sex <- enc2utf8("Køn")
ui_year <- enc2utf8("År")
ui_sex_levels <- enc2utf8(c("Kvinde", "Mand"))
choose_year <- enc2utf8("Vælg år:")
ui_download <- "Hent"
ui_download_graph <- "Hent figur"
ui_the <- "Den"

col_names_faq <- c(
  "Spørgsmål",
  "Svar"
)


# MAIN PANEL -----------------------------------------------
# Everything below here will need to be changed for the english verison

# Outcome dropdown, broken up into sections
outcome_choices <- c(list(
  "Sygdomme" = enc2utf8(outcomes_all[type == "diag"]$name),
  "Behandling" = enc2utf8(outcomes_all[type == "treatment"]$name),
  "Medicin" = enc2utf8(outcomes_all[type == "med"]$name)
))


choose_outcome <- enc2utf8("Vælg sygdom eller behandling:")


choose_aggr_lv <- enc2utf8("Opdelt efter:")
choose_var <- enc2utf8("Vælg statistik:")
choose_rate_count <- enc2utf8("Vælg rate/antal:")

aggr_choices <-
  data.table(
    label = c("År",
              "Alder",
              "Uddannelse",
              "Kommune",
              "Region",
              "Etnicitet"),
    label_long = c(
      "År",
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

ui_main_title <- enc2utf8("Hjerte-kar-sygdomme")
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

ui_about_title <- "Vejledning: hjerte-kar-sygdomme"
about_selection <- "Vælg definition"



about_choices <- list(
  "Generelt/FAQ" = "general",
  "Sygdomme" = "def_diag",
  "Procedurer" = "def_opr",
  "Medicin" = "def_med",
  "Uddannelse" = "def_edu",
  "Etnisk herkomst" = "def_ethnicity",
  "Befolkninger" = "def_populations"
  
)


col_names_diag <-
  c("Sygdomme",
    "Beskrivelse",
    "ICD-kode",
    "Diagnosetype",
    "Patienttype")

col_names_opr <-
  c("Sygdomme",
    "Beskrivelse",
    "ICD-kode")

col_names_med <-
  c("Medicintype",
    "Beskrivelse",
    "ATC kode")

col_names_edu <- enc2utf8(c(
  "Uddannelsesniveau",
  "Beskrivelse",
  "DISCED-15 kode"
))

col_names_pop <- enc2utf8(c("År", "Køn", "Aldersgrupper", "Antal"))

col_names_ethnicity <- enc2utf8(c(
  "Kode",
  "Land",
  "Gruppe"
))

def_diag_title <- "Definitioner af sygdomme"
def_opr_title <- "Definitioner af procedurer"
def_med_title <- "Definitioner af medicin"
def_variables_title <- "Definitioner af statistiker"
def_population_title <- "Definitioner af befolkninger"
def_stratas_title <- "Definitioner af stratifikationer"

# UI general tabs - intro to HjerteTal
ui_gen_1 <- enc2utf8("I HjerteTal er det muligt at finde oplysninger om:")

bullets <- list("Forekomsten og fordelingen af hjerte-kar-sygdomme i den voksne befolkning i Danmark.",
                "Fordelingen kan vurderes på baggrund af køn, alder, uddannelse, etnisk herkomst og geografisk placering (bopæl).",
                "Udviklingen af hjerte-kar-sygdomme fra 2006 og frem.",
                "Overlevelsen efter hjerte-kar-sygdom og efter invasive behandlinger.",
                "Omfanget af forbruget af hjerte-kar-medicin."
)
ui_bullets <- lapply(bullets, enc2utf8)


ui_gen_2 <-
  enc2utf8(
    "Databasen er udviklet ved Hjerteforeningen og opdateres én gang årligt.
    Opgørelserne findes som tabeller, søjlediagrammer eller som Danmarkskort,
    hvor du let kan få et overblik over kommunale og regionale forskelle.
    HjerteTal er frit tilgængelig og kan bruges af alle interesserede, fx
    læger, forskere, politiske beslutningstagere, sundhedsadministratorer og
    medier. Den fungerer på computeren, iPad, iPhone, og Android
    
    </br></br>Hvis du har feedback eller anbefalinger, skriv venligst til:
</br> <b>Maja Bülow Lykke</b>, Forskningskonsulent, Hjerteforeningen
</br>Telefon: 3366 9953,
</br>e-mail: <b>mblykke@hjerteforeningen.dk</b>
    <hr>
    <h3> FAQ (Ofte stillede spørgsmål):</h3>"
    )


# CHD PANEL ---------------------------------------------------------------
file.path <- paste0("language/outcome_descriptions_chd_", lang, ".rds")
outcome_descriptions_chd <-
  readRDS(file =  file.path)

ui_chd_title <- enc2utf8("Medfødt hjertefejl")
choose_outcome_chd <- enc2utf8("Vælg medfødt hjertefejl:")


choose_var_chd <- enc2utf8("Vælg statistik:")



aggr_levels_chd_pretty <- enc2utf8(c( "Total", "Alder","Køn", "Alder-køn"))

choose_aggr_lv_chd <-  enc2utf8("Opdelt efter:")

ui_replace_all_chd <- "en medfødt hjertefejl"

ui_warning_invalid_selection <-
  "We do not show new cases (incidence) of severe congenital heart disease
  stratified by age or age-sex.</br></br> Please make another selection and
  see the FAQ for more information."

# ABOUT CHD PANEL ---------------------------------------------------------


ui_about_title_chd <- "Vejledning: medfødt hjertefejl"
about_selection_chd <- "Vælg definition"
ui_about_text_chd <-
  fread(file = "data/chd/ui_about_text_chd.csv", encoding = "UTF-8")


about_choices_chd <- list(
  "Generelt/FAQ" = "general",
  "Sygdomme" = "def_diag"
  )

col_names_diag_about_chd <-
  c("Sygdomme",
    "Beskrivelse",
    "ICD-8 kode",
    "ICD-10 kode",
    "Diagnosetype",
    "Patienttype")

# UI general tabs - intro to HjerteTal
ui_gen_1_chd <- enc2utf8("I HjerteTal er det muligt at finde oplysninger om:")

bullets <- list("Forekomsten og fordelingen af medfødt hjertefejl i den dansk befolkning.",
                "Fordelingen kan vurderes på baggrund af køn, alder, og både køn og alder smatidig",
                "Udviklingen af medfødt hjertefejl fra 2004 og frem."
                
)
ui_bullets_chd <- lapply(bullets, enc2utf8)
ui_gen_2_chd <-
  enc2utf8(
    "Databasen er udviklet ved Hjerteforeningen og opdateres én gang årligt.
    Opgørelserne findes som tabeller, søjlediagrammer eller som Danmarkskort,
    hvor du let kan få et overblik over kommunale og regionale forskelle.

    HjerteTal er frit tilgængelig og kan bruges af alle interesserede, fx
    læger, forskere, politiske beslutningstagere, sundhedsadministratorer og
    medier. Den fungerer på computeren, iPad, iPhone, og Android
    
    </br></br>Hvis du har feedback eller anbefalinger, skriv venligst til:
    </br> <b>Maja Bülow Lykke</b>, Forskningskonsulent, Hjerteforeningen
    </br>Telefon: 3366 9953,
    </br>e-mail: <b>mblykke@hjerteforeningen.dk</b>
    <hr>
    <h3> FAQ (Ofte stillede spørgsmål):</h3>"
  )

# CODE PANEL --------------------------------------------------------------
ui_code_title <- "Code"

