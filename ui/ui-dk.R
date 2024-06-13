# INTRO´- OPDATERET MARIANNA 10-05-2024 -------------------------------------------------------------------
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

col_names_updates <- c(
  'Dato',
  'Beskrivelse'
)



# MAIN PANEL -----------------------------------------------
# Everything below here will need to be changed for the english verison

# Outcome dropdown, broken up into sections

diag_choices <- (enc2utf8(outcomes_all[type == "diag"]$hjertetal_code))
names(diag_choices) <- (enc2utf8(outcomes_all[type == "diag"]$name))

opr_choices <- enc2utf8(outcomes_all[type == "treatment"]$hjertetal_code)
names(opr_choices) <- (enc2utf8(outcomes_all[type == "treatment"]$name))

med_choices <- enc2utf8(outcomes_all[type == "med"]$hjertetal_code)
names(med_choices) <- (enc2utf8(outcomes_all[type == "med"]$name))

outcome_choices <- c(list(
  "Sygdomme" = diag_choices,
  "Behandling" = opr_choices,
  "Medicin" = med_choices
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

count_rate_choices <- list("Rate" = "rate",
                           "Antal" = "count")

ui_main_title <- enc2utf8("Hjerte-kar-sygdomme")
ui_age <- enc2utf8("Aldre")
ui_edu <- enc2utf8("Uddannelse")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")
ui_count_rate <-
  enc2utf8(c("Antal", "Aldersspecifikke rate", "Aldersstandardiserede rate"))
ui_read_more <- enc2utf8("Læse mere")
ui_percent <- enc2utf8("andel")
ui_age_35plus <- "(18+ årige)"
ui_edu_age_range <- "18 - 79 årige"
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


# Data_warning
ui_data_warning <- ""
# ABOUT MAIN PANEL -------------------------------------------------------------

ui_about_title <- "Vejledning: hjerte-kar-sygdomme"
about_selection <- "Vælg emne:"
about_dat_updates <- readRDS(file = 'language/updates_dk.rds')



about_choices <- list(
  "Generelt/FAQ" = "general",
  "Opdatering" = "updates",
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
                "Dødelighed af hjerte-kar-sygdomme fra 2006 til 2021.",
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
    medier. Den fungerer på computeren, iPad, iPhone, og Android.
    
     </br></br>Omkring 670.000 danskere lever med en hjerte-kar-sygdom*

</br></br>*Antallet af patienter med hjerte-kar-sygdom kan ikke direkte sammenlignes med tidligere opgørelser.
Det skyldes b.la. at der i 2019 kom en ny version af Landspatientregistret, hvor sygdomsforløb defineres anderledes end tidligere. 
Derudover er populationen i denne opdatering udvidet til at inkludere alle danskere over 18 år og inkluderer nye sygdomsgrupper.
Ændringerne i den nye beregningsmetode er foretaget for at harmonisere med Sundhedsdatastyrelsen.
Tal for medfødt hjertefejl er IKKE opdateret.
    
    </br></br>Hvis du har feedback eller anbefalinger, skriv venligst til:
</br> <b>Marianna Meaidi</b>, Data Scientist, Hjerteforeningen
</br>Telefon: 30722587,
</br>e-mail: <b>mmeaidi@hjerteforeningen.dk</b>
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
  "Vi viser ikke nye tilfælde (incidens) for svær medfødt hjertefejl opdelt efter 'Alder' samt 'Alder-køn' kombineret.
  </br></br> Vælg venligst noget andet og se vores FAQ for mere information."

# ABOUT CHD PANEL ---------------------------------------------------------


ui_about_title_chd <- "Vejledning: medfødt hjertefejl"
about_selection_chd <- "Vælg definition"
ui_about_text_chd <-
  readRDS(file = "language/ui_about_text_chd_dk.rds")


about_choices_chd <- list(
  "Generelt/FAQ" = "general",
  "Sygdomme" = "def_diag",
  "Operationer" = "def_opr"
  )

col_names_diag_about_chd <-
  c("Sygdomme",
    "Beskrivelse",
    "ICD-8 kode",
    "ICD-10 kode",
    "Diagnose-type",
    "Patient-type")

# UI general tabs - intro to HjerteTal
ui_gen_1_chd <- enc2utf8("I HjerteTal er det muligt at finde oplysninger om:")

bullets <- list("Forekomsten og fordelingen af medfødt hjertefejl i den dansk befolkning.",
                "Fordelingen kan vurderes på baggrund af køn, alder, og både køn og alder smatidig.",
                "Udviklingen af medfødt hjertefejl fra 2006 og frem."
                
)
ui_bullets_chd <- lapply(bullets, enc2utf8)
ui_gen_2_chd <-
  enc2utf8(
    "Databasen er udviklet ved Hjerteforeningen og opdateres én gang årligt.
    Opgørelserne findes som tabeller, søjlediagrammer eller som Danmarkskort,
    hvor du let kan få et overblik over kommunale og regionale forskelle.
    HjerteTal er frit tilgængelig og kan bruges af alle interesserede, fx
    læger, forskere, politiske beslutningstagere, sundhedsadministratorer og
    medier. Den fungerer på computeren, iPad, iPhone, og Android.
    
    </br></br>Hvis du har feedback eller anbefalinger, skriv venligst til:
</br> <b>Marianna Meaidi</b>, Data Scientist, Hjerteforeningen
</br>Telefon: 30722587,
</br>e-mail: <b>mmeaidi@hjerteforeningen.dk</b>
    <hr>
    <h3> FAQ (Ofte stillede spørgsmål):</h3>"
  )

# CODE PANEL --------------------------------------------------------------
ui_code_title <- "Code"

