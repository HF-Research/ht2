# INTRO -------------------------------------------------------------------
###########################
# Requires shiny_dat_* to be loaded - but will load this in global.R file before running this ui file
###########################





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
diag_choices <- (enc2utf8(outcomes_all[type == "diag"]$hjertetal_code))
names(diag_choices) <- (enc2utf8(outcomes_all[type == "diag"]$name))

opr_choices <- enc2utf8(outcomes_all[type == "treatment"]$hjertetal_code)
names(opr_choices) <- (enc2utf8(outcomes_all[type == "treatment"]$name))

med_choices <- enc2utf8(outcomes_all[type == "med"]$hjertetal_code)
names(med_choices) <- (enc2utf8(outcomes_all[type == "med"]$name))


outcome_choices <- c(list(
  "Illness" = diag_choices,
  "Procedure" = opr_choices,
  "Medicine" = med_choices
))


choose_outcome <- enc2utf8("Choose illness or treatment")


choose_aggr_lv <- enc2utf8("Choose stratification:")
choose_var <- enc2utf8("Choose metric:")
choose_rate_count <- enc2utf8("Choose rates/counts")

aggr_choices <-
  data.table(
    label = c("Year",
              "Age groups",
              "Education",
              "Municipality",
              "Region",
              "Ethnicity"),
    label_long = c("Year",
                   "Age groups",
                   "Education",
                   "Municipality",
                   "Region",
                   "Ethnicity"),
    name_ht = c("national",
                "age", "edu", "kom", "region", "ethnicity")
  )
row.names(aggr_choices) <- aggr_choices$label

count_rate_choices <- list("Rates" = "rate",
                           "Counts" = "count")

ui_main_title <- enc2utf8("Cardiovascular diseases")
ui_age <- enc2utf8("Age")
ui_edu <- enc2utf8("Education")
ui_region <- enc2utf8("Region")
ui_national <- enc2utf8("National")
ui_count_rate <-
  enc2utf8(c("Counts", "Age-specific rate", "Age-standardized rate"))
ui_read_more <- enc2utf8("Read more")
ui_percent <- enc2utf8("proportion")
ui_edu_age_range <- "35 - 79 years"
ui_moving_avg_desc <- "3-year moving average"

# Strings to place inside variable descriptions
replace_type_string_opr <- "having the operation"
replace_type_string_diag <- "being diagnosed with"
replace_type_string_med <- "received"
replace_allCVD_string <- "any cardiovascular disease"

# Tab names
ui_map <- "Map"
ui_d3_figures <- "Graph"
ui_data <- "Tabels"

# ABOUT MAIN PANEL -------------------------------------------------------------

ui_about_title <- "Methods: cardiovascular diseases"
about_selection <- "Choose definition"

col_names_faq <- c(
  "Questions",
  "Anwsers"
)


about_choices <- list(
  "General" = "general",
  "Illness" = "def_diag",
  "Procedures" = "def_opr",
  "Medicines" = "def_med",
  "Education" = "def_edu",
  "Ethnicity" = "def_ethnicity",
  "Population" = "def_populations"
  
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

col_names_ethnicity <- enc2utf8(c(
  "Code",
  "Country",
  "Ethnic grouping"
))

def_diag_title <- "Definition of illness"
def_opr_title <- "Definition of procedures"
def_med_title <- "Definition of medicine"
def_variables_title <- "Definition of metrics"
def_population_title <- "Definition of population"
def_stratas_title <- "Definition stratifications"

ui_gen_1 <- enc2utf8("In HjerteTal, it's possible to find information on:")

bullets <- list("A range of metrics describing the occurance and distribution of heart diseases in the adult Danish population",
                "How heart diseases are distributed on the basis of age, sex, educcational level, ethnicity, and place of residence",
                "The trends in heart diseases over time, starting in 2004",
                "Mortality after invasive treatments or diagnosis with a heart disease",
                "The use of medications used to treat people with heart diseases"
)
ui_bullets <- lapply(bullets, enc2utf8)
ui_gen_2 <-
  enc2utf8(
    "The database is developed by The Danish Heart Foundation, and is updated once
    yearly. The data is available as graphs, tables or interactive maps (when
    applicable).HjerteTal if free to use for all interested, for example,
    doctors, researchers, policy makers, health care administrators, or the media.
    It is accesible on, iPad, iPhone, and Android, no app download is required
    
    <br><br>If you have questions, recommendations, or feedback, please write to:
    </br> <b>Maja Bülow Lykke</b>, Research consultant, The Danish Heart Foundtion
    </br>Telephone: 3366 9953,
    </br>e-mail: <b>mblykke@hjerteforeningen.dk</b>
    
    </br><h3>FAQ:</h3>"
  )


# CHD PANEL ---------------------------------------------------------------

ui_chd_title <- enc2utf8("Congenital heart defects")
choose_outcome_chd <- enc2utf8("Choose conongenital heart defect category")


choose_var_chd <- enc2utf8("Choose metric:")

aggr_levels_chd_pretty <- c("Totals", "Age", "Sex", "Age-sex")

choose_aggr_lv_chd <-  enc2utf8("Choose stratification:")

ui_replace_all_chd <- "en medfødt hjertefjel"

ui_warning_invalid_selection <-
"We cannot show new cases (incidence) of severe congenital heart disease by
'Age' or 'Age-sex' combined. </br></br> Please make another seleciton, and
read the FAQ for more information."

# ABOUT CHD PANEL ---------------------------------------------------------




ui_about_title_chd <- "Methods: congenital heart defects"
about_selection_chd <- "Choose definition"
ui_about_text_chd <-
  readRDS(file = "language/ui_about_text_chd_en.rds")


about_choices_chd <- list(
  "General/FAQ" = "general",
  "Illness" = "def_diag",
  "Operations" = "def_opr"
)

col_names_diag_about_chd <-
  c("Congenital heart defect category",
    "Description",
    "ICD-8 code",
    "ICD-10 code",
    "Diagnosis type",
    "Patient type")


ui_gen_1_chd <- enc2utf8("In HjerteTal, it's possible to find information on:<br><br>")

bullets <-
  list(
    "The incidence and prevalence of congenital heart defects (CHD) in the Danish population ",
    "The distribution of congenital heart disease across age, sex, and both age and sex together",
    "The trends in congenital heart disease over time since 2004"
  )

ui_bullets_chd <- lapply(bullets, enc2utf8)

ui_gen_2_chd <-
  enc2utf8(
    "The database is developed by The Danish Heart Foundation, and is updated
    once yearly. The data is available as graphs or tables. HjerteTal if free
    to use for all interested, for example, doctors, researchers, policy
    makers, health care administrators, or the media. It is accesible on,
    iPad, iPhone, and Android, no app download is required.
    
    <br><br>If you have questions, recommendations, or feedback, please write to:
    </br> <b>Maja Bülow Lykke</b>, Research consultant, The Danish Heart Foundtion
    </br>Telephone: 3366 9953,
    </br>e-mail: <b>mblykke@hjerteforeningen.dk</b>
    
    </br><h3>FAQ:</h3>"
  )

# CODE PANEL --------------------------------------------------------------
ui_code_title <- "Code"

