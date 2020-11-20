# UI elements that are coded common for both language UIs
# 
# 
# # Use hjertetal_code to merge names and descriptions of outcomes. This will be
# in seperate script run once - not on every launch.
file.path <- paste0("language/outcome_descriptions_", lang, ".rds")
outcome_descriptions <-
  readRDS(file =  file.path)

variable_ui <-
  read_fst(path = "data/variable_ui.fst", as.data.table = TRUE
  )
edu <- read_fst(path = "data/edu_description.fst", as.data.table = TRUE)



# Encode to native
variable_ui <- variable_ui[, lapply(.SD, enc2native)]
edu <- edu[, lapply(.SD, enc2native)]

file_name <- paste0("data/outcomes_all_", lang, ".rds")
outcomes_all <- readRDS(file_name)



# MAIN-ABOUT SECTION ------------------------------------------------------

file.path <- paste0("language/about_ui_", lang, ".rds")
ui_about_text <-
  readRDS(file =  file.path)

file.path <- paste0("language/country_grps_", lang, ".rds")
country_grps <-
  readRDS(file =  file.path)


about_dat_faq <- fread(file = "input_ui_text/faq.csv", encoding = "UTF-8")
keep_vars <- grep(lang, colnames(about_dat_faq), value = TRUE)
about_dat_faq <- about_dat_faq[, ..keep_vars]


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


# CHD-RESULTS -------------------------------------------------------------

file.path <- paste0("language/outcome_descriptions_chd_", lang, ".rds")
outcome_descriptions_chd <-
  readRDS(file =  file.path)

outcome_choices_chd <- c((outcome_descriptions_chd$ht.code))
names(outcome_choices_chd) <- enc2utf8(outcome_descriptions_chd$name)



file.path <- paste0("language/variable_descriptions_chd_", lang, ".rds")
var_descriptions_chd <-
  readRDS(file =  file.path)

ui_var_choices_chd <- var_descriptions_chd$code_name
names(ui_var_choices_chd) <- enc2utf8(var_descriptions_chd$name)

aggr_levels_chd <- readRDS("data/chd/aggr_levels_chd.rds")

# Set order in which labels will appear on UI
tmp <- c("totals","age", "sex", "age_sex")

# Make sure labels are the same as the data
stopifnot(all(tmp %in% aggr_levels_chd))
aggr_levels_chd <- tmp
rm(tmp)


# CHD ABOUT SECTION -------------------------------------------------------


about_dat_faq_chd <- fread(file = "input_ui_text/faq_chd.csv", encoding = "UTF-8")
keep_vars <- grep(lang, colnames(about_dat_faq_chd), value = TRUE)
about_dat_faq_chd <- about_dat_faq_chd[, ..keep_vars]



