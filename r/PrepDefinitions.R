library(data.table)
load("data/icdcodes.rda")
load("data/sks_codes.rda")
source(file = "r/helperFunctions.R")


# DIAGNOSES ---------------------------------------------------------------


diag <- fread("data/definitions_diag.csv", encoding = "UTF-8")

diag_list <- lapply(1:nrow(diag), function(i) {
  mkDiag(
    diag_codes = setDT(icdcodes),
    name_dk = diag$name_dk[i],
    name_en = diag$name_en[i],
    simple_code = diag$icd_simple[i],
    grep_string = diag$grep_strings[i],
    admission_str = "ja",
    abulant_str = diag$ambulant[i],
    bi_diag_str = diag$bi_diag[i],
    duration_str = diag$duration[i],
    diag_type_str = diag$diag_type[i],
    pat_type_str = diag$pat_type[i]
  )
})




# PROCEDURES --------------------------------------------------------------

opr <- fread("data/definitions_opr.csv", encoding = "UTF-8")

opr_list <- lapply(1:nrow(opr), function(i) {
  mkOpr(
    dat = sks_codes,
    name_dk = opr$name_dk[i],
    name_en = opr$name_en[i],
    simple_code = opr$icd_simple[i],
    grep_string = opr$grep_strings[i]
  )
})



# MEDICINE ----------------------------------------------------------------

med <- fread("data/definitions_med.csv", encoding = "UTF-8")

med_list <- lapply(1:nrow(med), function(i) {
  mkOpr(
    dat = sks_codes,
    name_dk = med$name_dk[i],
    name_en = med$name_en[i],
    simple_code = med$atc_simple[i],
    grep_string = med$grep_strings[i]
  )
})
