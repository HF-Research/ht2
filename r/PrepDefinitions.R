library(data.table)
library(magrittr)
library(heaven)
source(file = "r/helperFunctions.R")


# DIAGNOSES ---------------------------------------------------------------


diag <- fread("data/definitions_diag.csv", encoding = "UTF-8")

diag_list <- lapply(1:nrow(diag), function(i) {
  mkDiag(
    diag_codes = setDT(heaven::icdcodes),
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



opr_dk <-
  c(
    "Ballonudvidelse (PCI)",
    "Bypassoperation (CABG)",
    "Karkirurgiske indgreb",
    "Røntgenundersøgelse af hjertets kranspulsårer (KAG)",
    "Hjerteklapoperation",
    "Pacemakeroperation",
    "ICD-operation",
    "Ablation"
  )
opr_en <-
  c(
    "Balloon angioplasty",
    "Bypass surgery",
    "vascular surgical interventions",
    "X-ray examination of the coronary arteries",
    "Heart valve surgery",
    "Pace maker operation",
    "ICD operation",
    "Ablation"
  )

simple_codes <-
  c(
    "KFNG00,KFNG02,KFNG02A, KFNG05, KFNG05A,
    KFNG10, KFNG12, KFNG20, KFNG22, 
    KFNG30, KFNG40, KFNG96,KZFX01",
    "KFNA, KFNB, KFNC, KFND, KFNE,
    KFNF, KFNH, KFNJ, KFNK, KFNW",
    "KPDE, KPDF, KPDG, KPDH, KPDN,
    KPDP, KPEE, KPEF, KPEH, KPEN, 
    KPEP, KPEQ, KPET, KPFE, KPFH, 
    KPFN, KPFP, KPFQ, KPFT, KPGH, KPGU",
    "UXAC40, UXAC85, UXAC85A, UXAC85B, UXAC85C, UXAC85D, UXAC90,
    UXUC85, UXUC86, UXUC87",
    "KFJE, KFJF, KFG, KFK, KFM",
    "BFCA",
    "BFCB",
    "BFFB"
  )

grep_strings <-
  c(
    "KFNG00|KFNG02|KFNG02A|KFNG05|KFNG05A|KFNG10|KFNG12|KFNG20|KFNG22|KFNG30|KFNG40|KFNG96|KZFX01",
    "KFNA|KFNB|KFNC|KFND|KFNE|KFNF|KFNH|KFNJ|KFNK|KFNW",
    "KPDE|KPDF|KPDG|KPDH|KPDN|KPDP|KPEE|KPEF|KPEH|KPEN|KPEP|KPEQ|KPET|KPFE|KPFH|KPFN|KPFP|KPFQ|KPFT|KPGH|KPGU",
    "UXAC40|UXAC85|UXAC85A|UXAC85B|UXAC85C|UXAC85D|UXAC90|UXUC85|UXUC86|UXUC87",
    "KFJE|KFJF|KFG|KFK|KFM",
    "BFCA",
    "BFCB",
    "BFFB"
  )

opr_names <- c(
  "ballon",
  "bypass",
  "vasulcar",
  "xray",
  "valve_opr",
  "pacemaker",
  "icd_opr",
  "ablation"
)

opr_list <- lapply(1:length(opr_names), function(i) {
  mkOpr(
    dat = heaven::sks_codes,
    name_dk = opr_dk[i],
    name_en = opr_en[i],
    simple_code = simple_codes[i],
    grep_string = grep_strings[i]
  )
})



# MEDICINE ----------------------------------------------------------------

med <- list()




med$med_dk <-
  c("Antiarytmisk",
    "Antitrombotisk",
    "Blodtrykssænkene",
    "Kloesterolsænkende")
med$med_en <-
  c(
    "Anti-arrhythmic",
    "Anti-thrombotic",
    "Blood-pressure lowering",
    "cholesterol lowering"
  )

med$simple_codes <-
  c("C01, C05",
    "B01",
    "C02, C03, C04, C07, C08, C09",
    "C10")

med$grep_strings <- c("C01|C05","B01","C02|C03|C04|C07|C08|C09", "C10")

med$med_names <- c("antiarrhythmic","antithrombotic","bp","cholesterol")

med_list <- lapply(1:length(med$med_names), function(i) {
  mkOpr(
    dat = heaven::sks_codes,
    name_dk = med$med_dk[i],
    name_en = med$med_en[i],
    simple_code = med$simple_codes[i],
    grep_string = med$grep_strings[i]
  )
})

