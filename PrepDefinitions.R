library(data.table)
library(magrittr)
source(file = "helperFunctions.R")


# DIAGNOSES ---------------------------------------------------------------

diag_codes <- fread(input = "data/d_codes.txt")
colnames(diag_codes) <- tolower(colnames(diag_codes))

syg_names_dk <-
  c(
    "Alle hjertekarsygdomme (inkl. Claudicatio/kritisk
iskæmi)",
    "Blodprop i benets vener (DVT)",
    "Blodprop i hjertet (AMI)",
    "Blodprop i lungerne",
    "Claudication/kritisk iskæmi",
    "Forkammerflimren",
    "Hjerteklapsygdom",
    "Hjertesvigt",
    "Iskæmisk hjertesygdom (IHD)",
    "Karsygdomme i hjernen (apopleksi)",
    "Stabile hjertekramper (angina pectoris)",
    "Udposning på aorta, abdomen (aortaaneurisme)",
    "Ustabile hjertekramper (angina pectoris) + NSTEMI"
  )

syg_names_en <-
  c(
    "All cardiovscular diseases (incl. claudication/critical ischemia)",
    "Blood clot in legs veins (DVT)",
    "Blood clot in the heart (AMI)",
    "Blood clot in the lungs",
    "Claudication/critical ischemia",
    "Atrial fibrillation",
    "Valvular heart I26disease",
    "Heart failure",
    "Ischemic heart disease",
    "Cerebrovascular diseases (apoplexy)",
    "Stabile angina (angina pectoris)",
    "Abdominal aortic aneurysm",
    "Unstable angina (angina pectoris) + NSTEMI"
  )

simple_codes <-
  c(
    "I00 - I99, E10.5, E11.1, E14.5, L97.9",
    "I80",
    "I21 - I22",
    "I26",
    "E10.5, E11.5, E14.5, I70.2 (without I70.2B), I73.9A+B+C, I74.3, L97.9",
    "I48",
    "I05 - I08, I34 - I37",
    "I11.0, I13.0, I13.2, I42.0, I42.6, I42.7, I42.9, I50.0, I50.1, I50.9",
    "I20 - I25",
    "I60 - I69",
    "I20.9, I25.1",
    "I71.3, I71.4",
    "I20.0, I21.4, I21.9"
  )


grep_strings <-
  c(
    "DI[0-9][0-9]|DE105|DE115|DE111|DE145|DL979",
    "DI80",
    "DI2[12]",
    "DI26",
    "DE105|DE115|DE145|DI702A|DI702$|DI739[ABC]|DI743|DL979",
    "DI48",
    "DI0[5-8]|DI3[4-7]",
    "DI110|DI130|DI132|DI420|DI426|DI427|DI429|DI501|DI509",
    "DI2[0-5]",
    "DI6[0-9]",
    "DI209|DI251",
    "DI713|DI714",
    "DI200|DI214|DI219"
  )
diag_codes[grep(grep_strings[1], kode), ]
ambulant <- c("NA", "ja", "nej", rep("ja", 9), "nej")
bi_diag <- c("NA", rep("nej", 4), "ja", "nej", "ja", rep("nej", 5))
duration_str <- c("NA", rep("None", 3), rep("Lifelong", 5), "TODO", "None", "Lifelong", "None")
diag_type_str <- c("NA", rep("A", 4), "A | B", "A", "A | B", rep("A", 5))
pat_type_str <- c("NA", "0 | 1 | 2", "0 | 1", rep("0 | 1 | 2", 9), "0 | 1")

syg_names <-
  c(
    "all",
    "clot_leg",
    "clot_heart",
    "clot_lung",
    "caludation",
    "af",
    "valv",
    "hf",
    "ihd",
    "apop",
    "stab_angina",
    "adbom_ao_aneur",
    "unstable_angina"
  )

diag_list <- lapply(1:length(syg_names), function(i) {
  mkDiag(
    diag_dat = diag_codes,
    name_dk = syg_names_dk[i],
    name_en = syg_names_en[i],
    simple_code = simple_codes[i],
    grep_string = grep_strings[i],
    admission_str = "ja",
    abulant_str = ambulant[i],
    bi_diag_str = bi_diag[i],
    duration_str = duration_str[i],
    diag_type_str = diag_type_str[i],
    pat_type_str = pat_type_str[i]
  )
})
names(diag_list) <- syg_names



# PROCEDURES --------------------------------------------------------------
opr_codes <- fread(input = "data/k_codes.txt")
colnames(opr_codes) <- tolower(colnames(opr_codes))

u_codes <- fread(input = "data/u_codes.txt")
colnames(u_codes) <- tolower(colnames(u_codes))

b_codes <- fread(input = "data/b_codes.txt")
colnames(b_codes) <- tolower(colnames(b_codes))

proc_codes <- rbindlist(list(opr_codes, u_codes, b_codes))

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
    dat = proc_codes,
    name_dk = opr_dk[i],
    name_en = opr_en[i],
    simple_code = simple_codes[i],
    grep_string = grep_strings[i]
  )
})



# MEDICINE ----------------------------------------------------------------

med <- list()
med$med_codes <- fread(input = "data/m_codes.txt")
colnames(med$med_codes) <- tolower(colnames(med$med_codes))


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
    dat = med$med_codes,
    name_dk = med$med_dk[i],
    name_en = med$med_en[i],
    simple_code = med$simple_codes[i],
    grep_string = med$grep_strings[i]
  )
})

