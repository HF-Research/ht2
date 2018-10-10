library(data.table)
library(magrittr)
source(file = "helperFunctions.R")


# DIAGNOSES ---------------------------------------------------------------

diag_codes <- fread(input = "data/diag_codes.txt")
colnames(diag_codes) <- tolower(colnames(diag_codes))

syg_names_dk <-
  c(
    "Alle hjertekarsygdomme",
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
    "All cardiovscular diseases",
    "Blood clot in legs veins (DVT)",
    "Blood clot in the heart (AMI)",
    "Blood clot in the lungs",
    "Claudication/critical ischemia",
    "Atrial fibrillation",
    "Valvular heart disease",
    "Heart failure",
    "Ischemic heart disease",
    "Cerebrovascular diseases (apoplexy)",
    "Stabile angina (angina pectoris)",
    "Abdominal aortic aneurysm",
    "Unstable angina (angina pectoris) + NSTEMI"
  )

simple_codes <-
  c(
    "I00 - I99",
    "I80",
    "I21 - I22",
    "I26",
    "E10.5, E11.5, E14.5, I70.2A, I70.2, I73.9A+B+C, I74.3, L97.9",
    "I48",
    "I05 - I08, I34 - I37",
    "I11.0, I13.0, I13.2, I42.0, I42.6, I42.7, I42.9, I50.0, I50.1, I50.9",
    "I20 - I25",
    "I60 - I69",
    "I20.0, I25.1",
    "I71.3, I71.4",
    "I20.0, I21.4, I21.9"
  )


grep_strings <-
  c(
    "DI[0-9][0-9]",
    "DI80",
    "DI2[12]",
    "DI26",
    "DE105|DE115|DE145|DI702A|DI702|DI739[ABC]|DI743|DL979",
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

syg_names <-
  c(
    "all",
    "clot-leg",
    "clot-heart",
    "clot-lung",
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
    bi_diag_str = bi_diag[i]
  )
})
names(diag_list) <- syg_names

diag_pretty <- lapply(diag_list, function(i) i[1, .(diag_dk, diag_en, code_simple)]) %>%
  rbindlist()


# PROCEDURES --------------------------------------------------------------

proc_dk <-
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
proc_en <-
  c(
    "balloon angioplasty",
    "Bypass surgery",
    "vascular surgical interventions",
    "X-ray examination of the coronary arteries",
    "Heart valve surgery",
    "Pace maker operation",
    "ICD operation",
    "Ablation"
  )
