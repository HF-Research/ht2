library(data.table)


# HELPER FUNCTIONS --------------------------------------------------------

mkDiag <-
  function(dat = NULL,
           name_dk,
           name_en,
           code_simple,
           grep_str,
           pat_type_str,
           diag_type_str,
           duration_str) {
    # Makes a data.table with the common english and danish names, along with
    # the corresponding SKS code. "dat" is the SKS data
    if (!is.null(dat)) {
      dat <- copy(dat)
      dat <- dat[grep(grep_str, kode), ]
      n <- nrow(dat)
      data.table(
        desc_dk = c(rep(name_dk, n)),
        desc_en = c(rep(name_en, n)),
        code_simple = c(rep(code_simple, n)),
        grep_str = c(rep(grep_str, n)),
        code_full = c(dat$kode),
        desc_full = c(dat$tekst),
        diag_type = c(rep(diag_type_str, n)),
        pat_type = c(rep(pat_type_str, n)),
        duration = c(rep(duration_str, n))
      )
    } else {
      n <- 1
      data.table(
        desc_dk = c(rep(name_dk, n)),
        desc_en = c(rep(name_en, n)),
        code_simple = c(rep(code_simple, n)),
        grep_str = c(rep(grep_str, n)),
        diag_type = c(rep(diag_type_str, n)),
        pat_type = c(rep(pat_type_str, n)),
        duration = c(rep(duration_str, n))
      )
    }
  }

mkTreatment <- function(dat = NULL,
                        name_dk,
                        name_en,
                        code_simple,
                        grep_str) {
  # Makes a data.table with the common english and danish names, along with
  # the corresponding SKS code. "dat" is the SKS data
  if (!is.null(dat)) {
    dat <- copy(dat)
    dat <- dat[grep(grep_str, kode), ]
    n <- nrow(dat)
    
    data.table(
      desc_dk = c(rep(name_dk, n)),
      desc_en = c(rep(name_en, n)),
      code_simple = c(rep(code_simple, n)),
      grep_str = c(rep(grep_str, n)),
      code_full = c(dat$kode),
      desc_full = c(dat$tekst)
    )
  } else {
    n <- 1
    data.table(
      desc_dk = c(rep(name_dk, n)),
      desc_en = c(rep(name_en, n)),
      code_simple = c(rep(code_simple, n)),
      grep_str = c(rep(grep_str, n))
    )
  }
}


# MAKING TABLES -----------------------------------------------------------

# Read in user created definition tables
#dat_files <- list.files(path = "data/", full.names = TRUE)
#code_tables <- lapply(dat_files, fread)
load("tables.rdata")
names(code_tables) <- c("behandling", "diag", "edu", "med")

# Read in sks code datasets
sks <- list()
sks$diag_codes <- fread("path to diag sks")
sks$med_codes <- fread("path to med sks")

k_codes <- fread(input = "data/opr_codes.txt")
u_codes <- fread(input = "data/u_codes.txt")
b_codes <- fread(input = "data/b_codes.txt")
sks$opr_codes <- rbindlist(list(k_codes, u_codes, b_codes))

colnames(sks$diag_codes) <- tolower(sks$diag_codes)
colnames(sks$med_codes) <- tolower(sks$med_codes)
colnames(sks$opr_codes) <- tolower(sks$opr_codes)

diag_list <- lapply(1:nrow(code_tables$diag), function(i) {
  # If there is no SKS dataset to match against, this method of making the diag
  # table is overly complicated (we could just use the csv file). But once the
  # SKS data is available, this will provided the expanded table containing all
  # the sub-codes used.
  # browser()
  mkDiag(dat = sks$diag_codes,
    name_dk = code_tables$diag$diag_name_dk[i],
    name_en = code_tables$diag$diag_names_en[i],
    code_simple = code_tables$diag$codes_simple[i],
    grep_str = code_tables$diag$grep_str[i],
    pat_type_str = code_tables$diag$pat_type_str[i],
    diag_type_str = code_tables$diag$diag_type_str[i],
    duration_str = code_tables$diag$duration_str[i]
  )
})

opr_list <-  lapply(1:nrow(code_tables$behandling), function(i) {
  # If there is no SKS dataset to match against, this method of making the diag
  # table is overly complicated (we could just use the csv file). But once the
  # SKS data is available, this will provided the expanded table containing all
  # the sub-codes used.
  # browser()
  mkTreatment(dat = sks$opr_codes,

    name_dk = code_tables$behandling$opr_name_dk[i],
    name_en = code_tables$behandling$opr_name_en[i],
    code_simple = code_tables$behandling$codes_simple[i],
    grep_str = code_tables$behandling$grep_str[i]
  )
})

med_list <-  lapply(1:nrow(code_tables$med), function(i) {
  # If there is no SKS dataset to match against, this method of making the diag
  # table is overly complicated (we could just use the csv file). But once the
  # SKS data is available, this will provided the expanded table containing all
  # the sub-codes used.
  # browser()
  mkTreatment(
    dat = sks$med_codes,
    name_dk = code_tables$med$med_name_dk[i],
    name_en = code_tables$med$med_name_en[i],
    code_simple = code_tables$med$codes_simple[i],
    grep_str = code_tables$med$grep_str[i]
  )
})
