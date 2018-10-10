mkDiag <-
  function(diag_dat,
           name_dk,
           name_en,
           simple_code,
           grep_string,
           admission_str,
           abulant_str,
           bi_diag_str) {
    # browser()
    diag_dat <- copy(diag_dat)
    diag_dat <- diag_dat[grep(grep_string, kode), ]
    n <- nrow(diag_dat)
    
    data.table(
      diag_dk = c(rep(name_dk, n)),
      diag_en = c(rep(name_en, n)),
      code_simple = c(rep(simple_code, n)),
      code_full = c(diag_dat$kode),
      desc_full = c(diag_dat$tekst),
      admission = c(rep(admission_str, n)),
      ambulant = c(rep(abulant_str, n)),
      bi_diag = c(rep(bi_diag_str, n))
    )
  }