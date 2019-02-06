mkDiag <-
  function(diag_codes,
           name_dk,
           name_en,
           simple_code,
           grep_string,
           admission_str,
           abulant_str,
           bi_diag_str,
           duration_str,
           diag_type_str,
           pat_type_str) {
    
    diag_codes <- copy(diag_codes)
    diag_codes <- diag_codes[grep(grep_string, diag), ]
    n <- nrow(diag_codes)
    
    data.table(
      desc_dk = c(rep(name_dk, n)),
      desc_en = c(rep(name_en, n)),
      code_simple = c(rep(simple_code, n)),
      code_full = c(diag_codes$diag),
      admission = c(rep(admission_str, n)),
      ambulant = c(rep(abulant_str, n)),
      bi_diag = c(rep(bi_diag_str, n)),
      duration = c(rep(duration_str, n)),
      diag_type = c(rep(diag_type_str, n)),
      pat_type = c(rep(pat_type_str, n))
    )
  }


mkOpr <-
  function(dat,
           name_dk,
           name_en,
           simple_code,
           grep_string
  ) {
    # browser()
    dat <- copy(dat)
    dat <- dat[grep(grep_string, kode), ]
    n <- nrow(dat)
    
    data.table(
      desc_dk = c(rep(name_dk, n)),
      desc_en = c(rep(name_en, n)),
      code_simple = c(rep(simple_code, n)),
      code_full = c(dat$kode),
      desc_full = c(dat$tekst)
    )
  }

constructSasWhere <- function(x, sas_var, and_vs_or){
  if(and_vs_or == "or" | and_vs_or == "|"){
    paste0(sas_var, " = '", x, "'", collapse = " or " )
  } else if(and_vs_or == "and" | and_vs_or == "&"){
    paste0(sas_var, " = '", x, "'", collapse = " and " )
  }
}
