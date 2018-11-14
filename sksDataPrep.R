tools::showNonASCII(sks_codes$tekst)
usethis::use_data(sks_codes, overwrite = TRUE)

data_files <- list.files("C:/git/ht2/data", full.names = TRUE, pattern = ".txt")
x <- lapply(data_files, fread)

remove_nonASCII <- function(x){
  # browser()
    x <- gsub("\u00e6", "ae", x)
    x <- gsub("\u00C6", "Ae", x)
    x <- gsub("\u00f8", "oe", x)
    x <- gsub("\u00D8", "Oe", x)
    x <- gsub("\u00E5", "aa", x)
    x <- gsub("\u00C5", "Aa", x)
    x <- gsub("\u00E9", "e", x)
    x <- gsub("\u00F6", "o", x)
    x <- gsub("\u00B0", " degree", x)
    x <- gsub("\u00A7", " sektion ", x)
    
    return(x)
}

x <- lapply(x, function(d) {
  # browser()
  colnames(d) <- tolower(colnames(d))
  d$tekst <- remove_nonASCII(d$tekst)
  d$tekst <- stringi::stri_trans_general(d$tekst, "latin-ascii")
  d
})

sks_codes <- rbindlist(x)

tools::showNonASCII(sks_codes$tekst)
usethis::use_data(sks_codes, overwrite = TRUE)
sks_codes
