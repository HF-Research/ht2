capWord <- function(y) {
  
  x <- strsplit(y, " ")
  sapply(x, function(i){
    paste(toupper(substring(i, 1, 1)),
          substring(i, 2),
          sep = "",
          collapse = " ")  
  })
  
}