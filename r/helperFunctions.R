capWord <- function(y) {
  
  x <- strsplit(y, " ")
  sapply(x, function(i){
    paste(toupper(substring(i, 1, 1)),
          substring(i, 2),
          sep = "",
          collapse = " ")  
  })
  
}

ht_link <- function(lang){
  if(lang == "en"){
    "https://hjerteforeningen.shinyapps.io/HjerteTal-en/"
  } else{
    "https://hjerteforeningen.shinyapps.io/HjerteTal/"
  }
}

html_wrap <- function(x, width){
  paste0(strwrap(x, width = width), collapse =  "<br>")
}

html_wrap_v <- function(x, width){
 stringr::str_wrap(x, width) %>% gsub("\n", "<br>", x= .)
}
