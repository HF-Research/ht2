formatNAValues <- function(lang){
  JS(
  paste0("
  function(data, type) {
  
    if (type !== 'display') return data;
    if (data > 0) return shinyjs.numberFormatter('", lang, "')(data);
    return 'NA';
  }
"
  )
)
}

header_JS <- function(background_color, text_color){
  JS(
  # Table hearder background color
  paste0("function(settings, json) {",
         "$(this.api().table().header()).css({'background-color': '", background_color, "', 'color':'",text_color,"'});",
         "}"
  )
)
}

# https://stackoverflow.com/questions/46694351/r-shiny-datatables-replace-numeric-with-string-and-sort-as-being-less-than-numer
formatSuppressedValues <- function(lang){
  JS(
  paste0("c
aa
  function(data, type) {
  
  if (type !== 'display') return data
  if (data > 0) return shinyjs.numberFormatter('", lang, "')(data)
  return '<4'

  }"
  )
)
}

