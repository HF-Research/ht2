ui <- function(request) {
  div(
  
  
  
  tags$head(
    includeHTML("www/google-analytics.html"),
    {
      # Link solution comes from:
      # https://stackoverflow.com/questions/55987238/add-external-hyperlink-to-tabpanel-or-navbarmenu-in-r-shiny
      js_lang <- paste0("www/navAppend-", lang, ".js")
    includeScript(js_lang)
    },
    includeScript("www/checkBrowser.js")
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/css-ht2.css")
    
  ),
  img(
    src = "hf-logo.png",
    align = "left",
    style = "padding-top: 20px; padding-bottom: 40px; padding-left: 2.5rem;",
    height = "110px"
  ),
  fluidPage(
    div(style = "padding-left: 0px; padding-right: 0px;",
        titlePanel(
          title = "", windowTitle = tags$head(
            tags$link(rel = "icon", type = "image/png", href = "hf-icon.png"),
            tags$title("HjerteTal"))
        ))
  ),
  
  
   navbarPage(
    title = "HjerteTal",
    id = "navbar",
    theme = "css-ht2.css",
    selected = "cvd_adult",
    collapsible = TRUE,
    source(file.path("ui", "main_ui.R"), local = TRUE)$value,
    source(file.path("ui", "chd_ui.R"), local = TRUE)$value,
    navbarMenu(title = {
      methods_label <- "Vejledning/FAQ"
      if (lang == "en") {
        methods_label <- "Help/FAQ"
      }
      methods_label
    },
               source(file.path("ui", "about_ui.R"), local = TRUE)$value,
               source(file.path("ui", "about_ui_chd.R"), local = TRUE)$value
               )
    
     
  )
  
)
}