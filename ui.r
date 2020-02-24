ui <- div(
  
  
  
  tags$head(
    includeHTML("www/google-analytics.html"),
    {
      # Link solution comes from:
      # https://stackoverflow.com/questions/55987238/add-external-hyperlink-to-tabpanel-or-navbarmenu-in-r-shiny
      js_lang <- paste0("www/navAppend-", lang, ".js")
    includeScript(js_lang)
    }  
    
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
          title = "", windowTitle = "HjerteTal"
        ))
  ),
  
  
   navbarPage(
    
    
    title = "HjerteTal",
    collapsible = TRUE,
    source(file.path("ui", "main_ui.R"), local = TRUE)$value,
    source(file.path("ui", "chd_ui.R"), local = TRUE)$value,
    navbarMenu(title = "Metoder",
               source(file.path("ui", "about_ui.R"), local = TRUE)$value,
               source(file.path("ui", "about_ui_chd.R"), local = TRUE)$value
               )
    
    
  )
  
)
