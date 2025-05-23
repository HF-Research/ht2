ui <- function(request) {
  div(
    useShinyjs(),
    extendShinyjs(script = "www/numberFormatter.js", functions = "numberFormatter"),
    tags$head(
      includeHTML("www/google-analytics.html"),
      {
        # Link solution comes from:
        # https://stackoverflow.com/questions/55987238/add-external-hyperlink-to-tabpanel-or-navbarmenu-in-r-shiny
        js_lang <- paste0("www/navAppend-", lang, ".js")
        includeScript(js_lang)
      },
      
      # JS code to add link to en/dk version of website
      includeScript("www/checkBrowser.js") 
      
      
    ),
    tags$a(
      img(
      src = "HF_RGB_svg.svg",
      align = "left",
      style = "padding-top: 5px; padding-bottom: 10px; padding-left: 2.5rem;",
      height = "130px"
    ), href = "https://hjerteforeningen.dk/", target ="_blank"),
    fluidPage(
      div(style = "padding-left: 0px; padding-right: 0px;",
          titlePanel(
            title = "",
            windowTitle = tags$head(
              tags$link(rel = "icon", type = "image/png", href = "hf_logo.png"),
              tags$title("HjerteTal")
            )
          ))
    ),
    
    
    navbarPage(
      title = HTML(paste0('<a id="ht_title" href=', shQuote(ht_link(
        lang
      ))), '>', 'HjerteTal', '</a>'),
      id = "bar",
      theme = "css-app-specifc.css",
      selected = "cvd",
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
      source(file.path(
        "ui", "about_ui.R"
      ), local = TRUE)$value,
      source(
        file.path("ui", "about_ui_chd.R"), local = TRUE
      )$value)
      
      
    )
    
  )
}
