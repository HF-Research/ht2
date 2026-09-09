ui <- function(request) {
  
  div(
    
    useShinyjs(),
    
    extendShinyjs(
      script = "numberFormatter.js",
      functions = "numberFormatter"
    ),
    
    tags$head(
      includeHTML("www/google-analytics.html"),
      
      {
        js_lang <- paste0("www/navAppend-", lang, ".js")
        includeScript(js_lang)
      },
      
      includeScript("www/checkBrowser.js"),
      
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css-new.css"
      )
    ),
    
    tags$a(
      img(
        src = "HF_RGB_svg.svg",
        align = "left",
        style = paste0(
          "padding-top: 5px; ",
          "padding-bottom: 10px; ",
          "padding-left: 2.5rem;"
        ),
        height = "130px"
      ),
      href = "https://hjerteforeningen.dk/",
      target = "_blank"
    ),
    
    fluidPage(
      div(
        style = "padding-left: 0px; padding-right: 0px;",
        
        titlePanel(
          title = "",
          
          windowTitle = tags$head(
            tags$link(
              rel = "icon",
              type = "image/png",
              href = "hf_logo.png"
            ),
            tags$title("HjerteTal")
          )
        )
      )
    ),
    
    navbarPage(
      
      title = HTML(
        paste0(
          '<a id="ht_title" href=',
          shQuote(
            ht_link(lang)
          ),
          '>',
          'HjerteTal',
          '</a>'
        )
      ),
      
      id = "bar",
      
      # IMPORTANT FOR THIS TEST:
      theme = NULL,
      
      selected = "cvd",
      
      collapsible = TRUE,
      
      # CVD
      source(
        file.path(
          "ui",
          "main_ui.R"
        ),
        local = TRUE
      )$value,
      
      # CHD
      source(
        file.path(
          "ui",
          "chd_ui.R"
        ),
        local = TRUE
      )$value,
      
      # Help / FAQ
      navbarMenu(
        
        title = {
          
          methods_label <- "Vejledning/FAQ"
          
          if (lang == "en") {
            methods_label <- "Help/FAQ"
          }
          
          methods_label
        },
        
        # CVD help
        source(
          file.path(
            "ui",
            "about_ui.R"
          ),
          local = TRUE
        )$value,
        
        # CHD help
        source(
          file.path(
            "ui",
            "about_ui_chd.R"
          ),
          local = TRUE
        )$value
      )
    )
  )
}