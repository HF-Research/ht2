output$about_title <- renderText("Title")
# DATATABLES --------------------------------------------------------------

aboutDiagDT <- reactive({
  
  col_subset <-
    c(
      "name",
      "desc",
      "code_simple",
      "diag_type",
      "pat_type"
    )
  diag <- about_dat_diag[, ..col_subset]
  colnames(diag) <- col_names_diag
  makeAboutTables(diag, col_names_diag)
  
})

aboutOprDT <- reactive({
  col_subset <-
    c("name",
    "desc",
      "code_simple")
  opr <- about_dat_opr[, ..col_subset]
  colnames(opr) <- col_names_opr
  makeAboutTables(opr, col_names_opr)
})


aboutMedDT <- reactive({
  col_subset <-
    c("name",
      "desc",
      "code_simple")
  med <- about_dat_med[, ..col_subset]
  colnames(med) <- col_names_med
  makeAboutTables(med, col_names_med)
})

aboutEduDT <- reactive({
  col_subset <-
    c(paste0("edu_name_", lang),
      paste0("long_desc_", lang),
      "code_simple")
  edu <- edu[, ..col_subset]
  colnames(edu) <- col_names_edu
   makeAboutTables(edu, col_names_edu)
})


aboutEthnicityDT <- reactive({
  makeAboutTables(country_grps,
                  col_names_ethnicity,
                  order = TRUE,
                  paging = TRUE,
                  search = TRUE,
                  dom = "Bfpt")
})


aboutPopDT <- reactive({
  colnames(pop) <- col_names_pop
  pop_DT <- DT::datatable(
    data = pop,
    extensions = 'Buttons',
    rownames = FALSE,
    class = ' hover row-border',
    selection = c("multiple"),
    options = list(
      lengthMenu = list(c(15, 50, -1), c('15', '50', 'Alle')),
      pageLength = 15,
      dom = "Blftp",
      buttons = list('pdf', 'excel'),
      initComplete = JS(
        # Table hearder background color
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#e7e7e7'});",
        "}"
      )
    )
  )
})


# TEXT --------------------------------------------------------------------
uiAboutText <- reactive({
  
  ui_about_text[code == input$about_selection,]
})
output$ui_about_title <- renderText({
  
  uiAboutText()[, title_text]
})

output$ui_about_desc <- renderText({
  uiAboutText()[, desc_text]
})

output$ui_about_desc_2 <- renderText({
  uiAboutText()[, desc_text_2]
})


# RENDER ------------------------------------------------------------------
output$table_diag <- renderDT({
  aboutDiagDT()
}, server = FALSE)
output$table_opr <- renderDT({
  aboutOprDT()
}, server = FALSE)
output$table_med <- renderDT({
  aboutMedDT()
}, server = FALSE)
output$table_edu <- renderDT({
  aboutEduDT()
}, server = FALSE)
output$table_pop <- renderDT({
  aboutPopDT()
})

output$table_ethnicity <- renderDT({
  aboutEthnicityDT()
})
