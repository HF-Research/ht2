output$about_title_chd <- renderText("Title")



# DATATABLES --------------------------------------------------------------
aboutDiagDTChd <- reactive({
  col_subset <-
    c(
      "name",
      "desc",
      "icd8",
      "icd10",
      "diag_type",
      "pat_type"
    )
  diag <- outcome_descriptions_chd[, ..col_subset]
  colnames(diag) <- col_names_diag_about_chd
  makeAboutTables(diag, col_names_diag_about_chd)
  
})


# TEXT --------------------------------------------------------------------
uiAboutTextChd <- reactive({
  ui_about_text_chd[code == input$about_selection_chd, ]
})
output$ui_about_title_chd <- renderText({
  uiAboutText()[, title_text]
})

output$ui_about_desc_chd <- renderText({
  uiAboutText()[, desc_text]
})

output$ui_about_desc_2_chd <- renderText({
  uiAboutText()[, desc_text_2]
})


# RENDER ------------------------------------------------------------------
output$table_diag_chd <- renderDT({
  
  aboutDiagDTChd()}, server = FALSE)
