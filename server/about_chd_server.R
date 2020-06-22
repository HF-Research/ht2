output$about_title_chd <- renderText("Title")
# DATATABLES --------------------------------------------------------------
aboutFAQDTChd <- reactive({
  colnames(about_dat_faq_chd) <- col_names_faq
  n_col <- NCOL(about_dat_faq_chd)
  makeAboutTables(about_dat_faq_chd, col_names_faq) %>% 
    formatStyle(1:n_col, borderColor = "white") %>% 
    formatStyle(columns = c(1), width='35%')
})



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
  
  makeAboutTables(diag, col_names_diag_about_chd) %>%
    formatStyle(columns = c(2), width='40%') %>% 
    formatStyle(columns = c(3:4), width='20%')
  
})


# TEXT --------------------------------------------------------------------
uiAboutTextChd <- reactive({

  ui_about_text_chd[code == input$about_selection_chd, ]
})
output$ui_about_title_chd <- renderText({
  uiAboutTextChd()[, title_text]
})

output$ui_about_desc_chd <- renderText({
  
  if(input$about_selection_chd == "general"){
    str2 <- paste0("<ui><li>", paste0(ui_bullets_chd, collapse = "</li><li>"), "</ui></li>")
    HTML(paste(ui_gen_1_chd, str2, "</br>", ui_gen_2_chd))
    
  } else {
    HTML(uiAboutTextChd()[, desc_text])
  }
})

output$ui_about_desc_2_chd <- renderText({
  
  uiAboutTextChd()[, desc_text_2]
})


# RENDER ------------------------------------------------------------------
output$table_faq_chd <- renderDT({
  
  aboutFAQDTChd()}, server = FALSE)

output$table_diag_chd <- renderDT({
  
  aboutDiagDTChd()}, server = FALSE)
