output$about_title <- renderText("Title")



# DATATABLES --------------------------------------------------------------


uiAboutText <- reactive({
  ui_about_text[code == input$about_selection, ]
})
output$ui_about_title <- renderText({
  uiAboutText()[, title_text]
})
output$ui_about_desc_2 <- renderText({
  uiAboutText()[, desc_text_2]
})
output$ui_about_desc <- renderText({
  uiAboutText()[, desc_text]
})


output$table_diag <- renderDT({diag_DT})
output$table_opr <- renderDT({opr_DT})
output$table_med <- renderDT({med_DT})