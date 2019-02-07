output$about_title <- renderText("Title")



# DATATABLES --------------------------------------------------------------


  
output$table_diag <- renderDT({diag_DT})
output$table_opr <- renderDT({opr_DT})
output$table_med <- renderDT({med_DT})