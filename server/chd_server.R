output$outcome_description_chd <- renderUI({
  req(input$outcome_chd)
  
  out_title <- tags$b(input$outcome_chd)
  out <-
    outcome_descriptions_chd[hjertetal_code == outcomeCodeChd(), .(desc_dk, link_dk)]
  # Add link for further reading - if link exists, otherwise just desc
  url <- a(ui_read_more,
           href = (out$link_dk),
           target = "_blank")
  
  if (out$link_dk != "na") {
    tagList(out_title, out$desc_dk, url)
  }
  else {
    tagList(out_title, out$desc_dk)
  }
})





outcomeCodeChd <- reactive({
  # Connect the input in normal language to the hjertetal_code. This is so we
  # can change the description without having to rename allll the datasets.
  
  outcome_descriptions_chd[name_dk == input$outcome_chd, ht.code]
})
