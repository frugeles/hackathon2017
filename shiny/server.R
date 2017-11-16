
shinyServer(function(input, output) {
  
  output$NetWork.Out <- renderTable({
    
    DF[network == input$NetWork.In]
  
  })
})



