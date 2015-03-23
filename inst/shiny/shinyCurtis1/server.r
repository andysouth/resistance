#resistance/shiny/shinyCurtis1/server.r
#andy south 22/3/15

library(shiny)
library(resistance)


shinyServer(function(input, output) {
  
  
  output$plot <- renderPlot({
    
    #p <- 
      
    resistSimple( input$P_1, input$P_2, input$h.RS1_00, input$h.RS2_00 )
    
    #print(p)
    
  })
  
})