#resistance/shiny/shinyCurtis1/server.r
#andy south 22/3/15

library(shiny)
library(resistance)


shinyServer(function(input, output) {
  
  
  output$plot <- renderPlot({
    

    cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
    
    resistSimple( P_1 = input$P_1, 
                  P_2 = input$P_2*input$P_1, #i need to put a limit of 1 on this
                  h.RS1_00 = input$h.RS1_00, 
                  h.RS2_00 = input$h.RS2_00,
                  phi.SS1_A0 = input$phi.SS1_A0,
                  phi.SS2_0B = input$phi.SS2_0B,
                  s.RR1_A0 = input$s.RR1_A0,
                  s.RR2_0B = input$s.RR2_0B )

    
  })
  
})