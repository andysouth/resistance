#resistance/shiny/shinyFig2Curtis/server.r
#andy south 8/7/15

library(shiny)
#install_github('AndySouth/resistance')
library(resistance)


shinyServer(function(input, output) {
  
  output$plotDefault <- renderPlot({
    
    #isolate reactivity of other objects
    isolate({
      runcurtis_f2()
    }) #end isolate 
  })    
    
  
  output$plot <- renderPlot({
    
    #add dependency on the button
    if ( input$aButtonRun > 0 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        #cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
        
        runcurtis_f2( P_1 = input$P_1, 
                      P_2 = input$P_2,
                      #P_2 = input$P_2*input$P_1, #if doing 2 as a proportion of 1, i need to put a limit of 1 on this
                      h.RS1_A0 = input$h.RS1_A0, 
                      h.RS2_0B = input$h.RS2_0B,
                      exposure = input$exposure,
                      phi.SS1_A0 = input$phi.SS1_A0,
                      phi.SS2_0B = input$phi.SS2_0B,
                      s.RR1_A0 = input$s.RR1_A0,
                      s.RR2_0B = input$s.RR2_0B )
        
        #a hack to output the inputs
        cat("runcurtis_f2( P_1 =",input$P_1,",", 
                      "P_2 =",input$P_2,",", 
                      "h.RS1_A0 =",input$h.RS1_A0,",", 
                      "h.RS2_0B =",input$h.RS2_0B,",",
                      "exposure =",input$exposure,",",
                      "phi.SS1_A0 =",input$phi.SS1_A0,",",
                      "phi.SS2_0B =",input$phi.SS2_0B,",",
                      "s.RR1_A0 =",input$s.RR1_A0,",",
                      "s.RR2_0B =",input$s.RR2_0B,")\n" )
        
      }) #end isolate  
    } #end if ( input$aButtonRun > 0 ) 
  })
  
})