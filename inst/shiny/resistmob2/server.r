#resistance/shiny/resistmob2/server.r
#andy south 3/6/16
#unmodified from MixSeqResist1, only changes in UI


library(shiny)
#library(devtools)
#install_github('AndySouth/resistance')
library(resistance)


shinyServer(function(input, output) {
  
  output$plotDefault <- renderPlot({
    
    #isolate reactivity of other objects
    isolate({
      runcurtis_f2( )
    }) #end isolate 
  })    
    
  
  output$plotA <- renderPlot({
    
    #add dependency on the button
    if ( input$aButtonRunA >= 0 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        #cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
        
        runcurtis_f2( max_gen = 500,
                      P_1 = input$frequency_A1, 
                      P_2 = input$frequency_A2,
                      #P_2 = input$P_2*input$P_1, #if doing 2 as a proportion of 1, i need to put a limit of 1 on this
                      h.RS1_A0 = input$dominance_A1, 
                      h.RS2_0B = input$dominance_A2,
                      exposure = input$exposure_A,
                      phi.SS1_A0 = input$effectiveness_A1,
                      phi.SS2_0B = input$effectiveness_A2,
                      # 14/6/16
                      #s.RR1_A0 = input$advantage_A1,
                      #s.RR2_0B = input$advantage_A2,
                      rr_restoration_ins1 = input$advantage_A1,
                      rr_restoration_ins2 = input$advantage_A2,
                      
                      cex.axis = 1,
                      
                      #strategyLabels = c('seq','','adapt','mix2'),
                      
                      addCombinedStrategy = FALSE )
        
                      #correct_mix_deploy = input$correct_mix_deploy,
                      #addCombinedStrategy = input$addCombinedStrategy )
        
        #a hack to output the inputs
        cat("A:\n")
        cat("runcurtis_f2( max_gen=500, ", 
                      "P_1 =",input$frequency_A1,",", 
                      "P_2 =",input$frequency_A2,",", 
                      "h.RS1_A0 =",input$dominance_A1,",", 
                      "h.RS2_0B =",input$dominance_A2,",",
                      "exposure =",input$exposure_A,",",
                      "phi.SS1_A0 =",input$effectiveness_A1,",",
                      "phi.SS2_0B =",input$effectiveness_A2,",",
                      "rr_restoration_ins1 =",input$advantage_A1,",",
                      "rr_restoration_ins2 =",input$advantage_A2,",",
                      "addCombinedStrategy = FALSE,", 
                      "strategyLabels = c('seq','','adapt','mix2')",
                      ")\n" )
        
        #"correct_mix_deploy =",input$correct_mix_deploy,",",
        #"addCombinedStrategy =",input$addCombinedStrategy, 
               
      }) #end isolate  
    } #end button
  })

  output$plotB <- renderPlot({
    
    #add dependency on the button
    if ( input$aButtonRunB >= 1 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        runcurtis_f2( max_gen = 500,
                      P_1 = input$frequency_B1, 
                      P_2 = input$frequency_B2,
                      h.RS1_A0 = input$dominance_B1, 
                      h.RS2_0B = input$dominance_B2,
                      exposure = input$exposure_B,
                      phi.SS1_A0 = input$effectiveness_B1,
                      phi.SS2_0B = input$effectiveness_B2,
                      # 14/6/16
                      #s.RR1_A0 = input$advantage_B1,
                      #s.RR2_0B = input$advantage_B2,
                      rr_restoration_ins1 = input$advantage_B1,
                      rr_restoration_ins2 = input$advantage_B2,
                      cex.axis = 1,
                      
                      #strategyLabels = c('seq','','adapt','mix2'),
                      
                      addCombinedStrategy = FALSE )
        #correct_mix_deploy = input$correct_mix_deploy,
        #addCombinedStrategy = input$addCombinedStrategy )
        
        #a hack to output the inputs
        cat("B:\n")
        cat("runcurtis_f2( max_gen=500, ", 
            "P_1 =",input$frequency_B1,",", 
            "P_2 =",input$frequency_B2,",", 
            "h.RS1_A0 =",input$dominance_B1,",", 
            "h.RS2_0B =",input$dominance_B2,",",
            "exposure =",input$exposure_B,",",
            "phi.SS1_A0 =",input$effectiveness_B1,",",
            "phi.SS2_0B =",input$effectiveness_B2,",",
            "rr_restoration_ins1 =",input$advantage_B1,",",
            "rr_restoration_ins2 =",input$advantage_B2,",",
            "addCombinedStrategy = FALSE,", 
            "strategyLabels = c('seq','','adapt','mix2')",
            ")\n" )
        
      }) #end isolate  
    } #end button
  })  
  
    
})