#resistance/shiny/shinyMixSeqCompare/server.r
#andy south 6/7/15
#to view mixtures versus sequential in pre-run sensitivity analysis results

library(shiny)
library(resistance)


## to load previously saved runs, e.g. these that I added 100 onto filename
load(file='listOutMix_100.rda')
load(file='listOutI1_100.rda')
load(file='listOutI2_100.rda')


shinyServer(function(input, output) {
  
  output$plotTest <- renderPlot({
    plot(1:10)
  })
  
  
  output$plot <- renderPlot({
    
    cat("in plot scenario:", input$scenarioNum,"\n")
    
    if (input$scenarioNum > 0)
      plotcurtis_f2_generic( listOutMix$results[[input$scenarioNum]], listOutI2$results[[input$scenarioNum]], listOutI1$results[[input$scenarioNum]] )
    
#     #a hack to output the inputs
#     cat("resistSimple( P_1 =",input$P_1,",", 
#                   "P_2 =",input$P_2,",", 
#                   "h.RS1_A0 =",input$h.RS1_A0,",", 
#                   "h.RS2_0B =",input$h.RS2_0B,",",
#                   "a.m_AB =",input$a.m_AB,",",
#                   "a.f_AB =",input$a.m_AB,",", 
#                   "a.m_00 =",1-input$a.m_AB,",", 
#                   "a.f_00 =",1-input$a.m_AB,",", 
#                   "phi.SS1_A0 =",input$phi.SS1_A0,",",
#                   "phi.SS2_0B =",input$phi.SS2_0B,",",
#                   "s.RR1_A0 =",input$s.RR1_A0,",",
#                   "s.RR2_0B =",input$s.RR2_0B,")\n" )
  })
  
  
  
})