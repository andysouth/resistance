#resistance/shiny/shinyCurtis1/server.r
#andy south 22/3/15

library(shiny)
library(resistance)


shinyServer(function(input, output) {
  
  
  output$plot <- renderPlot({
    

    #cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
    
    resistSimple( P_1 = input$P_1, 
                  P_2 = input$P_2,
                  #P_2 = input$P_2*input$P_1, #if doing 2 as a proportion of 1, i need to put a limit of 1 on this
                  h.RS1_A0 = input$h.RS1_A0, 
                  h.RS2_0B = input$h.RS2_0B,
                  a.m_AB = input$a.m_AB,
                  a.f_AB = input$a.m_AB, #set f to same as m
                  a.m_00 = 1-input$a.m_AB, #set 00 to 1-AB
                  a.f_00 = 1-input$a.m_AB, #set 00 to 1-AB
                  phi.SS1_A0 = input$phi.SS1_A0,
                  phi.SS2_0B = input$phi.SS2_0B,
                  s.RR1_A0 = input$s.RR1_A0,
                  s.RR2_0B = input$s.RR2_0B )
    
    #a hack to output the inputs
    cat("resistSimple( P_1 =",input$P_1,",", 
                  "P_2 =",input$P_2,",", 
                  "h.RS1_A0 =",input$h.RS1_A0,",", 
                  "h.RS2_0B =",input$h.RS2_0B,",",
                  "a.m_AB =",input$a.m_AB,",",
                  "a.f_AB =",input$a.m_AB,",", 
                  "a.m_00 =",1-input$a.m_AB,",", 
                  "a.f_00 =",1-input$a.m_AB,",", 
                  "phi.SS1_A0 =",input$phi.SS1_A0,",",
                  "phi.SS2_0B =",input$phi.SS2_0B,",",
                  "s.RR1_A0 =",input$s.RR1_A0,",",
                  "s.RR2_0B =",input$s.RR2_0B,")\n" )
    

    
  })
  
})