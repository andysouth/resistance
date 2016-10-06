#resistance/inst/shiny/simpfitvis/server.r
#andy south 13/7/16

#developed from covmob1/

library(shiny)
#library(devtools)
#install_github('AndySouth/resistance')
library(resistance)
library(png)

#these may be needed: I should really import them to resistance
library(ggrepel)
library(stringr)


shinyServer(function(input, output, session) {


  ################################
  output$plotA <- renderPlot({

    #add dependency on the button
    #if ( input$aButtonRun > 0 )
    #{
      #isolate reactivity of other objects
    #  isolate({

        # plotsimpfit( effectiveness1 = input$effectiveness_A1,
        #              dominance = input$dominance_A1,
        #              rr_restoration1 = input$advantage_A1 )
    
        #trying new way
        fitnessSingleLocus( eff1 = input$effectiveness_A1,
                            eff2 = input$effectiveness_A2,
                            dom1 = input$dominance_A1,
                            dom2 = input$dominance_A2,
                            rr_1 = input$advantage_A1,
                            rr_2 = input$advantage_A2,
                            plot = TRUE)
    

      #}) #end isolate
    #} #end if ( input$aButtonRun > 0 )
  })

  ################################
  output$plotB <- renderPlot({
    
    #add dependency on the button
    #if ( input$aButtonRun > 0 )
    #{
    #isolate reactivity of other objects
    #  isolate({
    
    # plotsimpfit( eff1 = input$effectiveness_A1,
    #              eff2 = input$effectiveness_A2,
    #              dom1 = input$dominance_A1,
    #              dom2 = input$dominance_A2,
    #              rr_1 = input$advantage_A1,
    #              rr_2 = input$advantage_A2 )
    
    #trying new way
    fitnessGenotype( eff1 = input$effectiveness_A1,
                        eff2 = input$effectiveness_A2,
                        dom1 = input$dominance_A1,
                        dom2 = input$dominance_A2,
                        rr_1 = input$advantage_A1,
                        rr_2 = input$advantage_A2,
                        exposure = input$exposure_A,
                        plot = TRUE)
    
    
    #}) #end isolate
    #} #end if ( input$aButtonRun > 0 )
  })  


})
