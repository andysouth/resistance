#resistance/inst/shiny/simpfitvis/server.r
#andy south 13/7/16

#developed from covmob1/

library(shiny)
#library(devtools)
#install_github('AndySouth/coverage')
library(coverage)
library(resistance)
library(png)

shinyServer(function(input, output, session) {


  ################################
  output$plotA <- renderPlot({

    #add dependency on the button
    #if ( input$aButtonRun > 0 )
    #{
      #isolate reactivity of other objects
    #  isolate({

        plotsimpfit( effectiveness1 = input$effectiveness_A1,
                     dominance = input$dominance_A1,
                     rr_restoration1 = input$advantage_A1 )

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
    
    plotsimpfit( effectiveness1 = input$effectiveness_B1,
                 dominance = input$dominance_B1,
                 rr_restoration1 = input$advantage_B1 )
    
    #}) #end isolate
    #} #end if ( input$aButtonRun > 0 )
  })  


})
