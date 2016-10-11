#resistance/inst/shiny/expovis/server.r


library(shiny)
#library(devtools)
#install_github('AndySouth/resistance')
library(resistance)


shinyServer(function(input, output, session) {


  ################################
  output$plotA <- renderPlot({
    
    setExposure( exp1 = input$exposure_A1,
                 exp2 = input$exposure_A2,
                 insecticideUsed = input$insecticideUsed, 
                 correct_mix_deploy = input$correct_mix_deploy, 
                 male_exposure_prop = input$male_exposure_prop,
                 plot = TRUE)
    
  })  


})
