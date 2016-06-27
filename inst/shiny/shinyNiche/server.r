#resistance/shiny/shinyNiche/server.r
#andy south 29/2/16

library(shiny)
#install_github('AndySouth/resistance')
library(resistance)


shinyServer(function(input, output) {
 
   
  output$tableNicheF <- renderTable({
    
    #isolate reactivity of other objects
    #isolate({
      a <- setExposure(exposure = input$exposure,
                       insecticideUsed = input$insecticideUsed,
                       male_exposure_prop = input$male_exposure_prop, 
                       correct_mix_deploy = input$correct_mix_deploy)
      
      #to replace 0s with NAs to look better in table
      a[which(a[,,]==0)] <- NA
      
      a["f",,]
    #}) #end isolate 
  })

  output$tableNicheM <- renderTable({
    
    a <- setExposure(exposure = input$exposure,
                     insecticideUsed = input$insecticideUsed,
                     male_exposure_prop = input$male_exposure_prop, 
                     correct_mix_deploy = input$correct_mix_deploy)
    
    #to replace 0s with NAs to look better in table
    a[which(a[,,]==0)] <- NA
    
    a["m",,]
    
  })    
    
  
})