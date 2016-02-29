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
                       maleExposureProp = input$maleExposureProp, 
                       correctMixDeployProp = input$correctMixDeployProp)
      
      #to replace 0s with NAs to look better in table
      a[which(a[,,]==0)] <- NA
      
      a["f",,]
    #}) #end isolate 
  })

  output$tableNicheM <- renderTable({
    
    a <- setExposure(exposure = input$exposure,
                     insecticideUsed = input$insecticideUsed,
                     maleExposureProp = input$maleExposureProp, 
                     correctMixDeployProp = input$correctMixDeployProp)
    
    #to replace 0s with NAs to look better in table
    a[which(a[,,]==0)] <- NA
    
    a["m",,]
    
  })    
    
  
})