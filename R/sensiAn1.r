#' sensitivity analysis first attempt
#' 
#' 
#' @param nScenarios number of scenarios 
#' @param ... parameter ranges to pass to \code{\link{setInputSensiScenarios}} in form c(min,max)
#' 
#' @examples
#' sensiAn1( P_1=c(0,0.5) )
#' sensiAn1( h.RS1_A0=c(0.1,1))
#' @return todo
#' @export
#' 
sensiAn1 <- function( nScenarios = 10,
                      ... )
{

  input <- setInputSensiScenarios( nScenarios = nScenarios, ... )
  #input <- setInputSensiScenarios( nScenarios = nScenarios)  

  #run the model using input which sets num scenarios
  #suppress plotting to speed
  listOut <- runModel(input, produce.plots = FALSE)
  
  #plot all scenario results on top of each other
  plotallele.freq.andy(listOut)
  
  #extract timetoFifty from listOUt
  #todo replace with my own func
  #note this is currently locus1
  ttf <- timetoFifty(locus=1, max_gen=100, listOut$results, input)
  
  #paste on to input
  inputAndResults <- rbind(input,ttf)
  
  #transpose input, add results column extracted from listOut
  inputAndResults <- t(inputAndResults)
  
  #try a classification tree
  #but may need to simplify outcome to 0/1 ??
  #this from rpart vignette
  #library(rpart)
  #creates a factor for response var to improve labelling
  #outcome <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))
  #fit the model by adding the columns
  #**me can I specify the predictors by names(match.call())[-1]**
  #cfit <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy, data = stagec, method = 'class')
  #plot(cfit) #plots tree
  #text(cfit) #labels tree
  #*check how this relates to susanas method
  
  #not sure what to return yet
  invisible(inputAndResults)
  
}