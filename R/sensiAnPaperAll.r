#' overall sensitivity analysis for the first paper
#' 
#' unlikely to be run in one go
#' but held here as a record of everything done
#' 
#' @param nScenarios number of scenarios 
#' 
#' @examples
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'mixture' )
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'insecticide1' )
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'insecticide2' )
#' #colnames(inputAndResults)
#' @return todo
#' @export
#' 
sensiAnPaperAll <- function( nScenarios = 10 )
{

  
  ## do model runs
  inAndOutMix <- sensiAnPaper1( nscenarios, insecticideUsed = 'mixture' )
  inAndOutI1 <- sensiAnPaper1( nscenarios, insecticideUsed = 'insecticide1' )
  inAndOutI2 <- sensiAnPaper1( nscenarios, insecticideUsed = 'insecticide2' )
  
  ## save results objects as rda ?
  
  
  ## post processing
  sensiPostProc( inAndOutMix = inAndOutMix,
                 inAndOutI1 = inAndOutI1,
                 inAndOutI2 = inAndOutI2 )
  
  
  #return(listOut)
  
}