#' overall sensitivity analysis for the first paper
#' 
#' unlikely to be run in one go
#' but held here as a record of everything done
#' 
#' @param nScenarios number of scenarios 
#' 
#' @return todo
#' @export
#' 
sensiAnPaperAll <- function( nScenarios = 10 )
{

  nScenarios <- 1000
  ## do model runs
  inAndOutMix <- sensiAnPaperPart( nScenarios, insecticideUsed = 'mixture' )
  inAndOutI1 <- sensiAnPaperPart( nScenarios, insecticideUsed = 'insecticide1' )
  inAndOutI2 <- sensiAnPaperPart( nScenarios, insecticideUsed = 'insecticide2' )
  
  ## save results objects as rda ?
  
  
  ## post processing
  #sensiPostProc( inAndOutMix = inAndOutMix,
  #               inAndOutI1 = inAndOutI1,
  #               inAndOutI2 = inAndOutI2 )
  
  
  #return(listOut)
  
}