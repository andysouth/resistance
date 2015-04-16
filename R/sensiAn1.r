#' sensitivity analysis first attempt
#' 
#' 
#' @param nScenarios number of scenarios 
#' @param ... parameter ranges to pass to \code{\link{setInputSensiScenarios}}
#' 
#' @examples
#' sensiAn1( P_1=c(0,0.5) )
#' @return todo
#' @export
#' 
sensiAn1 <- function( nScenarios = 10,
                      ... )
{

  input <- setInputSensiScenarios( nScenarios = nScenarios, ... )
  #input <- setInputSensiScenarios( nScenarios = nScenarios)  

  listOut <- runModel(input)
  plotallele.freq.andy(listOut)
  
  #extract timeToFifty from listOUt
  
  
  #transpose input, add results column extracted from listOut
  
  
  #try a classification tree
  
  #not sure what to return yet
  #invisible(listOut)
  
}