#' post-processing for sensitivity analysis for the first paper
#' 
#' takes output files for sensitivity analysis runs for insecticide1, insecticide2 and mixture
#' uses these to calculate time to resistance critical points (which can be passed as an argument)
#' for 3 scenarios of insecticide use :   
#' 1) sequential : time to resistance for each insecticide in isolation
#' 2) mixture1 : time to resistance for either insecticide when used in a mixture
#' 3) mixture2 : when resistance to one insecticide in th mixture reached, switch to sole use of the 
#' other until that too reaches the critical point. Record total time.
#' 
#' @param criticalPoints vector of resistance critical points where insecticide is deemed no longer useful
#' #@param proabably need to give it names for results files/objects
#' 
#' @examples
#' #sensiPostProc()
#' @return todo
#' @export
#' 
sensiPostProc <- function( criticalPoints = c(0.1,0.25,0.5) )
{
  #1) sequential : time to resistance for each insecticide in isolation
  
  
  #2) mixture1 : time to resistance for either insecticide when used in a mixture
  
  
  #3) mixture2 : when resistance to one insecticide in th mixture reached, switch to sole use of the 
  #   other until that too reaches the critical point. Record total time.
  
  
}