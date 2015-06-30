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
#' @param inAndOutMix output object for mixture from sensiAnPaperPart()
#' @param inAndOutI1 output object for insecticide1 (commoner and first) from sensiAnPaperPart()
#' @param inAndOutI2 output object for insecticide2 (rarer and secound) from sensiAnPaperPart()
#' 
#' @examples
#' #sensiPostProc()
#' @return todo
#' @export
#' 
sensiPostProc <- function( criticalPoints = c(0.1,0.25,0.5),
                           inAndOutMix,
                           inAndOutI1,
                           inAndOutI2 )
{
  
  #? should I do a loop for each criticalPoint ?
  #? for a start just reference criticalPoint below
  
  
  #1) sequential : time to resistance for each insecticide in isolation
  #inputs : inAndOutI1, inAndOutI2
  #find time to criticalPoint for insecticide1
  #find time to criticalPoint for insecticide2
  #add together
  resistPoints1 <- findResistancePoints(listOutI1, locus=1)
  resistPoints2 <- findResistancePoints(listOutI2, locus=2)  
  
  resistPointsSeq <- resistPoints1 + resistPoints2
  
  
  #2) mixture1 : time to resistance for either insecticide when used in a mixture
  #inputs : inAndOutMix
  #find time to criticalPoint for EITHER insecticide in mixture  
  resistPointsMix1 <- findResistancePoints(listOutMix, locus='either')  
  
  #3) mixture2 : when resistance to one insecticide in the mixture reached, switch to sole use of the 
  #   other until that too reaches the critical point. Record total time.
  # what I actually need to do is start with mixture find the first critical point
  # (need to know which of the insecticides it is)
  # then I need to go to the single run for the other insecticide & starting at 
  # it's current resistance point find out how many more generations to go
  #inputs : inAndOutI1, inAndOutI2, inAndOutMix
  
  
  
  
  
}