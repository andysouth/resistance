#' set inputs for a sensitivity analysis of multiple scenarios
#' 
#' DEPRECATED not used in analysis for Levick et al 2017. sensiAnPaper1All.Rmd used instead
#' creates a multi-column input matrix which can be passed to runModel() 
#' You need to pass a range for which parameters you want, as c(min,max).
#' Scenarios will be created by selecting from a uniform distribution specified by this range.
#' Any parameters not specified will be set to their default values.
#' TODO it fails if no param ranges are passed i.e. you can't just create a matrix of repeated default scenarios
#' 
#' @param nScenarios number of scenarios to generate 
#' @param ... parameter ranges to run, passed as name=c(min,max)
#' 
#' @examples
#' setInputSensiScenarios( nScenarios=2, h.RS1_A0=c(0,1) )
#' setInputSensiScenarios( nScenarios=3, P_1=c(0,0.5), P_2=c(0,0.5) )
#' setInputSensiScenarios(20, h.RS1_A0=c(0.1,1), h.RS2_0B=c(0.1,1), 
#'                        s.RR1_A0=c(0.2,1), s.RR2_0B=c(0.2,1))
#' @return a matrix of input scenarios
#' @export
#' 
setInputSensiScenarios <- function( nScenarios = 10,
                                    ... )
{
  #create empty object to hold input matrix for return
  input <- NULL

  #todo now can I get just the args that are passed
  #then I will want to select a value for them from a uniform distribution
  #and pass it to setInputOneScenario()
  funcCall <- match.call()
  #missing off first 2, but dangerous can't assume nScenarios will be first or specified
  #passedArgs <- as.list(funcCall)[-c(1:2)]
  #missing first which is func name
  passedArgs <- as.list(funcCall)[-1] 
  #trying to exclude nScenarios
  #names(passedArgs)
  if( "nScenarios" %in% names(passedArgs))
    passedArgs <- passedArgs[-which(names(passedArgs)=="nScenarios")]
  
  #TODO: I might be able to allow parameters to be set to non default values from here too.
  #I would just need to go through the args and find which had a length of 1 rather than 2
  #would need to use eval() to get around call trickiness
  
  for(i in 1:nScenarios)
  {
    #try to select a value for each passed arg from within the passed range
    #todo can probably replace this loop
    #copy so that the list element names are copied
    argVals <- passedArgs
    for(argNum in 1:length(passedArgs))
    { 
      #I might be able to do this more efficiently by passing nScenarios as first arg to runif
      #to generate multiple random samples at once
      #but then how to get them into the input dataframe
      
      #use eval to convert language object of c(0, 1) into something I can use
      argVals[argNum] <- runif(1, min=eval(passedArgs[[argNum]])[1], max=eval(passedArgs[[argNum]])[2])
    }
    
    #will passing argVals like this work ??
    #inputOneScenario <- setInputOneScenario( argVals ) 
    #above doesn't work, below seems to
    inputOneScenario <- do.call(setInputOneScenario, argVals)     
    
    
    input <- cbind(input, inputOneScenario)
  }
  
  #if i wanted this to run the model too
  #listOut <- runModel(input)
  #plotallele.freq.andy(listOut)
  
  return(input)
  
}