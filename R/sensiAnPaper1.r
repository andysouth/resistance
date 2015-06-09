#' sensitivity analysis for the first paper
#' 
#' codes in parameters and ranges
#' calls sensiAn1()
#' 
#' 
#' @param nScenarios number of scenarios 
#' @param ... parameter ranges to pass to \code{\link{setInputSensiScenarios}} in form c(min,max)
#' 
#' @examples
#' inputAndResults <- sensiAnPaper1( 5 )
#' #inputAndResults <- sensiAn1( h.RS1_A0=c(0.1,1))
#' #colnames(inputAndResults)
#' @return todo
#' @export
#' 
sensiAnPaper1 <- function( nScenarios = 10,
                      ... )
{
  #create empty object to hold input matrix for return
  input <- NULL
  
  for(i in 1:nScenarios)
  {
    #* todo : there might be a better way  doing this *
    #* by generating vectors of samples for each scenario at the same time *
    P_1 <- runif(1, min=0.01, max=0.1)
    P_2 <- runif(1, min=0.01, max=100) * P_1
    
    #exposure to insecticide
    #*depends on whether this is insecticide1, insecticide2 or mixture
    #*maybe need an extra loop above
    #insecticide1
    #for both m&f
    a[,'A','0'] <- runif(1, min=0.1, max=0.9)
    a[,'0','0'] <- 1 - a[,'A','0']
    
    
    
    
    
    
    setInputOneScenario(P_1 = P_1,
                        P_2 = P_2 )   
    
    
    input <- cbind(input, inputOneScenario)
  }
  
  
  
  
}