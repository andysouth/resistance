#' set inputs and run model for the sensitivity analysis for the first paper
#' 
#' sets input parameter ranges,  
#' draws random numbers for the specified number of scenarios,   
#' runs the model.   
#' needs to be run 3 times, once for each insecticide strategy 'insecticide1','insecticide2', & 'mixture'.
#' Setting the random seed by default means that it can be run for the 3 strategies and will generate the 
#' same inputs for each.
#' 
#' @param nScenarios number of scenarios 
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' @param randomSeed an integer to set seed, NULL for not setting it
#' 
#' @examples
#' inputAndResults <- sensiAnPaperPart( 5, insecticideUsed = 'mixture' )
#' inputAndResults <- sensiAnPaperPart( 5, insecticideUsed = 'insecticide1' )
#' inputAndResults <- sensiAnPaperPart( 5, insecticideUsed = 'insecticide2' )
#' #colnames(inputAndResults)
#' @return a list containing results and inputs
#' @export
#' 
sensiAnPaperPart <- function( nScenarios = 10,
                           insecticideUsed = 'mixture',
                           randomSeed = 1 )
{
  #set random seed
  if (!is.null(randomSeed)) set.seed(randomSeed)
  
  #create empty object to hold input matrix for return
  input <- NULL
  
  for(i in 1:nScenarios)
  {
    
    #there's probably a better way  doing this
    #by generating vectors of samples for each scenario at the same time
    #but this works
    
    max_gen <- 500
    
    #insecticide2 is always rarer than insecticide1 (because P_2 is multiplied by P_1)
    #P_1 <- runif(1, min=0.01, max=0.1)
    #possible bug, i think this should be max=1 rather than max=100
    #P_2 <- runif(1, min=0.01, max=100) * P_1
    #now both set to be independent of each other
    #P_1 <- runif(1, min=0.0001, max=0.1)    
    #P_2 <- runif(1, min=0.0001, max=0.1)
    #now set to log-uniform 
    #select ‘x’ from uniform -1 to -4 and set to 10^x to give equal weight to each log interval.
    P_1 <- 10^-(runif(1, min=1, max=4))
    P_2 <- 10^-(runif(1, min=1, max=4))  
    
    ## exposure to insecticide
    exposure <- runif(1, min=0.1, max=0.9)
    #this sets exposures according to whether insecticide1,2 or mixture
    a <- setExposure(exposure=exposure, insecticideUsed = insecticideUsed) 
  
    
    ## selection against SS in presence of insecticide to which it encodes resistance
    phi.SS1_A0 <- runif(1, min=0.4, max=1)
    phi.SS2_0B <- runif(1, min=0.4, max=1)    
      
    
    ## dominance of resistance
    h.RS1_A0 <- runif(1, min=0, max=1)
    h.RS2_0B <- runif(1, min=0, max=1)    
    #to try to get very different (& some very low, values of dominance)
    #h.RS1_A0 <- 10^-(runif(1, min=0, max=5))
    #h.RS2_0B <- 10^-(runif(1, min=0, max=5))     
    
    ## selective advantage of resistance
    #s.RR1_A0 <- runif(1, min=0.2, max=1)
    #s.RR2_0B <- runif(1, min=0.2, max=1)     
    # Ian suggested this should be dependent on phi to ensure fitness of RR stays below 1 
    s.RR1_A0 <- runif(1, min=0.2, max=1) * phi.SS1_A0
    s.RR2_0B <- runif(1, min=0.2, max=1) * phi.SS2_0B
    
    ## put the generated values into an input matrix, using defaults for non specified parameters
    inputOneScenario <- setInputOneScenario( max_gen = max_gen,
      
                        P_1 = P_1,
                        P_2 = P_2,
                        
                        a=a,
                        
                        phi.SS1_A0 = phi.SS1_A0,
                        phi.SS2_0B = phi.SS2_0B,
                        
                        h.RS1_A0 = h.RS1_A0,
                        h.RS2_0B = h.RS2_0B,
                        
                        s.RR1_A0 = s.RR1_A0,
                        s.RR2_0B = s.RR2_0B
                        )   
    
    
    input <- cbind(input, inputOneScenario)
  }

## run the model for all of the input scenarios    
listOut <- runModel2(input, produce.plots = FALSE) 
  

return(listOut)
  
}