#' one of model runs for sensitivity analysis for the first paper
#' 
#' codes in parameters and ranges
#' needs to be run at least 3 times, once for each insecticide regime
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
#' @return todo
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
    #no this counter shouldn't go here because this is just creation of the input file
    #if (i%%100==0) cat("in sensiAnPaperPart() scenario",i,"/",nScenarios,'\n')
    
    
    #* todo : there might be a better way  doing this *
    #* by generating vectors of samples for each scenario at the same time *
    max_gen <- 500
    
    #insecticide2 is always rarer than insecticide1 (because P_2 is multiplied by P_1)
    #P_1 <- runif(1, min=0.01, max=0.1)
    #possible bug, i think this should be max=1 rather than max=100
    #P_2 <- runif(1, min=0.01, max=100) * P_1
    #now both set to be independent of each other
    P_1 <- runif(1, min=0.0001, max=0.1)    
    P_2 <- runif(1, min=0.0001, max=0.1)

    
    #exposure to insecticide
    #exposure array initialise with 0s in loop so that previous values are zeroed
    exposure <- runif(1, min=0.1, max=0.9)
    a <- setExposure(exposure=exposure, insecticideUsed = insecticideUsed) 
  
    #fitness of SS in presence of insecticide to which it encodes resistance
    phi.SS1_A0 <- runif(1, min=0.1, max=0.4)
    phi.SS2_0B <- runif(1, min=0.1, max=0.4)    
      
    #dominance of resistance
#     h.RS1_A0 <- runif(1, min=0, max=1)
#     h.RS2_0B <- runif(1, min=0, max=1)    
    
    #to try to get very different (& some very low, values of dominance)
    h.RS1_A0 <- 10^-(runif(1, min=0, max=5))
    h.RS2_0B <- 10^-(runif(1, min=0, max=5))     
    
    
    #selective advantage
    s.RR1_A0 <- runif(1, min=0.2, max=1)
    s.RR2_0B <- runif(1, min=0.2, max=1)       
    
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
  
listOut <- runModel2(input, produce.plots = FALSE) 
  
#return(input)
return(listOut)
  
}