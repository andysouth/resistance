#' sensitivity analysis for the first paper
#' 
#' codes in parameters and ranges
#' not sure whether to run all 3 insecticide scenarios
#' from within the function, or have to run it 3 times
#' 
#' @param nScenarios number of scenarios 
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' 
#' @examples
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'mixture' )
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'insecticide1' )
#' inputAndResults <- sensiAnPaper1( 5, insecticideUsed = 'insecticide2' )
#' #colnames(inputAndResults)
#' @return todo
#' @export
#' 
sensiAnPaper1 <- function( nScenarios = 10,
                           insecticideUsed = 'mixture')
{
  #create empty object to hold input matrix for return
  input <- NULL
  
  for(i in 1:nScenarios)
  {
    #* todo : there might be a better way  doing this *
    #* by generating vectors of samples for each scenario at the same time *
    max_gen <- 500
    
    P_1 <- runif(1, min=0.01, max=0.1)
    P_2 <- runif(1, min=0.01, max=100) * P_1
    
    #exposure to insecticide
    #**todo should I create an exposure array here, initialise it with 0s
    #and then add an option to setInputOneScenario() to accept the array
    #to minimise complications with setting mf & ensuring exposures sum to 1
    #needs to be created in loop so that previous values are zeroed
    a <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
    #*depends on whether this is insecticide1, insecticide2 or mixture
    #*maybe need an extra loop above
    if (insecticideUsed == "insecticide1")
    {
      #for both m&f
      a[,'A','0'] <- runif(1, min=0.1, max=0.9)
      a[,'0','0'] <- 1 - a[,'A','0']      
    } else if (insecticideUsed == "insecticide2")
    {
      #for both m&f
      a[,'0','B'] <- runif(1, min=0.1, max=0.9)
      a[,'0','0'] <- 1 - a[,'0','B']        
    } else if (insecticideUsed == "mixture")
    {
      #for both m&f
      a[,'A','B'] <- runif(1, min=0.1, max=0.9)
      a[,'0','0'] <- 1 - a[,'A','B']         
    } else
    {
      stop("insecticideUsed neds to be one of (insecticide1, insecticide2, mixture) it is ",insecticideUsed)
    }

  
    #fitness of SS in presence of insecticide to which it encodes resistance
    phi.SS1_A0 <- runif(1, min=0.1, max=0.4)
    phi.SS2_0B <- runif(1, min=0.1, max=0.4)    
      
    #dominance of resistance
    h.RS1_A0 <- runif(1, min=0, max=1)
    h.RS2_0B <- runif(1, min=0, max=1)    
    
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
  
return(input)
#return(listOut)
  
}