#' NOT WORKING, abandoned
#' was the start of an attempt to run a sensitivity analysis varying just one param at a time
#' this modifies just one var at a time to allow plot generation
#' 
#' sets input parameter ranges, not random
#' runs the model.   
#' needs to be run 3 times, once for each insecticide strategy 'insecticide1','insecticide2', & 'mixture'.
#' 
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' @param nSamples number of samples
#' 
#' @examples
#' inputAndResultsMix <- sensiAnPaperPartOneAtATime( insecticideUsed = 'mixture' )
#' inputAndResultsI1 <- sensiAnPaperPartOneAtATime( insecticideUsed = 'insecticide1' )
#' inputAndResultsI2 <- sensiAnPaperPartOneAtATime( insecticideUsed = 'insecticide2' )
#' #colnames(inputAndResults)
#' @return a list containing results and inputs
#' @export
#' 
sensiAnPaperPartOneAtATime <- function( insecticideUsed = 'mixture', nSamples )
{
  #set random seed
  #if (!is.null(randomSeed)) set.seed(randomSeed)
  
  #create empty object to hold input matrix for return
  input <- NULL

  max_gen <- 500  
    
  #set vectors of parameter ranges
  #and average value (to be used while other params changed)
  
  #TODO I need to think more carefully about how to do this
  #and maybe lookup to see if I can steal any existing sensi analysis type code
  #and look back at my existing notes
  
  
  v_P_1 <- 10^-(c(1,1.5,2,2.5,3,3.5,4))
  v_P_2 <- 10^-(c(1,1.5,2,2.5,3,3.5,4)) 
  av_P_1 <- 10^-(2)
  av_P_2 <- 10^-(2) 
  
  ## exposure to insecticide
  v_exposure <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  av_exposure <- 0.5  
  
  ## selection against SS in presence of insecticide to which it encodes resistance
  v_phi.SS1_A0 <- c(0.4,0.5,0.6,0.7,0.8,0.9,1)
  v_phi.SS2_0B <- c(0.4,0.5,0.6,0.7,0.8,0.9,1)
  av_phi.SS1_A0 <- 0.7
  av_phi.SS2_0B <- 0.7  
  
  ## dominance of resistance
  v_h.RS1_A0 <- c(0,0.2,0.4,0.6,0.8,1)
  v_h.RS2_0B <- c(0,0.2,0.4,0.6,0.8,1) 
  av_h.RS1_A0 <- 0.6
  av_h.RS2_0B <- 0.6   
  
  ## selective advantage of resistance
  # Ian suggested this should be dependent on phi to ensure fitness of RR stays below 1 
  #THAT makes it trickier to deal with
  #TODO work out how to do this ???
  v_s.RR1_A0 <- runif(1, min=0.2, max=1) * phi.SS1_A0
  v_s.RR2_0B <- runif(1, min=0.2, max=1) * phi.SS2_0B
  
  for(i in 1:nSamples)
  {
    
    
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
    inputOneScenario <- setInputOneScenario( max_gen=max_gen, P_1=P_1, P_2=P_2, a=a, phi.SS1_A0=phi.SS1_A0, phi.SS2_0B=phi.SS2_0B,
                                             h.RS1_A0=h.RS1_A0, h.RS2_0B=h.RS2_0B, s.RR1_A0=s.RR1_A0 ,s.RR2_0B=s.RR2_0B )   
    
    
    input <- cbind(input, inputOneScenario)
  }

## run the model for all of the input scenarios    
listOut <- runModel2(input, produce.plots = FALSE) 
  

return(listOut)
  
}