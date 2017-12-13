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
#' @param experiment effects which inputs to change, 
#'    'extended' for adding reduced male exposure & incorrect deploymnet 
#' @param old_insecticide if set TRUE then freq of I2 is set to 0.5 to represent an existing insecticide that resistance has already developed
#' 
#' @examples
#' listOutMix <- sensiAnPaperPart( 5, insecticideUsed = 'mixture' )
#' listOutI1 <- sensiAnPaperPart( 5, insecticideUsed = 'insecticide1' )
#' listOutI2 <- sensiAnPaperPart( 5, insecticideUsed = 'insecticide2' )
#' plotcurtis_f2_generic( listOutMix$results[[1]], listOutI2$results[[1]], listOutI1$results[[1]] )
#' #colnames(inputAndResults)
#' @return a list containing results and inputs
#' @export
#' 
sensiAnPaperPart <- function( nScenarios = 10,
                           insecticideUsed = 'mixture',
                           randomSeed = 1,
                           experiment = "curtis1",
                           old_insecticide = FALSE )
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
    
    #independent
    #P_1 <- runif(1, min=0.0001, max=0.1)    
    #P_2 <- runif(1, min=0.0001, max=0.1)
    #now set to log-uniform 
    #select ‘x’ from uniform -1 to -4 and set to 10^x to give equal weight to each log interval.
    P_1 <- 10^-(runif(1, min=1, max=4))
    P_2 <- 10^-(runif(1, min=1, max=4))  
    
    #23/3/16 set freq high in this expt
    if (old_insecticide) P_2 <- 0.5
    
    ## exposure to insecticide
    exposure <- runif(1, min=0.1, max=0.9)
    
    
    ## aha! BEWARE could this mess up the random seed
    ## probably not because it is set to 'extended'
    ## for all 3 runs
    
    if (experiment=='extended')
    {
      male_exposure_prop <- runif(1, min=0, max=1)
      correct_mix_deploy <- runif(1, min=0.5, max=1)          
    } else
    {
      male_exposure_prop <- 1
      correct_mix_deploy <- 1     
    }
    
    
    #this sets exposures according to whether insecticide1,2 or mixture
    a <- setExposure(exposure=exposure, insecticideUsed=insecticideUsed,
                     male_exposure_prop=male_exposure_prop, correct_mix_deploy=correct_mix_deploy) 
  
    
    ## effectiveness of insecticides, selection against SS in presence of that insecticide
    # phi.SS1_A0 <- runif(1, min=0.45, max=1)
    # phi.SS2_0B <- runif(1, min=0.45, max=1)
    # 14/6/16 increased range now that we are using rr_restoration to avoid fitness > 1
    phi.SS1_A0 <- runif(1, min=0.3, max=1)
    phi.SS2_0B <- runif(1, min=0.3, max=1)      
    
    ## dominance of resistance
    h.RS1_A0 <- runif(1, min=0, max=1)
    h.RS2_0B <- runif(1, min=0, max=1)    
    #to try to get very different (& some very low, values of dominance)
    #h.RS1_A0 <- 10^-(runif(1, min=0, max=5))
    #h.RS2_0B <- 10^-(runif(1, min=0, max=5))     
    
    ## selective advantage of resistance
    # Ian suggested this should be dependent on phi to ensure fitness of RR stays below 1 
    # 5/2/16 i'm concerned this adds in a correlation between inputs that might fudge our ability to see what's going on
    #s.RR1_A0 <- runif(1, min=0.2, max=1) * phi.SS1_A0
    #s.RR2_0B <- runif(1, min=0.2, max=1) * phi.SS2_0B
    # 14/6/16 now replaced with below
    # s.RR1_A0 <- runif(1, min=0.1, max=0.45)
    # s.RR2_0B <- runif(1, min=0.1, max=0.45)     
    
    # 14/6/16 to add rr_restoration_ins1 & I2
    # as an alternative to varying s directly from sensi analysis
    # to allow a wider range (higher values) while avoiding correlation between 
    # effectiveness and advantage inputs
    rr_restoration_ins1 <- runif(1, min=0.2, max=1)
    rr_restoration_ins2 <- runif(1, min=0.2, max=1)
    # now have moved s calculation into setInputOneScenario()
    # s.RR1_A0 <- rr_restoration_ins1 * phi.SS1_A0
    # s.RR2_0B <- rr_restoration_ins2 * phi.SS2_0B
    
    ## put the generated values into an input matrix, using defaults for non specified parameters
    inputOneScenario <- setInputOneScenario( max_gen = max_gen,
      
                        P_1 = P_1,
                        P_2 = P_2,
                        
                        a=a,
                        exposure=exposure, 
                        
                        phi.SS1_A0 = phi.SS1_A0,
                        phi.SS2_0B = phi.SS2_0B,
                        
                        h.RS1_A0 = h.RS1_A0,
                        h.RS2_0B = h.RS2_0B,
                        
                        # 14/6/16 now these calc from rr_ inside the func
                        # s.RR1_A0 = s.RR1_A0,
                        # s.RR2_0B = s.RR2_0B,
                        male_exposure_prop = male_exposure_prop,
                        correct_mix_deploy = correct_mix_deploy,
                        # 14/6/16
                        rr_restoration_ins1 = rr_restoration_ins1,
                        rr_restoration_ins2 = rr_restoration_ins2
                        )   
    
    
    input <- cbind(input, inputOneScenario)
  }

## run the model for all of the input scenarios    
listOut <- runModel2(input, produce.plots = FALSE) 
  

return(listOut)
  
}