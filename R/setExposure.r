#' set exposure to insecticide
#' 
#' fills an array of exposure values, according to whether insecticide 1,2 or mixture are being used
#' initially assumes same for M&F could change later
#' also assumes exposure is the same to each insecticide which can also be changed in future
#' 
#' NOTE that currently this just works for 'no' & 'hi' exposure, it can't fill in 'lo' exposure
#' 
#' @param exposure exposure to the insecticide(s) if set same for both
#' @param exp1 option to set exposure differently for the diff insecticides
#' @param exp2 option to set exposure differently for the diff insecticides
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' @param male_exposure_prop proportion tht males are exposed relative to f, default 1, likely to be <1
#' @param correct_mix_deploy proportion of times that mixture is deployed correctly, 
#'    assumes that when not deployed correctly the single insecticides are used instead
#' @param plot whether to plot exposure    
#' 
#' @examples
#' a <- setExposure( exposure=0.9, insecticideUsed = 'mixture' )
#' a <- setExposure( exposure=0.9,  insecticideUsed = 'insecticide1' )
#' 
#' #allowing exposures to be set separately
#' as.data.frame(setExposure(exp1=0.5, exp2=0.6))
#' 
#' @return array of exposure values for the different insecticides
#' @export
#' 
setExposure <- function( exposure = 0.9,
                         exp1 = NULL,
                         exp2 = NULL,
                         insecticideUsed = 'mixture',
                         male_exposure_prop = 1,
                         correct_mix_deploy = 1,
                         plot = TRUE) #set to FALSE after testing
{

  #exposure to insecticide
  #exposure array initialise with 0s 
  a <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )

  #option to set exposure diferently for the 2 insecticides
  #BEWARE of what the defaults are
  #to set exp1 & 2 from exposure if they are not passed, partly for backwards compatibility
  if (is.null(exp1) & is.null(exp2))
  {
    exp1 <- exp2 <- exposure
  }
  
  
  if (insecticideUsed == "insecticide1")
  {
    #f
    a['f','A','0'] <- exp1
    #m
    a['m','A','0'] <- exp1 * male_exposure_prop    
    #for both m&f
    a[,'0','0'] <- 1 - a[,'A','0']  
    
  } else if (insecticideUsed == "insecticide2")
  {
    #f
    a['f','0','B'] <- exp2
    #m
    a['m','0','B'] <- exp2 * male_exposure_prop     
    #for both m&f
    a[,'0','0'] <- 1 - a[,'0','B']  
    
  } else if (insecticideUsed == "mixture")
  {
    #f
    #a['f','A','B'] <- exposure * correct_mix_deploy 
    #beware new, set AB to min of A & B
    a['f','A','B'] <- min(c(exp1,exp2)) * correct_mix_deploy
    #m
    a['m','A','B'] <- a['f','A','B'] * male_exposure_prop
    
    if ( correct_mix_deploy < 1 )
    {
      a['f','A','0'] <- (1 - correct_mix_deploy)/2 * exp1
      a['m','A','0'] <- a['f','A','0'] * male_exposure_prop
      
      #7/10/16 allowing separate exposures
      #BEWARE this is what it was before, need to do slightly differently because exp are diff
      #m&f together
      # a[,'0','B'] <- a[,'A','0']    
      # a[,'0','0'] <- 1 - (a[,'A','B'] + a[,'A','0'] + a[,'0','B'])  
      
      #new sep exp version, now same structure as for A above
      a['f','0','B'] <- (1 - correct_mix_deploy)/2 * exp2
      a['m','0','B'] <- a['f','0','B'] * male_exposure_prop
      
      #? does this need to be changed from single exp version ?
      #i think it may be ok
      a[,'0','0'] <- 1 - (a[,'A','B'] + a[,'A','0'] + a[,'0','B'])       
      
    } else
    {
      #if mixture applied correctly there are no single insecticide niches
      #same answer would probably be reached by going through previous loop
      #but this might make code clearer
      #a[,'0','0'] <- 1 - a[,'A','B']
      
      #7/10/16 allowing separate exposures
      #BEWARE with sep exposures you can get single insecticide niches
      #but just for the higher exp insecticide
      #need to be carfeul of when they are equal
      #ag! & if male_exp is different could get even trickier
      if (exp1 > exp2)
      {
        a['f','A','0'] <- exp1 - exp2
        a['m','A','0'] <- a['f','A','0'] * male_exposure_prop  
        
      } else if (exp2 > exp1)
      {
        a['f','0','B'] <- exp2 - exp1
        a['m','0','B'] <- a['f','0','B'] * male_exposure_prop        
      }
      
      a[,'0','0'] <- 1 - (a[,'A','B'] + a[,'A','0'] + a[,'0','B']) 
    }

# old version without correct_mix_deploy
#     #f
#     a['f','A','B'] <- exposure
#     #m
#     a['m','A','B'] <- exposure * male_exposure_prop
#     #for both m&f
#     a[,'0','0'] <- 1 - a[,'A','B']         
  } else
  {
    stop("insecticideUsed needs to be one of (insecticide1, insecticide2, mixture) it is ",insecticideUsed)
  }
 
  if (plot) plot_exposure(a)
   
  
  return(a)
}