#' set exposure to insecticide
#' 
#' fills an array of exposure values, according to whether insecticide 1,2 or mixture are being used
#' initially assumes same for M&F could change later
#' also assumes exposure is the same to each insecticide which can also be changed in future
#' 
#' @param exposure exposure to the insecticide(s)
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' @param maleExposureProp proportion tht males are exposed relative to f, default 1, likely to be <1
#' @param correctMixDeployProp proportion of times that mixture is deployed correctly, 
#'    assumes that when not deployed correctly the single insecticides are used instead
#'    TODO check my understanding of this
#' 
#' @examples
#' a <- setExposure( exposure='0.9', insecticideUsed = 'mixture' )
#' a <- setExposure( exposure='0.9',  insecticideUsed = 'insecticide1' )
#' @return array of exposure values for the different insecticides
#' @export
#' 
setExposure <- function( exposure = 0.9,
                         insecticideUsed = 'mixture',
                         maleExposureProp = 1,
                         correctMixDeployProp = 1 )
{

  #exposure to insecticide
  #exposure array initialise with 0s 
  a <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
  #depends on whether this is insecticide1, insecticide2 or mixture
  
  if (insecticideUsed == "insecticide1")
  {
    #f
    a['f','A','0'] <- exposure
    #m
    a['m','A','0'] <- exposure * maleExposureProp    
    #for both m&f
    a[,'0','0'] <- 1 - a[,'A','0']  
    
  } else if (insecticideUsed == "insecticide2")
  {
    #f
    a['f','0','B'] <- exposure
    #m
    a['m','0','B'] <- exposure * maleExposureProp     
    #for both m&f
    a[,'0','0'] <- 1 - a[,'0','B']  
    
  } else if (insecticideUsed == "mixture")
  {
    #f
    a['f','A','B'] <- exposure * correctMixDeployProp
    #m
    a['m','A','B'] <- a['f','A','B'] * maleExposureProp
    
    if ( correctMixDeployProp < 1 )
    {
      a['f','A','0'] <- (1 - correctMixDeployProp)/2 * exposure
      
      a['m','A','0'] <- a['f','A','0'] * maleExposureProp
      
      #m&f together
      a[,'0','B'] <- a[,'A','0']    
      a[,'0','0'] <- 1 - (a[,'A','B'] + a[,'A','0'] + a[,'0','B'])  
      
    } else
    {
      #if mixture applied correctly there are no single insecticide niches
      #same answer would probably be reached by going through previous loop
      #but this might make code clearer
      a[,'0','0'] <- 1 - a[,'A','B']
    }

# old version without correctMixDeployProp
#     #f
#     a['f','A','B'] <- exposure
#     #m
#     a['m','A','B'] <- exposure * maleExposureProp
#     #for both m&f
#     a[,'0','0'] <- 1 - a[,'A','B']         
  } else
  {
    stop("insecticideUsed needs to be one of (insecticide1, insecticide2, mixture) it is ",insecticideUsed)
  }
  
  
  return(a)
}