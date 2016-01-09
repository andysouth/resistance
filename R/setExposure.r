#' set exposure to insecticide
#' 
#' fills an array of exposure values, according to whether insecticide 1,2 or mixture are being used
#' initially assumes same for M&F could change later
#' also assumes exposure is the same to each insecticide which can also be changed in future
#' 
#' @param exposure exposure to teh insecticide(s)
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' 
#' @examples
#' a <- setExposure( exposure='0.9', insecticideUsed = 'mixture' )
#' a <- setExposure( exposure='0.9',  insecticideUsed = 'insecticide1' )
#' @return array of exposure values for the different insecticides
#' @export
#' 
setExposure <- function( exposure = 0.9,
                         insecticideUsed = 'mixture',
                         maleExposureProp = 1)
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
    a['f','A','B'] <- exposure
    #m
    a['m','A','B'] <- exposure * maleExposureProp
    #for both m&f
    a[,'0','0'] <- 1 - a[,'A','B']         
  } else
  {
    stop("insecticideUsed needs to be one of (insecticide1, insecticide2, mixture) it is ",insecticideUsed)
  }
  
  
  return(a)
}