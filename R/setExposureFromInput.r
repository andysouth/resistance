#' set exposure to insecticide from input vector
#' 
#' fills an array of exposure values from input vector (i.e. where calculations have already been done)
#' see setExposure() to do the calculations
#' 
#' @param input input vector
#' @param scen_num scenario number
#' 
#' @examples
#' input <- setInputOneScenario()
#' expos <- setExposureFromInput( input )
#' @return array of exposure values for the different insecticides
#' @export
#' 
setExposureFromInput <- function( input, scen_num )
{

  # exposure to insecticides
  # exposure array initialise with 0s 
  a <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )

  ## Exposure levels of males and females to each insecticide niche
  # lower case = low concentration, upper case = high, 0 = absence   
  
  # males
  a['m','0','0'] <- input[8,scen_num]
  
  a['m','a','0'] <- input[9,scen_num]
  a['m','A','0'] <- input[10,scen_num]
  
  a['m','0','b'] <- input[11,scen_num]
  a['m','0','B'] <- input[12,scen_num]
  
  a['m','a','b'] <- input[13,scen_num]
  a['m','A','B'] <- input[14,scen_num]    
  
  a['m','A','b'] <- input[15,scen_num]
  a['m','a','B'] <- input[16,scen_num]      
  
  #allow rounding errors
  if ( !isTRUE( all.equal(1, sum(a['m',,])  ))){
    stop( paste("Error in male exposures: must total one: ", sum(a['m',,])) )
  }
  
  # females
  a['f','0','0'] <- input[17,scen_num]
  
  a['f','a','0'] <- input[18,scen_num]
  a['f','A','0'] <- input[19,scen_num]
  
  a['f','0','b'] <- input[20,scen_num]
  a['f','0','B'] <- input[21,scen_num]
  
  a['f','a','b'] <- input[22,scen_num]
  a['f','A','B'] <- input[23,scen_num] 
  
  a['f','A','b'] <- input[24,scen_num]
  a['f','a','B'] <- input[25,scen_num] 
  
  #allow rounding errors
  if ( !isTRUE( all.equal(1, sum(a['f',,])  ))){    
    stop( paste("Error in female exposures: must total one: ", sum(a['f',,])) )
  }
  
  
  return(a)
}