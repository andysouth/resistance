#' calculate individual fitness based on niche fitness and exposure
#' 
#' allows calling from runModel2() and independently 

#' @param Wniche array of niche fitnesses
#' @param a array of exposure in each niche
#' @param Windiv array of individual fitnesses to fill
#' 
#' @examples 
#' fitnessIndiv()
#' 
#' Wloci <- fitnessSingleLocus( effectiveness = 0.8 )
#' Wniche <- fitnessNiche( Wloci = Wloci )
#' a <- setExposure( exposure=0.5, insecticideUsed = 'mixture' )
#' Windiv <- fitnessIndiv( Wniche = Wniche, a = a )

#' @return fitness values in an array
#' @export

fitnessIndiv <- function ( Wniche = NULL,
                           a = NULL,
                           Windiv = NULL )
{
  
  # to allow this function to be called with no args
  if ( is.null(Wniche) )
  {
    Wniche   <- fitnessNiche()
  }  

  if ( is.null(a) )
  {
    a <- setExposure()
    #a <- setExposure( exposure=0.9, insecticideUsed = 'mixture' )
  }
  
  if ( is.null(Windiv) )
  {
    # empty array to fill
    Windiv  <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
  }

    
  for( sex in dimnames(Windiv)$sex)
  {
    for( locus1 in dimnames(Windiv)$locus1)
    {
      for( locus2 in dimnames(Windiv)$locus2)
      {
        # multiplies exposure by fitness for all niches & then sums
        # creates a weighted average of exposure in each niche
        Windiv[sex,locus1,locus2] <- sum( a[sex,,] * Wniche[locus1,locus2,,])
      }
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any(Windiv > 1 ) ) 
    warning( sum(Windiv > 1  ), " individual fitness values (Wloci) are >1")
  if ( any( Windiv < 0 ) ) 
    warning( sum( Windiv < 0 ), " individual fitness values (Wloci) are <0")
  
  
  return(Windiv)
}