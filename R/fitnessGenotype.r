#' calculate individual fitness based on niche fitness and exposure
#' 
#' allows calling from runModel2() and independently 

#' @param a_fitnic array of niche fitnesses
#' @param a array of exposure in each niche
#' @param a_fitgen array of individual fitnesses to fill
#' 
#' @examples 
#' fitnessGenotype()
#' 
#' a_fitloc <- fitnessSingleLocus( effectiveness = 0.8 )
#' a_fitnic <- fitnessNiche( a_fitloc = a_fitloc )
#' expos <- setExposure( exposure=0.5, insecticideUsed = 'mixture' )
#' a_fitgen <- fitnessGenotype( a_fitnic = a_fitnic, expos = expos )

#' @return fitness values in an array
#' @export

fitnessGenotype <- function ( a_fitnic = NULL,
                           expos = NULL,
                           a_fitgen = NULL )
{
  
  # to allow this function to be called with no args
  if ( is.null(a_fitnic) )
  {
    a_fitnic   <- fitnessNiche()
  }  

  if ( is.null(expos) )
  {
    expos <- setExposure()
    #expos <- setExposure( exposure=0.9, insecticideUsed = 'mixture' )
  }
  
  if ( is.null(a_fitgen) )
  {
    # empty array to fill
    a_fitgen  <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
  }

  #testing
  # cat("in fitnessGenotype\n")
  # df_niche <- as.data.frame( aperm( a_fitnic[,,c('A'),c('B','0')], c('niche2','locus1','locus2')) )
  # rownames(df_niche)[1] <- 'niche'
  # print(df_niche[1,])
  # print(as.data.frame(expos)[1,]) #exposure
    
  
  for( sex in dimnames(a_fitgen)$sex)
  {
    for( locus1 in dimnames(a_fitgen)$locus1)
    {
      for( locus2 in dimnames(a_fitgen)$locus2)
      {
        # multiplies exposure by fitness for all niches & then sums
        # creates a weighted average of exposure in each niche
        a_fitgen[sex,locus1,locus2] <- sum( expos[sex,,] * a_fitnic[locus1,locus2,,])
      }
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any(a_fitgen > 1 ) ) 
    warning( sum(a_fitgen > 1  ), " individual fitness values (Wloci) are >1")
  if ( any( a_fitgen < 0 ) ) 
    warning( sum( a_fitgen < 0 ), " individual fitness values (Wloci) are <0")
  
  #testing
  #cat("in fitnessGenotype\n")
  #df_indiv <- as.data.frame(a_fitgen)
  #print(df_indiv[1,]) #just m
  
  return(a_fitgen)
}