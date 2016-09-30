#' calculate niche genotype fitness from fitness of each locus
#' 
#' allows calling from runModel2() and independently 

#' @param a_fitloc array of single locus fitnesses
#' @param niche insecticide niche toggle allowing niches to be turned off
#' @param Wniche array of genotype fitnesses to fill
#' 
#' @examples 
#' fitnessNiche()
#' 
#' a_fitloc <- fitnessSingleLocus( effectiveness = 0.8 )
#' Wniche <- fitnessNiche( a_fitloc = a_fitloc )
#' a <- setExposure( exposure=0.5, insecticideUsed = 'mixture' )
#' Windiv <- fitnessIndiv( Wniche = Wniche, a = a )

#' @return fitness values in an array
#' @export

fitnessNiche <- function ( a_fitloc = NULL,
                           niche = NULL,
                           Wniche = NULL )
{
  
  # to allow this function to be called with no args
  if ( is.null(a_fitloc) )
  {
    a_fitloc   <- fitnessSingleLocus()
  }  

  if ( is.null(niche) )
  {
    # insecticide niche toggle
    niche   <- createArray2( niche1=c('0','a','A'), niche2=c('0','b','B') )
    #niche['0','0'] <- 1
    #niche['A','B'] <- 1
    # set all toggles on if nothing has been passed
    # this is what happens from runModel2() too
    niche[] <- 1
    
  }
  
  if ( is.null(Wniche) )
  {
    Wniche  <- createArray2( locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )    
  }
  
  ##################################################
  ### calculate two locus niche fitness in two insecticide Niche
  
  # multiply fitness of two insecticides
  # niches can be toggled off to get fitness of 0
  
  #refactored to replace 250+ lines in earlier version from Beth

  for( nicheNum1 in 1:3 ) #todo get this 1:3 from somewhere
  {
    for( nicheNum2 in 1:3 ) #todo get this 1:3 from somewhere
    { 
      #temporary solution
      #to get both niche (one of 0aAbB)
      #and exposure (one of no,lo,hi)
      niche1 <- dimnames(Wniche)$niche1[ nicheNum1 ]
      niche2 <- dimnames(Wniche)$niche2[ nicheNum2 ]
      exposure1 <- dimnames(a_fitloc)$exposure[ nicheNum1 ]
      exposure2 <- dimnames(a_fitloc)$exposure[ nicheNum2 ]        
      
      #if this niche toggled off set fitness to 0
      if (niche[niche1,niche2] == 0)
      {
        Wniche[,,niche1,niche2] <- 0
      } else{
        #otherwise set fitness to product of the 2 loci
        for( locus1 in dimnames(Wniche)$locus1)
        {
          for( locus2 in dimnames(Wniche)$locus2)
          {
            ###########################################################################
            #6/1/16 i think ians new insecticide interaction parameter can just go here
            #does in need to be just one param or 4 ?
            #ΛAB, ΛAb, ΛaB or Λab 
            #Wniche[locus1,locus2,niche1,niche2] <- interaction * a_fitloc[locus1,exposure1] * a_fitloc[locus2,exposure2]
            Wniche[locus1,locus2,niche1,niche2] <- a_fitloc[locus1,exposure1] * a_fitloc[locus2,exposure2]
          }
        }          
      }
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any(Wniche > 1  ) ) 
    warning( sum(Wniche > 1  ), " niche fitness values (Wniche) are >1 ")
  if ( any( Wniche < 0 ) ) 
    warning( sum( Wniche < 0 ), " niche fitness values (Wniche) are <0")    
 
   
  return(Wniche)
}