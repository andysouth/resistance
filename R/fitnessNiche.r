#' calculate niche genotype fitness from fitness of each locus
#' 
#' allows calling from runModel2() and independently 

#' @param a_fitloc array of single locus fitnesses
#' @param a_nichetog insecticide niche toggle allowing niches to be turned off
#' @param a_fitnic array of genotype fitnesses to fill
#' 
#' @examples 
#' fitnessNiche()
#' 
#' a_fitloc <- fitnessSingleLocus( eff1 = 0.8 )
#' a_fitnic <- fitnessNiche( a_fitloc = a_fitloc )
#' a_expos <- setExposure( exposure=0.5, insecticideUsed = 'mixture' )
#' a_fitgen <- fitnessGenotype( a_fitnic = a_fitnic, a_expos = a_expos )

#' @return fitness values in an array
#' @export

fitnessNiche <- function ( a_fitloc = NULL,
                           a_nichetog = NULL,
                           a_fitnic = NULL )
{
  
  # to allow this function to be called with no args
  if ( is.null(a_fitloc) )
  {
    a_fitloc   <- fitnessSingleLocus()
  }  

  if ( is.null(a_nichetog) )
  {
    # insecticide niche toggle
    a_nichetog   <- createArray2( niche1=c('0','a','A'), niche2=c('0','b','B') )
    #a_nichetog['0','0'] <- 1
    #a_nichetog['A','B'] <- 1
    # set all toggles on if nothing has been passed
    # this is what happens from runModel2() too
    a_nichetog[] <- 1
    
  }
  
  if ( is.null(a_fitnic) )
  {
    a_fitnic  <- createArray2( locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )    
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
      niche1 <- dimnames(a_fitnic)$niche1[ nicheNum1 ]
      niche2 <- dimnames(a_fitnic)$niche2[ nicheNum2 ]
      exposure1 <- dimnames(a_fitloc)$exposure[ nicheNum1 ]
      exposure2 <- dimnames(a_fitloc)$exposure[ nicheNum2 ]        
      
      #if this niche toggled off set fitness to 0
      if (a_nichetog[niche1,niche2] == 0)
      {
        a_fitnic[,,niche1,niche2] <- 0
      } else{
        #otherwise set fitness to product of the 2 loci
        for( locus1 in dimnames(a_fitnic)$locus1)
        {
          for( locus2 in dimnames(a_fitnic)$locus2)
          {
            ###########################################################################
            #6/1/16 i think ians new insecticide interaction parameter can just go here
            #does in need to be just one param or 4 ?
            #ΛAB, ΛAb, ΛaB or Λab 
            #a_fitnic[locus1,locus2,niche1,niche2] <- interaction * a_fitloc[locus1,exposure1] * a_fitloc[locus2,exposure2]
            a_fitnic[locus1,locus2,niche1,niche2] <- a_fitloc[locus1,exposure1] * a_fitloc[locus2,exposure2]
          }
        }          
      }
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any(a_fitnic > 1  ) ) 
    warning( sum(a_fitnic > 1  ), " niche fitness values (a_fitnic) are >1 ")
  if ( any( a_fitnic < 0 ) ) 
    warning( sum( a_fitnic < 0 ), " niche fitness values (a_fitnic) are <0")    
 
   
  return(a_fitnic)
}