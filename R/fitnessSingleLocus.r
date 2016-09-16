#' calculate single locus fitness from insecticide exposure
#' 
#' allows setting for multiple niches and insecticides within runModel2() by passing arrays h,s,phi,z 
#'  and testing for a single insecticide by passing effectiveness etc.

#' @param effectiveness1 effectiveness
#' @param effectiveness2 effectiveness
#' @param dominance dominance
#' @param selection_co selection_coefficient
#' @param cost fitness cost of R in no insecticide
#' @param SS fitness of SS if no insecticide
#' @param h dominance array
#' @param s selection coefficient array
#' @param phi effectiveness array
#' @param z cost array
#' @param Wloci array of single locus fitnesses to fill
#' 
#' @examples 
#' fitnessSingleLocus()
#' fitnessSingleLocus(effectiveness = 0.8)

#' @return fitness values
#' @export

fitnessSingleLocus <- function ( effectiveness1 = 0.5,
                                 effectiveness2 = 0.5,
                                 dominance = 0.5,
                                 selection_co = 0.5,
                                 cost = 0,
                                 SS = 1,
                                 h = NULL,
                                 s = NULL,
                                 phi = NULL,
                                 z = NULL,
                                 Wloci = NULL)
{
  
  if ( is.null(Wloci) )
  {
    Wloci   <- createArray2( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    #set from input file in runModel2()
    Wloci['SS1','no'] <- SS #input[30,i]
    Wloci['SS2','no'] <- SS #input[31,i] 
  }

  if ( is.null(h) )  
  {
    # dominance coefficient
    h       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi')) 
    h[1, 'hi'] <- dominance
    h[2, 'hi'] <- dominance
  }
  if ( is.null(s) )  
  {
    # selection coefficient
    s       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi') ) #or just lo hi
    s[1, 'hi'] <- selection_co
    s[2, 'hi'] <- selection_co    
  }  
  if ( is.null(phi) )  
  {
    # fitness of one locus (baseline), effectiveness
    phi     <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    phi[1, 'hi'] <- effectiveness1
    phi[2, 'hi'] <- effectiveness2    
  }  
  if ( is.null(z) )  
  {
    # fitness cost of resistance allele in no insecticide
    z       <- createArray2(locusNum=c(1,2))
    z[1] <- cost
    z[2] <- cost
  } 
 
  #testing
  #cat('effectiveness\n')
  #print(phi)
  
  
  for( locusNum in 1:2 ) #todo improve 1:2 get it from somewhere
  {
    #exposure 0 'no'
    Wloci[ paste0('RS',locusNum), 'no'] <- 1 - (h[locusNum, 'no'] * z[locusNum])
    Wloci[ paste0('RR',locusNum), 'no'] <- 1 - z[locusNum]
    
    for( exposID in c('lo','hi') )
    {
      Wloci[ paste0('SS',locusNum), exposID] <-  1 - phi[locusNum, exposID] 
      
      Wloci[ paste0('RS',locusNum), exposID] <- (1 - phi[locusNum, exposID]) + 
        (h[locusNum, exposID] * s[locusNum, exposID])
      
      Wloci[ paste0('RR',locusNum), exposID] <- (1 - phi[locusNum, exposID]) + 
        (s[locusNum, exposID])
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any( Wloci > 1  ) ) 
    warning( sum(Wloci > 1 ), " locus fitness values (Wloci) are >1 : ", Wloci[Wloci>1])
  if ( any( Wloci < 0 ) ) 
    warning( sum( Wloci < 0 ), " locus fitness values (Wloci) are <0")     
  
  return(Wloci)
}