#' convert expanded genotype array to contracted genotype array
#' 
#' constructs genotypes given gametes
#' returns array of genotype frequencies
#' This is refactored from code in runModel()
#' 
#' @param aLong array with frequencies of expanded genotypes
#' 
#' @examples 
#' #genotypesLong2Short() 
#' @return array with frequencies of genotypes
#' @export

genotypesLong2Short <- function( aLong )
{
  
  #to convert from this
  #fGenotypeExpanded <- array_named(l1a1=c('S1','R1'),l1a2=c('S1','R1'),l2a1=c('S2','R2'),l2a2=c('S2','R2'))  

  #to this
  genotypes <- c( "SS1SS2", "SS1RS2", "SS1RR2", 
                    "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                    "RR1SS2", "RR1RS2", "RR1RR2")
  
  aShort <- array_named( genotypes=genotypes )
  
  
  aShort["SS1SS2"] <- aLong['S1','S1','S2','S2']
  
  aShort["SS1RS2"] <- aLong['S1','S1','S2','R2'] +
                      aLong['S1','S1','R2','S2']
  
  aShort["SS1RR2"] <- aLong['S1','S1','R2','R2']
  
  aShort["RS1SS2"] <- aLong['S1','R1','S2','S2'] +
                      aLong['R1','S1','S2','S2']
  
  aShort["RS1RS2_cis"] <- aLong['S1','R1','S2','R2'] +
                          aLong['R1','S1','R2','S2']  
  
  aShort["RS1RS2_trans"] <- aLong['S1','R1','R2','S2'] +
                            aLong['R1','S1','S2','R2']  
  
  aShort["RS1RR2"] <- aLong['S1','R1','R2','R2'] +
                      aLong['R1','S1','R2','R2']
      
  aShort["RR1SS2"] <- aLong['R1','R1','S2','S2']

  aShort["RR1RS2"] <- aLong['R1','R1','S2','R2'] +
                      aLong['R1','R1','R2','S2']  
  
  aShort["RR1RR2"] <- aLong['R1','R1','R2','R2']
    
  return(aShort)

}