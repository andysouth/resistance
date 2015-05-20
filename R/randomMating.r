#' random mating at 2 loci that have either susceptible or resistant alleles
#' 
#' constructs genotypes given gametes
#' returns array of genotype frequencies in expanded format 
#' i.e. the 16 exhaustive combinations of 2 alleles at 2 loci (2x2x2x2).
#' These can be converted to the collapsed 10 genotype format (8+cis&trans for the double heterozygotes), 
#' by \code{\link{genotypesLong2Short}}
#' This is refactored from code in runModel()
#' 
#' @param G array with frequencies of gametes
#' 
#' @examples 
#' #randomMating() 
#' @return array with frequencies of genotypes in expanded format
#' @export

randomMating <- function( G)
{
  
  #create array with named dimensions to hold results
  #l1a1 : locus1 allele1
  #l1a2 : locus1 allele2
  #l2a1 : locus2 allele1
  #l2a2 : locus2 allele2
  fGenotypeExpanded <- createArray2(l1a1=c('S1','R1'),l1a2=c('S1','R1'),l2a1=c('S2','R2'),l2a2=c('S2','R2'))
  
  counter <- 0
  
  # m1,2 the male parent derived gamete at locus1 & 2
  # f1,2 the female parent equivalent  
  for( m2 in c('S2','R2'))
  {
    for( m1 in c('S1','R1'))
    {
      for( f2 in c('S2','R2'))
      {
        for( f1 in c('S1','R1'))
        {
          counter <- counter+1
          #cat(paste(counter, m1,f1,m2,f2,"\n"))
          #cat(paste0(counter," ",substr(m1,1,1),f1," ",substr(m2,1,1),f2,"\n"))
          
          fGenotypeExpanded[f1,m1,f2,m2] <- G['m',m1,m2] * G['f',f1,f2]
          
          #created genotype frequencies
          #beth just does for males to start
          #f['m',]
        }
      }
    }
  }
  
return( fGenotypeExpanded )
  
}  
  
## Random Mating ##

# # SS male with SS female
# f.m.SS1SS2 <-  G.m.S1.S2 * G.f.S1.S2 
# # SS male with SR female
# f.m.SS1RS2 <-  G.m.S1.S2 * G.f.S1.R2 
# # SS male with RS female
# f.m.RS1SS2 <-  G.m.S1.S2 * G.f.R1.S2 
# # SS male with RR female
# f.m.RS1RS2_cis <-  G.m.S1.S2 * G.f.R1.R2 
# 
# # SR male with SS female
# f.m.SS1RS2 <-  G.m.S1.R2 * G.f.S1.S2 
# # SR male with SR female
# f.m.SS1RR2 <-  G.m.S1.R2 * G.f.S1.R2 
# # SR male with RS female
# f.m.RS1RS2_trans <-  G.m.S1.R2 * G.f.R1.S2 
# # SR male with RR female
# f.m.RS1RR2 <-  G.m.S1.R2 * G.f.R1.R2 
# 
# # RS male with SS female
# f.m.RS1SS2 <-  G.m.R1.S2 * G.f.S1.S2 
# # RS male with SR female
# f.m.RS1RS2_trans <-  G.m.R1.S2 * G.f.S1.R2 
# # RS male with RS female
# f.m.RR1SS2 <-  G.m.R1.S2 * G.f.R1.S2
# # RS male with RR female
# f.m.RR1RS2 <-  G.m.R1.S2 * G.f.R1.R2 
# 
# # RR male with SS female
# f.m.RS1RS2_cis <-  G.m.R1.R2 * G.f.S1.S2 
# # RR male with SR female
# f.m.RS1RR2 <-  G.m.R1.R2 * G.f.S1.R2 
# # RR male with RS female
# f.m.RR1RS2 <-  G.m.R1.R2 * G.f.R1.S2 
# # RR male with RR female
# f.m.RR1RR2 <-  G.m.R1.R2 * G.f.R1.R2 

# check of total genotype frequencies
#gen.total <- ( f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2 +
#			   f.m.RS1SS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RS1RR2 +
#			   f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 )

#print( paste( "Genotype totals after mating = ",gen.total ) )