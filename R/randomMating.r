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
#' @param sexLinked, whether sex linked
#' @param isMale whether male for sex linkage
#' 
#' @examples 
#' namesLoci <- c( "SS1SS2", "SS1RS2", "SS1RR2", 
#'                 "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
#'                 "RR1SS2", "RR1RS2", "RR1RR2")
#' sex2 <- c("m","f")
#' a_gtypes <- createArray2( sex=sex2, loci=namesLoci )
#' #assign all of both parents to one genotype for convenience
#' a_gtypes['m','RS1RS2_trans'] <- 1
#' a_gtypes['f','RS1RS2_trans'] <- 1
#' G <- createGametes( a_gtypes = a_gtypes, recomb_rate = 0.5 )
#' fGenotypeExpanded <- randomMating(G) 
#' #and for sex linked
#' fGenotypeExpanded2 <- randomMating(G, sexLinked=TRUE, isMale=TRUE) 
#' @return array with frequencies of genotypes in expanded format
#' @export

randomMating <- function( G,
                          sexLinked = FALSE,
                          isMale = FALSE)
{
  
  #create array with named dimensions to hold results
  #l1a1 : locus1 allele1
  #l1a2 : locus1 allele2
  #l2a1 : locus2 allele1
  #l2a2 : locus2 allele2
  fGenotypeExpanded <- createArray2(l1a1=c('S1','R1'),l1a2=c('S1','R1'),l2a1=c('S2','R2'),l2a2=c('S2','R2'))
  
  
  #created genotype frequencies
  #beth did for males only and copied to females because they were the same
  
  #If sex-linked Locus 1 is homozygous in the male so heterozygotes are impossible at this locus 
  #the allele inherited by males at locus 1 is the maternal-derived one 
  #(because they get their X chromosome from their mother and the Y from the father). 
  #males will be simulated as RR or SS at the locus even though, in reality they will be either R- or S-
  
  
  counter <- 0
  
  #the simple non sex linked case
  if (!sexLinked | !isMale)
  {
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
          }
        }
      }
    }    
  } else #i.e. if is sexLinked and male offspring
  {
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
            
            fThisGenotype <- G['m',m1,m2] * G['f',f1,f2]
            
            #cat(paste0(counter," ",substr(m1,1,1),f1," ",substr(m2,1,1),f2,": ",signif(fThisGenotype),"\n"))
            #cat(paste0(counter," ",substr(m1,1,1),f1," ",substr(m2,1,1),f2,": ",signif(fThisGenotype),"   "))
            
            #BEWARE the logic here is quite tricky
            #but the warning at the end does now confirm that it works
            
            #heterozygotes at locus1 are impossible so add to the homozygotes instead
            if(f1!=m1)
            {
              #add to the homozygotes : f1,f1
              fGenotypeExpanded[f1,f1,f2,m2] <- fGenotypeExpanded[f1,f1,f2,m2] + fThisGenotype
              #cat(paste0("add to ",f1,f1,f2,m2,"\n"))
            } else #i.e. if not heterozygous at locus 1
            {
              #just add to this genotype : f1,m2
              #have to add rather than set in case this is a homozygote that has already been added to by previous condition
              fGenotypeExpanded[f1,m1,f2,m2] <- fGenotypeExpanded[f1,m1,f2,m2] + fThisGenotype
              #cat(paste0("set    ",f1,m1,f2,m2,"\n"))
            }
          }
        }
      }
    }    
      
  }
  
#allow for rounding differences in the warning
if ( !isTRUE( all.equal(1, sum(fGenotypeExpanded)  )))
  warning("genotype frequencies after random mating don't sum to 1 ", sum(fGenotypeExpanded) ) 
  
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