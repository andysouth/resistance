#' random mating
#' 
#' constructs genotypes given gametes
#' returns array of genotype frequencies
#' This is refactored from code in runModel()
#' 
#' @param f array with frequencies of genotypes in popn 
#' @param G array with frequencies of gametes
#' 
#' @examples 
#' #randomMating() 
#' @return array with frequencies of genotypes
#' @export

#todo f is only passed so I don't have to create the dimensions within the func, maybe change how this is done

randomMating <- function( f, G)
{
  
  
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