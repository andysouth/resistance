#' inputs_from_gen_fit calculate single locus model inputs from genotype fitnesses
#' 
#' not component of the model but useful to calc model inputs from mosquito data
#' 
#' new 2017_12

#' @param SSfit fitness of SS with insecticide
#' @param RSfit fitness of SR with insecticide
#' @param RRfit fitness of RR with insecticide
# @param plot whether to plot fitness
#' 
#' @examples 
#' inputs_from_gen_fit()
#' #curtis85 DDT
#' inputs_from_gen_fit(SS=0.27, RS=0.31, RR=0.5)
#' #curtis85 HCH
#' inputs_from_gen_fit(SS=0, RS=0.0007, RR=0.43)
#' #see survival_from_genotype_livedead.xls
#' #1 Kolaczinski2000 etofenprox (pyr), gives low eff 0.43, rr_ 0.63, dom 0.30
#' inputs_from_gen_fit(RR=0.84, RS=0.65, SS=0.57)
#' #Kolaczinski2000 Alpha-cypermethrin eff 0.41, rr_ 0.60, dom 0.52
#' inputs_from_gen_fit(RR=0.84, RS=0.72, SS=0.59)
#' #2 Essandoh2013 An. gambiae	bendiocarb	Ace1, eff 0.98, rr_ 0.84, dom 0.66
#' inputs_from_gen_fit(RR=0.84, RS=0.56, SS=0.02)
#' #Essandoh2013 An. coluzii	bendiocarb	Ace1, eff 0.94, rr_ 1, dom 0.36
#' #vlow sample
#' inputs_from_gen_fit(RR=1, RS=0.4, SS=0.06)





#' @return fitness values
#' @export

inputs_from_gen_fit <- function ( SSfit = 0.2,
                                  RSfit = 0.4,
                                  RRfit = 1)
{
  
  dfin <- data.frame(eff=NA, rr_=NA, dom=NA)
  
  #in insecticide
  # Effectiveness (1-SSfit)
  # Selection coefficient = RRfit-SSfit
  # Resistance restoration = selection coefficient / effectiveness
  # Dominance of restoration = (RSfit-SSfit)/(RRfit-SSfit)
  
  dfin$eff <- (1-SSfit)
  
  dfin$rr_ <- (RRfit-SSfit) / dfin$eff
  
  dfin$dom <- (RSfit-SSfit)/(RRfit-SSfit)
  
  #a_fitloc   <- array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
  return(dfin)
}