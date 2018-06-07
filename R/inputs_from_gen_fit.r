#' inputs_from_gen_fit calculate single locus model inputs from genotype fitnesses
#' 
#' not a component of the model but useful to calc model inputs from mosquito data. Can accept multiple rows.
#' Be careful that this needs fitness values, use 1-mortality if you have mortality data.
#'
#' @param SS SS fitness with insecticide
#' @param SR SR fitness with insecticide
#' @param RR RR fitness with insecticide
#' @param SS_noi SS fitness no insecticide
#' @param SR_noi SR fitness no insecticide
#' @param RR_noi RR fitness no insecticide
#' 
# @param plot whether to plot fitness
#' 
#' @examples 
#' inputs_from_gen_fit()
#' #curtis85 DDT
#' inputs_from_gen_fit(SS=0.27, SR=0.31, RR=0.5)
#' #curtis85 HCH
#' inputs_from_gen_fit(SS=0, SR=0.0007, RR=0.43)
#' #multiple rows
#' inputs_from_gen_fit(SS=c(0,0.1), SR=c(0.0007,0.1), RR=c(0.43,0.5))
#' #see survival_from_genotype_livedead.xls
#' #1 Kolaczinski2000 etofenprox (pyr), gives low eff 0.43, rr_ 0.63, dom 0.30
#' inputs_from_gen_fit(RR=0.84, SR=0.65, SS=0.57)
#' #Kolaczinski2000 etofenprox (pyr) including control mortality. eff 0.32, rr_ 1, dom 0.30
#' inputs_from_gen_fit(RR=0.84, SR=0.65, SS=0.57, SS_noi=0.84)
#' #Kolaczinski2000 Alpha-cypermethrin eff 0.41, rr_ 0.60, dom 0.52
#' inputs_from_gen_fit(RR=0.84, SR=0.72, SS=0.59)
#' #Kolaczinski2000 Alpha-cypermethrin with control mort eff 0.29, rr_ 1, dom 0.54, cost 0.19, domcost 1 (truncated from 2.06)
#' inputs_from_gen_fit(RR=0.83, SR=0.72, SS=0.59, SS_noi=0.83) 
#' inputs_from_gen_fit(RR=0.83, SR=0.72, SS=0.59, SS_noi=0.83, SR_noi = 0.5, RR_noi = 0.67) 
#' #2 Essandoh2013 An. gambiae	bendiocarb	Ace1, eff 0.98, rr_ 0.84, dom 0.66
#' inputs_from_gen_fit(RR=0.84, SR=0.56, SS=0.02)
#' #Essandoh2013 An. coluzii	bendiocarb	Ace1, eff 0.94, rr_ 1, dom 0.36
#' #vlow sample
#' inputs_from_gen_fit(RR=1, SR=0.4, SS=0.06)

#' @return fitness values
#' @export

inputs_from_gen_fit <- function ( SS = 0.2,
                                  SR = 0.4,
                                  RR = 1,
                                  SS_noi = NULL,
                                  SR_noi = NULL,
                                  RR_noi = NULL)
{
  
  dfin <- data.frame(eff=rep(NA,length(SS)), rr_=NA, dom=NA, cost=NA, domcost=NA)
  
  #in insecticide
  # Effectiveness (1-SSfit)
  # Selection coefficient = RRfit-SSfit
  # Resistance restoration = selection coefficient / effectiveness
  # Dominance of restoration = (SRfit-SSfit)/(RRfit-SSfit)

  
  if (!is.null(SS_noi)) 
  {
    #rescaling everything so that SS_noi is effectively 1  
    SS <- SS/SS_noi
    SR <- SR/SS_noi
    RR <- RR/SS_noi
    
    if (!is.null(SR_noi)) SR_noi <- SR_noi/SS_noi
    if (!is.null(RR_noi)) RR_noi <- RR_noi/SS_noi
  }

    
  dfin$eff <- (1-SS)
  dfin$rr_ <- (RR-SS) / dfin$eff
  dfin$dom <- (SR-SS) / (RR-SS)
  
  # calculate cost if there are data on fitness in absence of the insecticide
  if (!is.null(RR_noi) & !is.null(SR_noi) & !is.null(SS_noi))
  {
    dfin$cost <- 1 - RR_noi
    dfin$domcost <- (1 - SR_noi) / (1 - RR_noi)
  }
  

  return(dfin)
}