#' run analyses to create fig2 of Curtis(1985)
#' 
#' Recreates figure 2 of Curtis(1985), allows inputs to be tweaked
#' 
#' @param max_gen number of generations
#' @param P_1 locus 1 frequency of resistance allele
#' @param P_2 locus 2 frequency of resistance allele
#' @param recomb_rate recombination rate
#' @param exposure the single exposure param used to generate the array
#' @param phi.SS1_a0 Baseline fitness of SS1 in a0
#' @param phi.SS1_A0 Baseline fitness of SS1 in A0
#' @param phi.SS2_0b Baseline fitness of SS2 in 0b
#' @param phi.SS2_0B Baseline fitness of SS2 in 0B
#' @param W.SS1_00 Fitness of SS1 in 00 (no insecticide)
#' @param W.SS2_00 Fitness of SS2 in 00 (no insecticide)
#' @param h.RS1_00 Dominance coefficient locus1 in 00
#' @param h.RS1_a0 Dominance coefficient locus1 in a0
#' @param h.RS1_A0 Dominance coefficient locus1 in A0
#' @param h.RS2_00 Dominance coefficient locus2 in 00
#' @param h.RS2_0b Dominance coefficient locus2 in 0b
#' @param h.RS2_0B Dominance coefficient locus2 in 0B
#' @param s.RR1_a0 Selection coefficient locus1 in a
#' @param s.RR1_A0 Selection coefficient locus1 in A
#' @param s.RR2_0b Selection coefficient locus2 in b
#' @param s.RR2_0B Selection coefficient locus2 in B
#' @param z.RR1_00 fitness cost of resistance allele 1 in insecticide free environment
#' @param z.RR2_00 fitness cost of resistance allele 2 in insecticide free environment
#' @param sexLinked whether resistance is sex linked, default=0(FALSE)
#' @param maleExposureProp male exposure as a propoertion of female, default 1 for same, likely <1  
#' @param correctMixDeployProp proportion of correct deployment of mixtures, 
#'    if <1 other portion divided between single insecticides
#' @param addCombinedStrategy whether to add a line for the combined mixture/individual strategy, makes plot confusing so set to FALSE by default
#' @param addStrategyLabels add labels to plot for where strategies cross 0.5 line TRUE gives seq,mix1,mix2,mix3
#' @param strategyLabels = c("seq","mix1","adapt","mix2")
#'  
#' @return list of plot components
#' @export

runcurtis_f2 <- function( max_gen = 500,
                          P_1 = 0.01,
                          P_2 = 0.01,
                          recomb_rate = 0.5,
                          exposure = 0.9,
                          phi.SS1_a0	=	0	,
                          phi.SS1_A0	=	0.73	,
                          phi.SS2_0b	=	0	,
                          phi.SS2_0B	=	1	,
                          W.SS1_00	=	1	,
                          W.SS2_00	=	1	,
                          h.RS1_00	=	0	,
                          h.RS1_a0	=	0	,
                          h.RS1_A0	=	0.17	,
                          h.RS2_00	=	0	,
                          h.RS2_0b	=	0	,
                          h.RS2_0B	=	0.0016	,
                          s.RR1_a0	=	0	,
                          s.RR1_A0	=	0.23	,
                          s.RR2_0b	=	0	,
                          s.RR2_0B	=	0.43	,
                          z.RR1_00	=	0	,
                          z.RR2_00	=	0	,
                          sexLinked = 0 ,
                          maleExposureProp = 1,
                          correctMixDeployProp = 1,
                          addCombinedStrategy = TRUE,
                          addStrategyLabels = TRUE,
                          strategyLabels = c("seq","mix1","adapt","mix2") )
{
  
  #fill exposure array I1
  a <- setExposure(exposure=exposure, insecticideUsed = "insecticide1",
                   maleExposureProp = maleExposureProp,
                   correctMixDeployProp = correctMixDeployProp )
  
  inputI1 <- setInputOneScenario(a=a,
                                 max_gen = max_gen,
                                 P_1 = P_1,
                                 P_2 = P_2,
                                 recomb_rate = recomb_rate,
                                 phi.SS1_a0	=	phi.SS1_a0	,
                                 phi.SS1_A0	=	phi.SS1_A0	,
                                 phi.SS2_0b	=	phi.SS2_0b	,
                                 phi.SS2_0B	=	phi.SS2_0B	,
                                 W.SS1_00	=	W.SS1_00	,
                                 W.SS2_00	=	W.SS2_00	,
                                 h.RS1_00	=	h.RS1_00	,
                                 h.RS1_a0	=	h.RS1_a0	,
                                 h.RS1_A0	=	h.RS1_A0	,
                                 h.RS2_00	=	h.RS2_00	,
                                 h.RS2_0b	=	h.RS2_0b	,
                                 h.RS2_0B	=	h.RS2_0B	,
                                 s.RR1_a0	=	s.RR1_a0	,
                                 s.RR1_A0	=	s.RR1_A0	,
                                 s.RR2_0b	=	s.RR2_0b	,
                                 s.RR2_0B	=	s.RR2_0B	,
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 maleExposureProp = maleExposureProp,
                                 correctMixDeployProp = correctMixDeployProp )
  
  #fill exposure array I2
  a <- setExposure(exposure=exposure, insecticideUsed = "insecticide2",
                   maleExposureProp = maleExposureProp,
                   correctMixDeployProp = correctMixDeployProp)
  
  inputI2 <- setInputOneScenario(a=a,
                                 max_gen = max_gen,
                                 P_1 = P_1,
                                 P_2 = P_2,
                                 recomb_rate = recomb_rate,
                                 phi.SS1_a0	=	phi.SS1_a0	,
                                 phi.SS1_A0	=	phi.SS1_A0	,
                                 phi.SS2_0b	=	phi.SS2_0b	,
                                 phi.SS2_0B	=	phi.SS2_0B	,
                                 W.SS1_00	=	W.SS1_00	,
                                 W.SS2_00	=	W.SS2_00	,
                                 h.RS1_00	=	h.RS1_00	,
                                 h.RS1_a0	=	h.RS1_a0	,
                                 h.RS1_A0	=	h.RS1_A0	,
                                 h.RS2_00	=	h.RS2_00	,
                                 h.RS2_0b	=	h.RS2_0b	,
                                 h.RS2_0B	=	h.RS2_0B	,
                                 s.RR1_a0	=	s.RR1_a0	,
                                 s.RR1_A0	=	s.RR1_A0	,
                                 s.RR2_0b	=	s.RR2_0b	,
                                 s.RR2_0B	=	s.RR2_0B	,
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 maleExposureProp = maleExposureProp,
                                 correctMixDeployProp = correctMixDeployProp )
  
  #fill exposure array mix
  a <- setExposure(exposure=exposure, insecticideUsed = "mixture",
                   maleExposureProp = maleExposureProp,
                   correctMixDeployProp = correctMixDeployProp )
  
  inputmix <- setInputOneScenario(a=a,
                                 max_gen = max_gen,
                                 P_1 = P_1,
                                 P_2 = P_2,
                                 recomb_rate = recomb_rate,
                                 phi.SS1_a0	=	phi.SS1_a0	,
                                 phi.SS1_A0	=	phi.SS1_A0	,
                                 phi.SS2_0b	=	phi.SS2_0b	,
                                 phi.SS2_0B	=	phi.SS2_0B	,
                                 W.SS1_00	=	W.SS1_00	,
                                 W.SS2_00	=	W.SS2_00	,
                                 h.RS1_00	=	h.RS1_00	,
                                 h.RS1_a0	=	h.RS1_a0	,
                                 h.RS1_A0	=	h.RS1_A0	,
                                 h.RS2_00	=	h.RS2_00	,
                                 h.RS2_0b	=	h.RS2_0b	,
                                 h.RS2_0B	=	h.RS2_0B	,
                                 s.RR1_a0	=	s.RR1_a0	,
                                 s.RR1_A0	=	s.RR1_A0	,
                                 s.RR2_0b	=	s.RR2_0b	,
                                 s.RR2_0B	=	s.RR2_0B	,
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 maleExposureProp = maleExposureProp,
                                 correctMixDeployProp = correctMixDeployProp) 
  
  #bind inputs together
  input <- cbind(inputI1, inputI2, inputmix)
  
  #run model
  listOut <- runModel2(input=input, produce.plots = FALSE)
  
  #this is a modified version of Beths function
  #allowing curtis fig2 to be applied to any scenario
  #plotcurtis_f2_generic( listOutMix$results[[1]], listOutI2$results[[1]], listOutI1$results[[1]] )
  #be careful of order
  #mix,2,1
  plotcurtis_f2_generic( listOut$results[[3]], listOut$results[[2]], listOut$results[[1]], 
                         addCombinedStrategy = addCombinedStrategy,
                         addStrategyLabels = addStrategyLabels,
                         strategyLabels = strategyLabels
                         )  
  
}