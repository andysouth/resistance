#' run analyses to create fig2 of Curtis(1985)
#' 
#' Recreates figure 2 of Curtis(1985), allows inputs to be tweaked
#' 
#' @param max_gen number of generations
#' @param P_1 locus 1 frequency of resistance allele
#' @param P_2 locus 2 frequency of resistance allele
#' @param recomb_rate recombination rate
#' @param exposure exposure to the insecticide(s) if set same for both
#' @param exp1 option to set exposure differently for the diff insecticides
#' @param exp2 option to set exposure differently for the diff insecticides
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
#' @param rr_restoration_ins1 effect of resistance in overcoming insecticide 1 effectiveness in RR
#' @param rr_restoration_ins2 effect of resistance in overcoming insecticide 2 effectiveness in RR  
#' @param z.RR1_00 fitness cost of resistance allele 1 in insecticide free environment
#' @param z.RR2_00 fitness cost of resistance allele 2 in insecticide free environment
#' @param sexLinked whether resistance is sex linked, default=0(FALSE)
#' @param male_exposure_prop male exposure as a propoertion of female, default 1 for same, likely <1  
#' @param correct_mix_deploy proportion of times that mixture is deployed correctly, 
#'    assumes that when not deployed correctly the single insecticides are used instead
#' @param addCombinedStrategy whether to add a line for the combined mixture/individual strategy, makes plot confusing so set to FALSE by default
#' @param addStrategyLabels add labels to plot for where strategies cross 0.5 line TRUE gives seq,mix1,mix2,mix3
#' @param strategyLabels = c("seq","mix1","adapt","mix2")
#' @param xlab xlab
#' @param ylab ylab
#' @param ylabs whether to add tick labels
#' @param xlabs whether to add tick labels
#' @param yticks tick positions
#' @param cex.axis scaling for x&y tick labs 
#' @param addLegend whether to add a legend inside plot
#' @param main title for the plot
#' @param vlines colour of vertical lines to add to plot, NULL for none
#' @param maxX optional max x value for the plot to facilitate comparison with other plots
#' @param labelMixSeqRatio how many dp in label of the ratio of mix/seq, NULL for no label, only option compatible with addLegend
#'
#' @return list of plot components
#' @export

runcurtis_f2 <- function( max_gen = 500,
                          P_1 = 0.01,
                          P_2 = 0.01,
                          recomb_rate = 0.5,
                          exposure = 0.9,
                          exp1 = NULL,
                          exp2 = NULL,
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
                          s.RR1_A0	=	NULL	,
                          s.RR2_0b	=	0	,
                          s.RR2_0B	=	NULL	,
                          # 14/6/16 rr_restoration = s / effectiveness
                          rr_restoration_ins1 = 0.23/0.73,
                          rr_restoration_ins2 = 0.43/1,    

                          z.RR1_00	=	0	,
                          z.RR2_00	=	0	,
                          sexLinked = 0 ,
                          male_exposure_prop = 1,
                          correct_mix_deploy = 1,
                          addCombinedStrategy = FALSE,
                          addStrategyLabels = TRUE,
                          strategyLabels = c("seq","mix1","adapt","mix2"),
                          ylab = "Resistance frequency %",
                          ylabs = TRUE,
                          yticks = c(0.1,1,5,10,50,100),
                          xlab = "Generation",
                          xlabs = TRUE,
                          cex.axis = 0.8,
                          addLegend = TRUE,
                          main = "",
                          vlines = 'grey95',
                          maxX = NULL,
                          labelMixSeqRatio = NULL
                          )
{
  
  #fill exposure array I1
  a <- setExposure(exposure=exposure, exp1=exp1, exp2=exp2,
                   insecticideUsed = "insecticide1",
                   male_exposure_prop = male_exposure_prop,
                   correct_mix_deploy = correct_mix_deploy )
  
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
                                 # 14/6/16
                                 rr_restoration_ins1 = rr_restoration_ins1,
                                 rr_restoration_ins2 = rr_restoration_ins2,    
                                 
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 male_exposure_prop = male_exposure_prop,
                                 correct_mix_deploy = correct_mix_deploy )
  
  #fill exposure array I2
  a <- setExposure(exposure=exposure, exp1=exp1, exp2=exp2,
                   insecticideUsed = "insecticide2",
                   male_exposure_prop = male_exposure_prop,
                   correct_mix_deploy = correct_mix_deploy )
  
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
                                 # 14/6/16
                                 rr_restoration_ins1 = rr_restoration_ins1,
                                 rr_restoration_ins2 = rr_restoration_ins2, 
                                 
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 male_exposure_prop = male_exposure_prop,
                                 correct_mix_deploy = correct_mix_deploy )
  
  #fill exposure array mix
  a <- setExposure(exposure=exposure,  exp1=exp1, exp2=exp2,
                   insecticideUsed = "mixture",
                   male_exposure_prop = male_exposure_prop,
                   correct_mix_deploy = correct_mix_deploy  )
  
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
                                 # 14/6/16
                                 rr_restoration_ins1 = rr_restoration_ins1,
                                 rr_restoration_ins2 = rr_restoration_ins2, 
                                 
                                 z.RR1_00	=	z.RR1_00	,
                                 z.RR2_00	=	z.RR2_00	,
                                 sexLinked = sexLinked ,
                                 male_exposure_prop = male_exposure_prop,
                                 correct_mix_deploy = correct_mix_deploy ) 
  
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
                         strategyLabels = strategyLabels,
                         xlab = xlab,
                         xlabs = xlabs,
                         ylab = ylab,
                         ylabs = ylabs,
                         yticks = yticks,
                         cex.axis = cex.axis,
                         addLegend = addLegend,
                         main = main,
                         vlines = vlines,
                         maxX = maxX,
                         labelMixSeqRatio = labelMixSeqRatio
                         )  
  
  #add returning the results
  invisible(listOut)
  
}