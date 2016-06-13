#' set inputs for one scenario
#' 
#' fills one column of input matrix  
#' user can specify as many or few parameters as they wish, any not specified will be set to default value.
#' 
#' BEWARE function programmer not to change order of the function arguments as this determines the names of the output
#' 
#' @param calibration one of a limited set of integers effecting how scenarios are run 
#' @param max_gen number of generations
#' @param coll.fitvals save fitness values in a matrix 0/1
#' @param save.fitvals save fitness values to an external .csv 0/1
#' @param P_1 locus 1 frequency of resistance allele
#' @param P_2 locus 2 frequency of resistance allele
#' @param recomb_rate recombination rate
#' @param a array to set all exposure params, a[sex,loc1,loc2], overrides a.m_00 etc.
#' @param exposure the single exposure param used to generate the array, here just to allow saving for post-run analyses
#' @param a.m_00 insecticide exposure male no1 no2
#' @param a.m_a0 insecticide exposure male lo1 no2
#' @param a.m_A0 insecticide exposure male hi1 no2
#' @param a.m_0b insecticide exposure male no1 lo2
#' @param a.m_0B insecticide exposure male no1 hi2
#' @param a.m_ab insecticide exposure male lo1 lo2
#' @param a.m_AB insecticide exposure male hi1 hi2
#' @param a.m_Ab insecticide exposure male hi1 lo2
#' @param a.m_aB insecticide exposure male lo1 hi2
#' @param a.f_00 insecticide exposure female no1 no2
#' @param a.f_a0 insecticide exposure female lo1 no2
#' @param a.f_A0 insecticide exposure female hi1 no2
#' @param a.f_0b insecticide exposure female no1 lo2
#' @param a.f_0B insecticide exposure female no1 hi2
#' @param a.f_ab insecticide exposure female lo1 lo2
#' @param a.f_AB insecticide exposure female hi1 hi2
#' @param a.f_Ab insecticide exposure female hi1 lo2
#' @param a.f_aB insecticide exposure female lo1 hi2
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
#' @param niche_00 insecticide niche toggle no1 no2 0=off 1=on
#' @param niche_a0 insecticide niche toggle no1 no2 0=off 1=on
#' @param niche_A0 insecticide niche toggle hi1 no2 0=off 1=on
#' @param niche_0b insecticide niche toggle no1 lo2 0=off 1=on
#' @param niche_0B insecticide niche toggle no1 hi2 0=off 1=on
#' @param niche_ab insecticide niche toggle lo1 lo2 0=off 1=on
#' @param niche_AB insecticide niche toggle hi1 hi2 0=off 1=on
#' @param niche_Ab insecticide niche toggle hi1 lo2 0=off 1=on
#' @param niche_aB insecticide niche toggle lo1 hi2 0=off 1=on
#' @param sexLinked whether resistance is sex linked, default=0(FALSE)
#' @param maleExposureProp male exposure as a propoertion of female, default 1 for same, likely <1  
#' @param correctMixDeployProp proportion of correct deployment of mixtures, 
#'    if <1 other portion divided between single insecticides
#' @param resistance_coeff_I1 effect of resistance in overcoming insecticide 1 effectiveness
#' @param resistance_coeff_I2 effect of resistance in overcoming insecticide 2 effectiveness    
#' 
#' @return named vector
#' @export
#' 
setInputOneScenario <- function( calibration = 100,
                                 max_gen = 100,
                                 coll.fitvals = 1,
                                 save.fitvals = 0,
                                 P_1 = 0.001,
                                 P_2 = 0.001,
                                 recomb_rate = 0.5,
                                 a = NULL,
                                 #exposure = NULL, #has to go later to add on end of input to keep Beths code working
                                 a.m_00 = 0.1,
                                 a.m_a0 = 0,
                                 a.m_A0 = 0,
                                 a.m_0b	=	0	,
                                 a.m_0B	=	0	,
                                 a.m_ab	=	0	,
                                 a.m_AB	=	0.9	,
                                 a.m_Ab	=	0	,
                                 a.m_aB	=	0	,
                                 a.f_00	=	0.1	,
                                 a.f_a0	=	0	,
                                 a.f_A0	=	0	,
                                 a.f_0b	=	0	,
                                 a.f_0B	=	0	,
                                 a.f_ab	=	0	,
                                 a.f_AB	=	0.9	,
                                 a.f_Ab	=	0	,
                                 a.f_aB	=	0	,
                                 phi.SS1_a0	=	0	,
                                 phi.SS1_A0	=	1	,
                                 phi.SS2_0b	=	0	,
                                 phi.SS2_0B	=	1	,
                                 W.SS1_00	=	1	,
                                 W.SS2_00	=	1	,
                                 h.RS1_00	=	0	,
                                 h.RS1_a0	=	0	,
                                 h.RS1_A0	=	1	,
                                 h.RS2_00	=	0	,
                                 h.RS2_0b	=	0	,
                                 h.RS2_0B	=	1	,
                                 s.RR1_a0	=	0	,
                                 s.RR1_A0	=	1	,
                                 s.RR2_0b	=	0	,
                                 s.RR2_0B	=	1	,
                                 z.RR1_00	=	0	,
                                 z.RR2_00	=	0	,
                                 niche_00	=	1	,
                                 niche_a0	=	1	,
                                 niche_A0	=	1	,
                                 niche_0b	=	1	,
                                 niche_0B	=	1	,
                                 niche_ab	=	1	,
                                 niche_AB	=	1	,
                                 niche_Ab	=	1	,
                                 niche_aB	=	1 ,
                                 sexLinked = 0,
                                 maleExposureProp = 1,
                                 correctMixDeployProp = 1,
                                 exposure = 0.9,
                                 #14/6/16
                                 resistance_coeff_I1 = 0.5,
                                 resistance_coeff_I2 = 0.5
                                 )
{
  
  #input <- matrix( ncol=1, nrow=56 )
  #now set size to 1 less than num args, the array a is not included in input
  #otherwise I kept getting bug when adding extra args
  input <- matrix( ncol=1, nrow=length(formals())-1)
  
  input[1] <- calibration
  input[2] <- max_gen
  input[3] <- coll.fitvals
  input[4] <- save.fitvals
  input[5] <- P_1
  input[6] <- P_2
  input[7] <- recomb_rate
  #allowing exposure parameters to be set from a single array
  if (!is.null(a))
  {
    a.m_00 <- a['m','0','0']
    a.m_a0 <- a['m','a','0']
    a.m_A0 <- a['m','A','0']
    a.m_0b <- a['m','0','b']
    a.m_0B <- a['m','0','B']
    a.m_ab <- a['m','a','b']
    a.m_AB <- a['m','A','B']
    a.m_Ab <- a['m','A','b']
    a.m_ab <- a['m','a','b']
    a.f_00 <- a['f','0','0']
    a.f_a0 <- a['f','a','0']
    a.f_A0 <- a['f','A','0']
    a.f_0b <- a['f','0','b']
    a.f_0B <- a['f','0','B']
    a.f_ab <- a['f','a','b']
    a.f_AB <- a['f','A','B']
    a.f_Ab <- a['f','A','b']
    a.f_ab <- a['f','a','b']
  }
  input[8] <- a.m_00
  input[9] <- a.m_a0
  input[10] <- a.m_A0
  input[	11	] <-	a.m_0b
  input[	12	] <-	a.m_0B
  input[	13	] <-	a.m_ab
  input[	14	] <-	a.m_AB
  input[	15	] <-	a.m_Ab
  input[	16	] <-	a.m_ab
  input[	17	] <-	a.f_00
  input[	18	] <-	a.f_a0
  input[	19	] <-	a.f_A0
  input[	20	] <-	a.f_0b
  input[	21	] <-	a.f_0B
  input[	22	] <-	a.f_ab
  input[	23	] <-	a.f_AB
  input[	24	] <-	a.f_Ab
  input[	25	] <-	a.f_ab
  input[	26	] <-	phi.SS1_a0
  input[	27	] <-	phi.SS1_A0
  input[	28	] <-	phi.SS2_0b
  input[	29	] <-	phi.SS2_0B
  input[	30	] <-	W.SS1_00
  input[	31	] <-	W.SS2_00
  input[	32	] <-	h.RS1_00
  input[	33	] <-	h.RS1_a0
  input[	34	] <-	h.RS1_A0
  input[	35	] <-	h.RS2_00
  input[	36	] <-	h.RS2_0b
  input[	37	] <-	h.RS2_0B
  input[	38	] <-	s.RR1_a0
  input[	39	] <-	s.RR1_A0
  input[	40	] <-	s.RR2_0b
  input[	41	] <-	s.RR2_0B
  input[	42	] <-	z.RR1_00
  input[	43	] <-	z.RR2_00
  input[	44	] <-	niche_00
  input[	45	] <-	niche_a0
  input[	46	] <-	niche_A0
  input[	47	] <-	niche_0b
  input[	48	] <-	niche_0B
  input[	49	] <-	niche_ab
  input[	50	] <-	niche_AB
  input[	51	] <-	niche_Ab
  input[	52	] <-	niche_aB
  
  input[	53	] <-	sexLinked
  
  #22/1/16 new variables for extended experiment
  #they aren't used in runModel2() but are needed for post run analyses
  input[	54	] <-	maleExposureProp
  input[	55	] <-	correctMixDeployProp
  #1/2/16 allowing saving of single exposure param just for use in post-run analyses
  input[	56	] <-	exposure  
  
  #14/6/16
  input[	57	] <-	resistance_coeff_I1  
  input[	58	] <-	resistance_coeff_I2  
    
  a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
  #if ( a.m != 1 ){
  #these warnings allow for rounding differences
  if ( !isTRUE( all.equal(1, a.m  ))){
  	stop("male exposures must total one, currently: ", a.m )
  	}
  
  a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
  if ( !isTRUE( all.equal(1, a.f  ))){
    stop( "female exposures must total one, currently: ", a.f )
  	}
 
 #set rownames of input to the use variable names
 #trying to avoid code repetition and potential for confusion
 #BEWARE this relies on the arguments being specified in the function in the correct order
 rnames <- names(formals())
 #remove the array a from the arg list
 rnames <- rnames[rnames!="a"]
 rownames(input) <- rnames 
  
 return(input)
  
}