#' set inputs for a sensitivity analysis of multiple scenarios
#' 
#' creates a multi-column input matrix which can be passed to runModel() 
#' You need to pass a range for which parameters you want, as c(min,max).
#' Scenarios will be created by selecting from a uniform distribution specified by this range.
#' Any parameters not specified will be set to their default values.
#' TODO: I might be able to allow parameters to be set to non default values from here too.
#' 
#' @param nScenarios number of scenarios to generate 
#' @param calibration one of a limited set of integers effecting how scenarios are run
#' @param max_gen number of generations
#' @param coll.fitvals save fitness values in a matrix 0/1
#' @param save.fitvals save fitness values to an external .csv 0/1
#' @param P_1 locus 1 frequency of resistance allele
#' @param P_2 locus 2 frequency of resistance allele
#' @param recomb_rate recombination rate
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
#' 
#' @examples
#' setInputSensiScenarios( nScenarios=2, h.RS1_A0=c(0,1) )
#' setInputSensiScenarios( nScenarios=3, P_1=c(0,0.5), P_2=c(0,0.5) )
#' @return a matrix of input scenarios
#' @export
#' 
setInputSensiScenarios <- function( nScenarios = 10,
                                 calibration = 100,
                                 max_gen = 100,
                                 coll.fitvals = 1,
                                 save.fitvals = 0,
                                 P_1 = 0.001,
                                 P_2 = 0.001,
                                 recomb_rate = 0.5,
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
                                 niche_a0	=	0	,
                                 niche_A0	=	0	,
                                 niche_0b	=	0	,
                                 niche_0B	=	0	,
                                 niche_ab	=	0	,
                                 niche_AB	=	1	,
                                 niche_Ab	=	0	,
                                 niche_aB	=	0
                                 )
{
  #create empty object to hold input matrix for return
  input <- NULL

  #todo now can I get just the args that are passed
  #then I will want to select a value for them from a uniform distribution
  #and pass it to setInputOneScenario()
  funcCall <- match.call()
  #missing off first 2, but dangerous can't assume nScenarios will be first or specified
  #passedArgs <- as.list(funcCall)[-c(1:2)]
  #missing first which is func name
  passedArgs <- as.list(funcCall)[-1] 
  #trying to exclude nScenarios
  #names(passedArgs)
  if( "nScenarios" %in% names(passedArgs))
    passedArgs <- passedArgs[-which(names(passedArgs)=="nScenarios")]
  
  #TODO: I might be able to allow parameters to be set to non default values from here too.
  #I would just need to go through the args and find which had a length of 1 rather than 2
  #would need to use eval() to get around call trickiness
  
  for(i in 1:nScenarios)
  {
    #try to select a value for each passed arg from within the passed range
    #todo can probably replace this loop
    #copy so that the list element names are copied
    argVals <- passedArgs
    for(argNum in 1:length(passedArgs))
    { 
      #I might be able to do this more efficiently by passing nScenarios as first arg to runif
      #to generate multiple random samples at once
      #but then how to get them into the input dataframe
      
      #use eval to convert language object of c(0, 1) into something I can use
      argVals[argNum] <- runif(1, min=eval(passedArgs[[argNum]])[1], max=eval(passedArgs[[argNum]])[2])
    }
    
    #will passing argVals like this work ??
    #inputOneScenario <- setInputOneScenario( argVals ) 
    #above doesn't work, below seems to
    inputOneScenario <- do.call(setInputOneScenario, argVals)     
    
    
    input <- cbind(input, inputOneScenario)
  }
  
  #if i wanted this to run the model too
  #listOut <- runModel(input)
  #plotallele.freq.andy(listOut)
  
  return(input)
  
}