#' create input parameters matrix from a file or hardcoded scenarios
#' 
#' Creates a matrix with parameters in rows and scenarios in columns.  
#' Either from a csv file or hardcoded scenarios set by the calibration argument.
#' 
#' @param params.csv whether parameters are defined in an input file TRUE/FALSE
#' @param inFile parameters filename
#' @param calibration one of a limited set of integers effecting how scenarios are run 
#' @param save.fitvals whether to save fitness scores to csv 0=no, 1=yes 
#' 
#' @seealso \code{\link{setInputOneScenario}} and \code{\link{setInputSensiScenarios}} for creating input objects from passed arguments
#' 
#' @return matrix
#' @export

createInputMatrix <- function(params.csv,
                        inFile = system.file("extdata","input.parameters.csv", package="resistance"),
                        calibration = 1012,
                        save.fitvals = 0 ){
  
  #todo may be able to combine params.csv and file arguments
  
  ## if parameters from input file - 
  #params.csv <- TRUE #testing whether this always needs to be done
  if( params.csv )
  {
    #todo add check that file exists
    
    input <- read.csv( inFile, header=T, stringsAsFactors=F )    
    # make into matrix from data.frame
    input <- make.matrix(input, input$Input)
    return(input)
  }

  #seeing if I need to initialise the matrix outside of the callibration loops below
  #input <- matrix()

  ## Scenario 1 Calibration 1011 - Fig 1 Curtis ####
  if( !params.csv && calibration == 1011 ){  
    
    input <- matrix( ncol=2, nrow=53 )
    
    input[1,1] <- calibration   # calibration a code to run specific set of scenarios
    input[2,1] <- 18            # max_gen number of generations
    input[3,1] <- 1             # coll.fitvals save fitness values in a matrix 0/1
    input[4,1] <- save.fitvals	# save.fitvals save fitness values to an external .csv 0/1
    input[5,1] <- 0.001	        # P_1 locus 1 frequency of resistance allele
    input[6,1] <- 0.001	        # P_2 locus 2 frequency of resistance allele
    input[7,1] <- 0.5		        # recomb_rate recombination rate
    
    input[8,1] <- 0.1  	# a.m_00 insecticide exposure male no1 no2
    input[9,1] <- 0     # a.m_a0 insecticide exposure male lo1 no2
    input[10,1] <- 0    # a.m_A0 insecticide exposure male hi1 no2
    input[11,1] <- 0    # a.m_0b insecticide exposure male no1 lo2
    input[12,1] <- 0    # a.m_0B insecticide exposure male no1 hi2
    input[13,1] <- 0    # a.m_ab insecticide exposure male lo1 lo2 
    input[14,1] <- 0.9	# a.m_AB insecticide exposure male hi1 hi2
    input[15,1] <- 0    # a.m_Ab insecticide exposure male hi1 lo2 
    input[16,1] <- 0    # a.m_aB insecticide exposure male lo1 hi2 

    input[17,1] <- 0.1  	# a.f_00 insecticide exposure female no1 no2
    input[18,1] <- 0     # a.f_a0 insecticide exposure female lo1 no2
    input[19,1] <- 0    # a.f_A0 insecticide exposure female hi1 no2
    input[20,1] <- 0    # a.f_0b insecticide exposure female no1 lo2
    input[21,1] <- 0    # a.f_0B insecticide exposure female no1 hi2
    input[22,1] <- 0    # a.f_ab insecticide exposure female lo1 lo2 
    input[23,1] <- 0.9	# a.f_AB insecticide exposure female hi1 hi2
    input[24,1] <- 0    # a.f_Ab insecticide exposure female hi1 lo2 
    input[25,1] <- 0    # a.f_aB insecticide exposure female lo1 hi2         

    input[26,1] <- 0  # phi.SS1_a0 Baseline fitness of SS1 in a0
    input[27,1] <- 1	# phi.SS1_A0 Baseline fitness of SS1 in A0
    input[28,1] <- 0  # phi.SS2_0b Baseline fitness of SS2 in 0b
    input[29,1] <- 1	# phi.SS2_0B Baseline fitness of SS2 in 0B
    
    input[30,1] <- 1  # W.SS1_00 Fitness of SS1 in 00 (no insecticide)
    input[31,1] <- 1  # W.SS2_00 Fitness of SS2 in 00 (no insecticide)
    
    input[32,1] <- 0  # h.RS1_00 Dominance coefficient locus1 in 00
    input[33,1] <- 0  # h.RS1_a0 Dominance coefficient locus1 in a0
    input[34,1] <- 1	# h.RS1_A0 Dominance coefficient locus1 in A0
    input[35,1] <- 0  # h.RS2_00 Dominance coefficient locus2 in 00
    input[36,1] <- 0  # h.RS2_0b Dominance coefficient locus2 in 0b
    input[37,1] <- 1  # h.RS2_0B Dominance coefficient locus2 in 0B
    
    input[38,1] <- 0  # s.RR1_a0 Selection coefficient locus1 in a
    input[39,1] <- 1	# s.RR1_A0 Selection coefficient locus1 in A
    input[40,1] <- 0  # s.RR2_0b Selection coefficient locus2 in b
    input[41,1] <- 1	# s.RR2_0B Selection coefficient locus2 in B

    input[42,1] <- 0  # z.RR1_00 fitness cost of resistance allele 1 in insecticide free environment
    input[43,1] <- 0  # z.RR2_00 fitness cost of resistance allele 2 in insecticide free environment
    
    input[44,1] <- 1	# niche_00 insecticide niche toggle no1 no2 0=off 1=on
    input[45,1] <- 0  # niche_a0 insecticide niche toggle no1 no2 0=off 1=on
    input[46,1] <- 0  # niche_A0 insecticide niche toggle hi1 no2 0=off 1=on
    input[47,1] <- 0  # niche_0b insecticide niche toggle no1 lo2 0=off 1=on
    input[48,1] <- 0  # niche_0B insecticide niche toggle no1 hi2 0=off 1=on
    input[49,1] <- 0  # niche_ab insecticide niche toggle lo1 lo2 0=off 1=on
    input[50,1] <- 1  # niche_AB insecticide niche toggle hi1 hi2 0=off 1=on
    input[51,1] <- 0  # niche_Ab insecticide niche toggle hi1 lo2 0=off 1=on
    input[52,1] <- 0  # niche_aB insecticide niche toggle lo1 hi2 0=off 1=on
 
    input[53,1] <- 0  # sexLinked 0=FALSE 1=TRUE
    
    #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
    #if ( a.m != 1 ){		 
    #	print( paste("Error in male exposures: must total one: ", a.m) )
    #	}
    
    #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
    #if ( a.f != 1 ){		 
    #	print( paste("Error in female exposures: must total one: ", a.f) )
    #	}
    
    #todo check setting of all scenario2 params to scenario1
    input[,2] <- input[,1]
  }
  
  # Calibration 1012 - Fig 2 Curtis: Sequential (B,A) and then combination treatments
  if( !params.csv && calibration == 1012 ){
    input <- matrix( ncol=3, nrow=53 )
    colnames(input) <- c("B (HCH)", "A (DDT)","Combination")
    ### Sequential Treatment 
    ## Column 1 - Scenario 1, Insecticide B (Dieldrin)
    input[1,1] <- calibration
    
    ### Results Matrix ###
    # Give number of generations
    input[2,1] <- 70
    
    ### Saving Parameters ###
    # To save parameters in a matrix, set coll.fitvals to 1 ##
    input[3,1] <- 1
    # to save this matrix to an external .csv set to 1, will overwrite ##
    input[4,1] <- save.fitvals
    
    ### Genotype Frequencies ###
    ## setting up to get proportions of genotypes in population
    ## User enters value of P - frequency of resistance allele at locus 1&2 respectively
    input[5,1] <- 0.01	# locus 1
    input[6,1] <- 0.01	# locus 2
    
    ## From this, the function HW will find the proportions of each genotype
    ## RR = p, RS = pq, SS = q
    ## P = p = R
    
    ## Recombination ##
    input[7,1] <- 0.5		# recombination rate
    
    ### Insecticides are represented as a and b ###
    ### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###
    
    ### Exposure Values ###
    ## Exposure levels of males and females to each insecticide niche ##
    # males
    input[8,1] <- 0.1	#Niche -,-
    
    input[9,1] <- 0
    input[10,1] <- 0
    
    input[11,1] <- 0
    input[12,1] <- 0.9  #Niche 0,B
    
    input[13,1] <- 0
    input[14,1] <- 0
    
    input[15,1] <- 0
    input[16,1] <- 0
    
    #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
    #if ( a.m != 1 ){		 
    #	print( paste("Error in male exposures: must total one: ", a.m) )
    
    #	}else{
    #		print( paste( "Male exposures total 1: ", a.m ))
    #		}
    
    
    # females
    input[17,1] <- 0.1	#niche -,-
    
    input[18,1] <- 0
    input[19,1] <- 0
    
    input[20,1] <- 0
    input[21,1] <- 0.9	#Niche 0,B
    
    input[22,1] <- 0
    input[23,1] <- 0
    
    input[24,1] <- 0
    input[25,1] <- 0
    
    #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
    #if ( a.f != 1 ){		 
    #	print( paste("Error in female exposures: must total one: ", a.f) )
    #	
    #	}else{
    #		print( paste( "female exposures total 1: ", a.f ))
    #		}
    
    
    ### Selection from distributions ###
    ### Fitness Values ###
    ## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
    ## User entered fitness values to allow some survival of homozygote susceptible due to chance
    # set as variables to be used in function calls/equations
    # phi = baseline fitness value
    input[26,1] <- 0
    input[27,1] <- 0.73	#Phi SS1 in A
    
    input[28,1] <- 0
    input[29,1] <- 1	#Phi SS2 in B
    
    # fitness of SS in environment with no insecticide are set to 1
    # W = fitness value
    input[30,1] <- 1
    input[31,1] <- 1
    
    ## Dominance and selection coefficients
    ## needed to find fitness values of genotype in exposure to relating insecticide 
    # h = dominance coefficient
    input[32,1] <- 0
    
    input[33,1] <- 0
    input[34,1] <- 0.17	#Dominance coefficient in A
    
    input[35,1] <- 0
    
    input[36,1] <- 0
    input[37,1] <- 0.0016	#Dominance coefficient in B
    
    # s = selection coefficient
    input[38,1] <- 0
    input[39,1] <- 0.23	#in A
    
    input[40,1] <- 0
    input[41,1] <- 0.43	#in B
    
    # z = fitness cost of resistance allele in insecticide free environment
    input[42,1] <- 0
    input[43,1] <- 0
    
    ### Toggle Insecticide Niches on and off ###
    ## Allows for setting of specific combinations of insecticide niches to be used
    ## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    ## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    input[44,1] <- 1	#Niche 00
    
    input[45,1] <- 0
    input[46,1] <- 0
    
    input[47,1] <- 0
    input[48,1] <- 1	#Niche 0B
    
    input[49,1] <- 0
    input[50,1] <- 0
    
    input[51,1] <- 0
    input[52,1] <- 0
    
    input[53,1] <- 0  # sexLinked 0=FALSE 1=TRUE
    
    
    ## Scenario 2 - Insecticide A (DDT) ####
    input[1,2] <- calibration
    
    ### Results Matrix ###
    # Give number of generations
    input[2,2] <- 70
    
    ### Saving Parameters ###
    # To save parameters in a matrix, set coll.fitvals to 1 ##
    input[3,2] <- 1
    # to save this matrix to an external .csv set to 1, will overwrite ##
    input[4,2] <- save.fitvals		
    
    ### Genotype Frequencies ###
    ## setting up to get proportions of genotypes in population
    ## User enters value of P - frequency of resistance allele at locus 1&2 respectively
    input[5,2] <- 0.01	# locus 1
    input[6,2] <- 0.01	# locus 2
    
    ## From this, the function HW will find the proportions of each genotype
    ## RR = p, RS = pq, SS = q
    ## P = p = R
    
    ## Recombination ##
    input[7,2] <- 0.5		# recombination rate
    
    ### Insecticides are represented as a and b ###
    ### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###
    
    ### Exposure Values ###
    ## Exposure levels of males and females to each insecticide niche ##
    # males
    input[8,2] <- 0.1	#Niche -,-
    
    input[9,2] <- 0
    input[10,2] <- 0.9	#Niche A,0
    
    input[11,2] <- 0
    input[12,2] <- 0
    
    input[13,2] <- 0
    input[14,2] <- 0
    
    input[15,2] <- 0
    input[16,2] <- 0
    
    #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
    #if ( a.m != 1 ){		 
    #	print( paste("Error in male exposures: must total one: ", a.m) )
    
    #	}else{
    #		print( paste( "Male exposures total 1: ", a.m ))
    #		}
    
    
    # females
    input[17,2] <- 0.1	#niche -,-
    
    input[18,2] <- 0
    input[19,2] <- 0.9	#Niche A,0
    
    input[20,2] <- 0
    input[21,2] <- 0
    
    input[22,2] <- 0
    input[23,2] <- 0
    
    input[24,2] <- 0
    input[25,2] <- 0
    
    #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
    #if ( a.f != 1 ){		 
    #	print( paste("Error in female exposures: must total one: ", a.f) )
    #	
    #	}else{
    #		print( paste( "female exposures total 1: ", a.f ))
    #		}
    
    
    ### Selection from distributions ###
    ### Fitness Values ###
    ## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
    ## User entered fitness values to allow some survival of homozygote susceptible due to chance
    # set as variables to be used in function calls/equations
    # phi = baseline fitness value
    input[26,2] <- 0
    input[27,2] <- 0.73	#Phi SS1 in A
    
    input[28,2] <- 0
    input[29,2] <- 1	#Phi SS2 in B
    
    # fitness of SS in environment with no insecticide are set to 1
    # W = fitness value
    input[30,2] <- 1
    input[31,2] <- 1
    
    ## Dominance and selection coefficients
    ## needed to find fitness values of genotype in exposure to relating insecticide 
    # h = dominance coefficient
    input[32,2] <- 0
    
    input[33,2] <- 0
    input[34,2] <- 0.17	#Dominance coefficient in A
    
    input[35,2] <- 0
    
    input[36,2] <- 0
    input[37,2] <- 0.0016	#Dominance coefficient in B
    
    # s = selection coefficient
    input[38,2] <- 0
    input[39,2] <- 0.23	#in A
    
    input[40,2] <- 0
    input[41,2] <- 0.43	#in B
    
    # z = fitness cost of resistance allele in insecticide free environment
    input[42,2] <- 0
    input[43,2] <- 0
    
    ### Toggle Insecticide Niches on and off ###
    ## Allows for setting of specific combinations of insecticide niches to be used
    ## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    ## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    input[44,2] <- 1	#Niche 00
    
    input[45,2] <- 0
    input[46,2] <- 1	#Niche A0
    
    input[47,2] <- 0
    input[48,2] <- 0
    
    input[49,2] <- 0
    input[50,2] <- 0
    
    input[51,2] <- 0
    input[52,2] <- 0
    
    input[53,1] <- 0  # sexLinked 0=FALSE 1=TRUE
    
    ### Scenario 3 Combination Treatment ####
    input[1,3] <- calibration
    
    ### Results Matrix ###
    # Give number of generations
    input[2,3] <- 160
    
    ### Saving Parameters ###
    # To save parameters in a matrix, set coll.fitvals to 1 ##
    input[3,3] <- 1
    # to save this matrix to an external .csv set to 1, will overwrite ##
    input[4,3] <- save.fitvals
    
    ### Genotype Frequencies ###
    ## setting up to get proportions of genotypes in population
    ## User enters value of P - frequency of resistance allele at locus 1&2 respectively
    input[5,3] <- 0.01	# locus 1
    input[6,3] <- 0.01	# locus 2
    
    ## From this, the function HW will find the proportions of each genotype
    ## RR = p, RS = pq, SS = q
    ## P = p = R
    
    ## Recombination ##
    input[7,3] <- 0.5		# recombination rate
    
    ### Insecticides are represented as a and b ###
    ### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###
    
    ### Exposure Values ###
    ## Exposure levels of males and females to each insecticide niche ##
    # males
    input[8,3] <- 0.1	#Niche -,-
    
    input[9,3] <- 0
    input[10,3] <- 0
    
    input[11,3] <- 0
    input[12,3] <- 0
    
    #todo check this is wanted
    input[13,3] <-
      input[14,3] <- 0.9	#Niche A,B
    
    input[15,3] <- 0
    input[16,3] <- 0
    
    #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
    #if ( a.m != 1 ){		 
    #	print( paste("Error in male exposures: must total one: ", a.m) )
    
    #	}else{
    #		print( paste( "Male exposures total 1: ", a.m ))
    #		}
    
    
    # females
    input[17,3] <- 0.1	#niche -,-
    
    input[18,3] <- 0
    input[19,3] <- 0
    
    input[20,3] <- 0
    input[21,3] <- 0
    
    input[22,3] <- 0
    input[23,3] <- 0.9	#niche A,B
    
    input[24,3] <- 0
    input[25,3] <- 0
    
    #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
    #if ( a.f != 1 ){		 
    #	print( paste("Error in female exposures: must total one: ", a.f) )
    #	
    #	}else{
    #		print( paste( "female exposures total 1: ", a.f ))
    #		}
    
    
    ### Selection from distributions ###
    ### Fitness Values ###
    ## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
    ## User entered fitness values to allow some survival of homozygote susceptible due to chance
    # set as variables to be used in function calls/equations
    # phi = baseline fitness value
    input[26,3] <- 0
    input[27,3] <- 0.73	#Phi SS1 in A
    
    input[28,3] <- 0
    input[29,3] <- 1	#Phi SS2 in B
    
    # fitness of SS in environment with no insecticide are set to 1
    # W = fitness value
    input[30,3] <- 1
    input[31,3] <- 1
    
    ## Dominance and selection coefficients
    ## needed to find fitness values of genotype in exposure to relating insecticide 
    # h = dominance coefficient
    input[32,3] <- 0
    
    input[33,3] <- 0
    input[34,3] <- 0.17	#Dominance coefficient in A
    
    input[35,3] <- 0
    
    input[36,3] <- 0
    input[37,3] <- 0.00016	#Dominance coefficient in B
    
    # s = selection coefficient
    input[38,3] <- 0
    input[39,3] <- 0.23	#in A
    
    input[40,3] <- 0
    input[41,3] <- 0.43	#in B
    
    # z = fitness cost of resistance allele in insecticide free environment
    input[42,3] <- 0
    input[43,3] <- 0
    
    ### Toggle Insecticide Niches on and off ###
    ## Allows for setting of specific combinations of insecticide niches to be used
    ## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    ## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    input[44,3] <- 1	#Niche 00
    
    input[45,3] <- 0
    input[46,3] <- 0
    
    input[47,3] <- 0
    input[48,3] <- 0
    
    input[49,3] <- 0
    input[50,3] <- 1	#Niche AB
    
    input[51,3] <- 0
    input[52,3] <- 0
    
    input[53,1] <- 0  # sexLinked 0=FALSE 1=TRUE
  }
  
  if( !params.csv ){
    rownames(input) <- c( "Calibration (100 default)","Number of generations ",                    
                          "Collect fitness scores in matrix (1/0)","Export fitness scores matrix to .csv (1/0)",
                          "Frequency of R at locus 1","Frequency of R at locus 2","Recombination Rate","Exposure Males -,-","Exposure Males a,-",                      
                          "Exposure Males A,-","Exposure Males -,b","Exposure Males -,B","Exposure males a,b","Exposure Males A,B",                        
                          "Exposure Males A,b","Exposure Males a,B","Exposure Females -,-","Exposure Females a,-","Exposure Females A,-",                      
                          "Exposure Females -,b","Exposure Females -,B","Exposure Females a,b","Exposure Females A,B","Exposure Females A,b",                      
                          "Exposure Females a,B","Baseline fitness of SS1 in a,-","Baseline fitness of SS1 in A,-","Baseline fitness of SS2 in -,b",
                          "Baseline fitness of SS2 in -,B","Fitness of SS1 in -,-","Fitness of SS2 in -,-","Dominance coefficient L1 in -,-",         
                          "Dominance coefficient L1 in a,-","Dominance coefficient L1 in A,-","Dominance coefficient L2 in -,-","Dominance coefficient L2 in -,b",           
                          "Dominance coefficient L2 in -,B","Selection coefficient L1 in a","Selection coefficient L1 in A","Selection coefficient L2 in b",            
                          "Selection coefficient L2 in B","Fitness cost of R at L1 in -,-","Fitness cost of R at L2 in -,-","Niche -,- on (1/0)",                       
                          "Niche a,- on","Niche A,- on","Niche -,b on","Niche -,B on","Niche a,b on","Niche A,B on","Niche A,b on","Niche a,B on","sexLinked" )
  }
  
  
  return(input)
}
