#' run the model scenarios specified in the input object (refactored)
#' 
#' refactored version starting to use genotype and niche arrays to reduce code volume
#' run model scenarios and put the results for each in a results object
#' 
#' @param input a matrix with parameters in rows and scenarios in columns
#' @param calibration one of a limited set of integers effecting how scenarios are run
#' @param produce.plots whether to produce plots
#' @param savePlots whether to save plots to hardcoded filenames

#' @examples 
#' input <- setInputOneScenario()
#' tst <- runModel2(input)
#' @return a list of 3 lists of one or more scenarios: results, genotype and fitness. e.g. listOut$results[1] gives a results matrix for the first scenario
#' @export

runModel2 <- function(input,
                     calibration,
                     produce.plots = TRUE,
                     savePlots=FALSE){
 
  ### Lists to store results ####
  #replacing 3 results lists with a list of 3 lists
  listOut <- list( results=list(), fitness=list(), genotype=list() )

  
  ## Run through columns of matrix "input" to run model for each scenario set
  for (i in 1:ncol( input ) ){
    ### Calibrations ###
    calibration <- input[1,i]
    
    ### Results Matrix ###
    # Give number of generations
    max_gen <- input[2,i]
    
    ### Saving Parameters ###
    # To save parameters in a matrix, set coll.fitvals to 1 ##
    coll.fitvals <- input[3,i]
    # To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
    save.fitvals <- input[4,i]		## will OVERWRITE every time the matrix runs
    ## so files will need to be renamed/moved to be kept.
    
    
    ### Genotype Frequencies ###
    ## setting up to get proportions of genotypes in population
    ## User enters value of P - frequency of resistance allele at locus 1&2 respectively
    P_1 <- input[5,i]	# locus 1
    P_2	<- input[6,i]	# locus 2
    
    ## From this, the function HW will find the proportions of each genotype
    ## RR = p, RS = pq, SS = q
    ## P = p = R
    
    ## Recombination ##
    recomb_rate <- input[7,i]		# recombination rate
    
    
    ### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###

    ## Exposure levels of males and females to each insecticide niche ##
    #!r
    a <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
    Wloci <- createArray2( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    Wniche <- createArray2( locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )    
    Windiv <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
    niche <- createArray2( niche1=c('0','a','A'), niche2=c('0','b','B') )
    
    s <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi') ) #or just lo hi
    phi <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    h <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    z <- createArray2(locusNum=c(1,2))
    
    # males
    a.m_00 <- input[8,i]
    a['m','0','0'] <- input[8,i]
    
    a.m_a0 <- input[9,i]
    a.m_A0 <- input[10,i]
    a['m','a','0'] <- input[9,i]
    a['m','A','0'] <- input[10,i]
    
    a.m_0b <- input[11,i]
    a.m_0B <- input[12,i]
    a['m','0','b'] <- input[11,i]
    a['m','0','B'] <- input[12,i]
    
    a.m_ab <- input[13,i]
    a.m_AB <- input[14,i]
    a['m','a','b'] <- input[13,i]
    a['m','A','B'] <- input[14,i]    
    
    a.m_Ab <- input[15,i]
    a.m_aB <- input[16,i]
    a['m','A','b'] <- input[15,i]
    a['m','a','B'] <- input[16,i]      
    
    #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
    #if ( a.m != 1 ){		 
    #	print( paste("Error in male exposures: must total one: ", a.m) )
    #	}
    if ( sum(a['m',,]) != 1 ){		 
    	stop( paste("Error in male exposures: must total one: ", sum(a['m',,])) )
    	}
    
    # females
    a.f_00 <- input[17,i]
    a['f','0','0'] <- input[17,i]
    
    a.f_a0 <- input[18,i]
    a.f_A0 <- input[19,i]
    a['f','a','0'] <- input[18,i]
    a['f','A','0'] <- input[19,i]
    
    a.f_0b <- input[20,i]
    a.f_0B <- input[21,i]
    a['f','0','b'] <- input[20,i]
    a['f','0','B'] <- input[21,i]
    
    a.f_ab <- input[22,i]
    a.f_AB <- input[23,i]
    a['f','a','b'] <- input[22,i]
    a['f','A','B'] <- input[23,i] 
    
    a.f_Ab <- input[24,i]
    a.f_aB <- input[25,i]
    a['f','A','b'] <- input[24,i]
    a['f','a','B'] <- input[25,i] 
    
    
    #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
    #if ( a.f != 1 ){		 
    #	print( paste("Error in female exposures: must total one: ", a.f) )
    #	}
    if ( sum(a['f',,]) != 1 ){		 
      stop( paste("Error in female exposures: must total one: ", sum(a['f',,])) )
    }
    
    ### Selection from distributions ###
    ### Fitness Values ###
    ## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
    ## User entered fitness values to allow some survival of homozygote susceptible due to chance
    # set as variables to be used in function calls/equations
    # phi = baseline fitness value
    phi.SS1_a0 <- input[26,i]
    phi.SS1_A0 <- input[27,i]
    phi.SS2_0b <- input[28,i]
    phi.SS2_0B <- input[29,i]
    #!r
    phi[1,'lo'] <- input[26,i]
    phi[1,'hi'] <- input[27,i]
    phi[2,'lo'] <- input[28,i]
    phi[2,'hi'] <- input[29,i]
    
    
    # fitness of SS in environment with no insecticide are set to 1
    W.SS1_00 <- input[30,i]
    W.SS2_00 <- input[31,i]
    #!r
    Wloci['SS1','no'] <- input[30,i]
    Wloci['SS2','no'] <- input[31,i]    
    
    ## Dominance and selection coefficients
    ## needed to find fitness values of genotype in exposure to relating insecticide 
    # h = dominance coefficient
    h.RS1_00 <- input[32,i]
    h.RS1_a0 <- input[33,i]
    h.RS1_A0 <- input[34,i]
    h.RS2_00 <- input[35,i]
    h.RS2_0b <- input[36,i]
    h.RS2_0B <- input[37,i]
    #!r
    h[1,'no'] <- input[32,i]
    h[1,'lo'] <- input[33,i]
    h[1,'hi'] <- input[34,i]
    h[2,'no'] <- input[35,i]
    h[2,'lo'] <- input[36,i]
    h[2,'hi'] <- input[37,i]    
    
    
    # s = selection coefficient
    s.RR1_a0 <- input[38,i]
    s.RR1_A0 <- input[39,i]
    s.RR2_0b <- input[40,i]
    s.RR2_0B <- input[41,i]
    #!r
    s[1,'lo'] <- input[38,i]
    s[1,'hi'] <- input[39,i]
    s[2,'lo'] <- input[40,i]
    s[2,'hi'] <- input[41,i]
    
    # z = fitness cost of resistance allele in insecticide free environment
    z.RR1_00 <- input[42,i]
    z.RR2_00 <- input[43,i]
    #!r
    z[1] <- input[42,i]
    z[2] <- input[43,i]
    #todo but this could perhaps be
    #s[1,'no'] <- input[42,i]
    #s[2,'no'] <- input[43,i]    
    
    
    ### Toggle Insecticide Niches on and off ###
    ## Allows for setting of specific combinations of insecticide niches to be used
    ## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    ## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    niche_00 <- input[44,i]
    niche_a0 <- input[45,i]
    niche_A0 <- input[46,i]
    niche_0b <- input[47,i]
    niche_0B <- input[48,i]
    niche_ab <- input[49,i]
    niche_AB <- input[50,i]
    niche_Ab <- input[51,i]
    niche_aB <- input[52,i]
    
    niche['0','0'] <- input[44,i]
    niche['a','0'] <- input[45,i]
    niche['A','0'] <- input[46,i]
    niche['0','b'] <- input[47,i]
    niche['0','B'] <- input[48,i]
    niche['a','b'] <- input[49,i]
    niche['A','B'] <- input[50,i]
    niche['A','b'] <- input[51,i]
    niche['a','B'] <- input[52,i]
    
    
    
    #### end of reading inputs into parameters ####
    
    ## Set up matrices to print results to ####
    # Set up results matrix - prints overall freq of R and S allele per locus per sex, LD and overall allele freq (i.e. 1)
    results <- matrix ( nrow = max_gen, ncol = 11 )
    colnames( results ) <- c( "Gen", "m.R1", "m.R2", "m.LD", 
                              "f.R1", "f.R2", "f.LD", "M", "F", "dprime", "r2" )
    
    # set up fitness by niche matrix - records fitness scores for each niche for each genotype
    fitness <- matrix ( nrow = 10, ncol = 9, c(rep(0,90)))
    colnames(fitness) <- c( "-,-", "a,-", "A,-", "b,-", "B,-", "a,b", "A,B", "A,b", "a,B" )
    rownames(fitness) <- c( "SS1SS2", "SS2RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
    
    # set up genotype matrix - records frequencies of each of the 9 two locus genotypes each generation
    genotype <- matrix( nrow=max_gen, ncol=11 )
    colnames(genotype) <- c("gen", "SS1SS2", "SS2RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
    ## make.genotypemat function will use this data and make a matrix of the genotype frequencies
    ## frequencies of genotypes before selection - in HW equilibrium and same in male and female
    ## needs name of matrix and takes corresponding frequency of resistant allele in function call
    genotype.freq <- make.genotypemat ( P_1, P_2 )
    ## Check for errors
    #if ( sum(genotype.freq)!=1 ){			## Will print an error message if the genotype frequencies do not total 1. 
    #	print( "Error in frequencies" )		## Unhash to use for bugfixing
    #	
    #	}else{
    #		print( "Frequencies total 1")
    #		}
    
    
    ## Calculated single locus fitnesses ####
    #h[locusNum, exposure]
    #phi[locusNum, exposure] where exposure is lo, hi
    
    #!r trying to refactor below 32 lines
    #bit trickier than previous refactoring, I need to change the structure slightly
    for( locusNum in 1:2 ) #todo improve 1:2 get it from somewhere
    {
      #actually this is just for no
      Wloci[ paste0('RS',locusNum), 'no'] <- 1 - (h[locusNum, 'no'] * z[locusNum])
      Wloci[ paste0('RR',locusNum), 'no'] <- 1 - z[locusNum]
      
      for( exposure in c('lo','hi') )
      {
#         # low levels of insecticide a
#         W.SS1_a0 <- 1 - phi.SS1_a0
#         W.RS1_a0 <- W.SS1_a0 + (h.RS1_a0 * s.RR1_a0)
#         W.RR1_a0 <- W.SS1_a0 + s.RR1_a0
#         
#         # high levels of insecticide A
#         W.SS1_A0 <- 1 - phi.SS1_A0
#         W.RS1_A0 <- W.SS1_A0 + (h.RS1_A0 * s.RR1_A0)
#         W.RR1_A0 <- W.SS1_A0 + s.RR1_A0

        Wloci[ paste0('SS',locusNum), exposure] <-  1 - phi[locusNum, exposure] 
        Wloci[ paste0('RS',locusNum), exposure] <- (1 - phi[locusNum, exposure]) + 
                                                   (h[locusNum, exposure] * s[locusNum, exposure])
        Wloci[ paste0('RR',locusNum), exposure] <- (1 - phi[locusNum, exposure]) + 
                                                   (s[locusNum, exposure])
      }
    }
    

    # absence of insecticide
    # fitness of SS in absence of insecticide is entered above as a parameter
    W.RS1_00 <- 1 - (h.RS1_00 * z.RR1_00)
    W.RR1_00 <- 1 - z.RR1_00
    
    W.RS2_00 <- 1 - (h.RS2_00 * z.RR2_00)
    W.RR2_00 <- 1 - z.RR2_00
    
    # low levels of insecticide a
    W.SS1_a0 <- 1 - phi.SS1_a0
    W.RS1_a0 <- W.SS1_a0 + (h.RS1_a0 * s.RR1_a0)
    W.RR1_a0 <- W.SS1_a0 + s.RR1_a0
    
    # high levels of insecticide A
    W.SS1_A0 <- 1 - phi.SS1_A0
    W.RS1_A0 <- W.SS1_A0 + (h.RS1_A0 * s.RR1_A0)
    W.RR1_A0 <- W.SS1_A0 + s.RR1_A0
    
    # low levels of insecticide b
    W.SS2_0b <- 1 - phi.SS2_0b
    W.RS2_0b <- W.SS2_0b + (h.RS2_0b * s.RR2_0b)
    W.RR2_0b <- W.SS2_0b + s.RR2_0b
    
    # high levels of insecticide B
    W.SS2_0B <- 1 - phi.SS2_0B
    W.RS2_0B <- W.SS2_0B + (h.RS2_0B * s.RR2_0B)
    W.RR2_0B <- W.SS2_0B + s.RR2_0B
    
    
    ## Two genotype fitnesses in two insecticide Niche ##
    ## The ifelse clauses allow niches to be toggled on/off, i.e. a fitness can be given for A0
    ## but if only the niche A,B is toggled on, the fitness scores for A0 and Ab will be set to 0
    ## Fitness in specific niche is calculated by multipling fitness of two insecticides/absences present

    #!r to replace 250+ lines below
#     for( niche1 in dimnames(Wniche)$niche1)
#     {
#       for( niche2 in dimnames(Wniche)$niche2)
#       {
    for( nicheNum1 in 1:3 ) #todo get this 1:3 from somewhere
    {
      for( nicheNum2 in 1:3 ) #todo get this 1:3 from somewhere
      { 
        #this is a temporary solution
        #to get both niche (one of 0aAbB)
        #and nicheLevel (one of no,lo,hi)
        niche1 <- dimnames(Wniche)$niche1[ nicheNum1 ]
        niche2 <- dimnames(Wniche)$niche2[ nicheNum2 ]
        exposure1 <- dimnames(Wloci)$exposure[ nicheNum1 ]
        exposure2 <- dimnames(Wloci)$exposure[ nicheNum2 ]        
        
        #if this niche toggled off set fitness to 0
        if (niche[niche1,niche2] == 0)
        {
          Wniche[,,niche1,niche2] <- 0
        } else{
          #otherwise set fitness to product of the 2 loci
          for( locus1 in dimnames(Wniche)$locus1)
          {
            for( locus2 in dimnames(Wniche)$locus2)
            {   
              #*problem with this line
              #*needs to be something like
              #Wloci[locus1,niche1] * Wloci[locus2,niche2]
              #Wniche[locus1,locus2,niche1,niche2] <- Wloci[locus1,niche1,niche2] * Wloci[locus2,niche1,niche2]
              Wniche[locus1,locus2,niche1,niche2] <- Wloci[locus1,exposure1] * Wloci[locus2,exposure2]
              
            }
          }          
        }
      }
    }
    
    
    
    
    
    # -,- niche
    if( niche_00 == 0 ){
      
      #!r
      #Wniche[locus1, locus2, niche1, niche2]
      #?or should niche1 & 2 be combined
      Wniche[,,'0','0'] <- 0
      
      # SS1
      W.SS1SS2_00 <- 0
      W.SS1RS2_00 <- 0
      W.SS1RR2_00 <- 0
      # RS1
      W.RS1SS2_00 <- 0
      W.RS1RS2_00 <- 0
      W.RS1RR2_00 <- 0
      # RR1
      W.RR1SS2_00 <- 0
      W.RR1RS2_00 <- 0
      W.RR1RR2_00 <- 0
    }else{
      
      
      # SS1
      W.SS1SS2_00 <- W.SS1_00 * W.SS2_00
      W.SS1RS2_00 <- W.SS1_00 * W.RS2_00
      W.SS1RR2_00 <- W.SS1_00 * W.RR2_00
      # RS1
      W.RS1SS2_00 <- W.RS1_00 * W.SS2_00
      W.RS1RS2_00 <- W.RS1_00 * W.RS2_00
      W.RS1RR2_00 <- W.RS1_00 * W.RR2_00
      # RR1
      W.RR1SS2_00 <- W.RR1_00 * W.SS2_00
      W.RR1RS2_00 <- W.RR1_00 * W.RS2_00
      W.RR1RR2_00 <- W.RR1_00 * W.RR2_00
    }
    
    
    # a,- niche ####
    if( niche_a0 == 0 ){

      #!r to replace 9 lines below
      Wniche[,,'a','0'] <- 0      
      
      # SS1
      W.SS1SS2_a0 <- 0
      W.SS1RS2_a0 <- 0
      W.SS1RR2_a0 <- 0
      # RS1
      W.RS1SS2_a0 <- 0
      W.RS1RS2_a0 <- 0
      W.RS1RR2_a0 <- 0
      # RR1
      W.RR1SS2_a0 <- 0
      W.RR1RS2_a0 <- 0
      W.RR1RR2_a0 <- 0
    }else{
      
   
      
      # SS1
      W.SS1SS2_a0 <- W.SS1_a0 * W.SS2_00
      W.SS1RS2_a0 <- W.SS1_a0 * W.RS2_00
      W.SS1RR2_a0 <- W.SS1_a0 * W.RR2_00
      # RS1
      W.RS1SS2_a0 <- W.RS1_a0 * W.SS2_00
      W.RS1RS2_a0 <- W.RS1_a0 * W.RS2_00
      W.RS1RR2_a0 <- W.RS1_a0 * W.RR2_00
      # RR1
      W.RR1SS2_a0 <- W.RR1_a0 * W.SS2_00
      W.RR1RS2_a0 <- W.RR1_a0 * W.RS2_00
      W.RR1RR2_a0 <- W.RR1_a0 * W.RR2_00
    }
    
    # A,- niche ####
    if( niche_A0 == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'A','0'] <- 0       
      
      # SS1
      W.SS1SS2_A0 <- 0
      W.SS1RS2_A0 <- 0
      W.SS1RR2_A0 <- 0
      # RS1
      W.RS1SS2_A0 <- 0
      W.RS1RS2_A0 <- 0
      W.RS1RR2_A0 <- 0
      # RR1
      W.RR1SS2_A0 <- 0
      W.RR1RS2_A0 <- 0
      W.RR1RR2_A0 <- 0
    }else{
      

      
      # SS1
      W.SS1SS2_A0 <- W.SS1_A0 * W.SS2_00
      W.SS1RS2_A0 <- W.SS1_A0 * W.RS2_00
      W.SS1RR2_A0 <- W.SS1_A0 * W.RR2_00
      # RS1
      W.RS1SS2_A0 <- W.RS1_A0 * W.SS2_00
      W.RS1RS2_A0 <- W.RS1_A0 * W.RS2_00
      W.RS1RR2_A0 <- W.RS1_A0 * W.RR2_00
      # RR1
      W.RR1SS2_A0 <- W.RR1_A0 * W.SS2_00
      W.RR1RS2_A0 <- W.RR1_A0 * W.RS2_00
      W.RR1RR2_A0 <- W.RR1_A0 * W.RR2_00
    }
    
    
    # -,b niche ####
    #andy : i think this was a bug here B should have been b - corrected
    if( niche_0b == 0 ){ #if( niche_0B == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'0','b'] <- 0  
      
      # SS1
      W.SS1SS2_0b <- 0
      W.SS1RS2_0b <- 0
      W.SS1RR2_0b <- 0
      # RS1
      W.RS1SS2_0b <- 0
      W.RS1RS2_0b <- 0
      W.RS1RR2_0b <- 0
      # RR1
      W.RR1SS2_0b <- 0
      W.RR1RS2_0b <- 0
      W.RR1RR2_0b <- 0
    }else{
      

      
      # SS1
      W.SS1SS2_0b <- W.SS1_00 * W.SS2_0b
      W.SS1RS2_0b <- W.SS1_00 * W.RS2_0b
      W.SS1RR2_0b <- W.SS1_00 * W.RR2_0b
      # RS1
      W.RS1SS2_0b <- W.RS1_00 * W.SS2_0b
      W.RS1RS2_0b <- W.RS1_00 * W.RS2_0b
      W.RS1RR2_0b <- W.RS1_00 * W.RR2_0b
      # RR1
      W.RR1SS2_0b <- W.RR1_00 * W.SS2_0b
      W.RR1RS2_0b <- W.RR1_00 * W.RS2_0b
      W.RR1RR2_0b <- W.RR1_00 * W.RR2_0b
    }
    
    # -,B niche ####
    if( niche_0B == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'0','B'] <- 0  
      
      # SS1
      W.SS1SS2_0B <- 0
      W.SS1RS2_0B <- 0
      W.SS1RR2_0B <- 0
      # RS1
      W.RS1SS2_0B <- 0
      W.RS1RS2_0B <- 0
      W.RS1RR2_0B <- 0
      # RR1
      W.RR1SS2_0B <- 0
      W.RR1RS2_0B <- 0
      W.RR1RR2_0B <- 0
    }else{
      
  
      
      # SS1
      W.SS1SS2_0B <- W.SS1_00 * W.SS2_0B
      W.SS1RS2_0B <- W.SS1_00 * W.RS2_0B
      W.SS1RR2_0B <- W.SS1_00 * W.RR2_0B
      # RS1
      W.RS1SS2_0B <- W.RS1_00 * W.SS2_0B
      W.RS1RS2_0B <- W.RS1_00 * W.RS2_0B
      W.RS1RR2_0B <- W.RS1_00 * W.RR2_0B
      # RR1
      W.RR1SS2_0B <- W.RR1_00 * W.SS2_0B
      W.RR1RS2_0B <- W.RR1_00 * W.RS2_0B
      W.RR1RR2_0B <- W.RR1_00 * W.RR2_0B
    }
    
    # a,b niche ####
    if( niche_ab == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'a','b'] <- 0  
      
      # SS1
      W.SS1SS2_ab <- 0
      W.SS1RS2_ab <- 0
      W.SS1RR2_ab <- 0
      # RS1
      W.RS1SS2_ab <- 0
      W.RS1RS2_ab <- 0
      W.RS1RR2_ab <- 0
      # RR1
      W.RR1SS2_ab <- 0
      W.RR1RS2_ab <- 0
      W.RR1RR2_ab <- 0
      
    }else{
      

      
      # SS1
      W.SS1SS2_ab <- W.SS1_a0 * W.SS2_0b
      W.SS1RS2_ab <- W.SS1_a0 * W.RS2_0b
      W.SS1RR2_ab <- W.SS1_a0 * W.RR2_0b
      # RS1
      W.RS1SS2_ab <- W.RS1_a0 * W.SS2_0b
      W.RS1RS2_ab <- W.RS1_a0 * W.RS2_0b
      W.RS1RR2_ab <- W.RS1_a0 * W.RR2_0b
      # RR1
      W.RR1SS2_ab <- W.RR1_a0 * W.SS2_0b
      W.RR1RS2_ab <- W.RR1_a0 * W.RS2_0b
      W.RR1RR2_ab <- W.RR1_a0 * W.RR2_0b
      
    }
    
    # A,B niche ####
    if( niche_AB == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'A','B'] <- 0  
      
      # SS1
      W.SS1SS2_AB <- 0
      W.SS1RS2_AB <- 0
      W.SS1RR2_AB <- 0
      # RS1
      W.RS1SS2_AB <- 0
      W.RS1RS2_AB <- 0
      W.RS1RR2_AB <- 0
      # RR1
      W.RR1SS2_AB <- 0
      W.RR1RS2_AB <- 0
      W.RR1RR2_AB <- 0
    }else{
      

      
      # SS1
      W.SS1SS2_AB <- W.SS1_A0 * W.SS2_0B
      W.SS1RS2_AB <- W.SS1_A0 * W.RS2_0B
      W.SS1RR2_AB <- W.SS1_A0 * W.RR2_0B
      # RS1
      W.RS1SS2_AB <- W.RS1_A0 * W.SS2_0B
      W.RS1RS2_AB <- W.RS1_A0 * W.RS2_0B
      W.RS1RR2_AB <- W.RS1_A0 * W.RR2_0B
      # RR1
      W.RR1SS2_AB <- W.RR1_A0 * W.SS2_0B
      W.RR1RS2_AB <- W.RR1_A0 * W.RS2_0B
      W.RR1RR2_AB <- W.RR1_A0 * W.RR2_0B
      
    }
    
    
    # A,b niche ####
    if( niche_Ab == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'A','b'] <- 0  
      
      # SS1
      W.SS1SS2_Ab <- 0
      W.SS1RS2_Ab <- 0
      W.SS1RR2_Ab <- 0
      # RS1
      W.RS1SS2_Ab <- 0
      W.RS1RS2_Ab <- 0
      W.RS1RR2_Ab <- 0
      # RR1
      W.RR1SS2_Ab <- 0
      W.RR1RS2_Ab <- 0
      W.RR1RR2_Ab <- 0
    }else{
      
   
      
      # SS1
      W.SS1SS2_Ab <- W.SS1_A0 * W.SS2_0b
      W.SS1RS2_Ab <- W.SS1_A0 * W.RS2_0b
      W.SS1RR2_Ab <- W.SS1_A0 * W.RR2_0b
      # RS1
      W.RS1SS2_Ab <- W.RS1_A0 * W.SS2_0b
      W.RS1RS2_Ab <- W.RS1_A0 * W.RS2_0b
      W.RS1RR2_Ab <- W.RS1_A0 * W.RR2_0b
      # RR1
      W.RR1SS2_Ab <- W.RR1_A0 * W.SS2_0b
      W.RR1RS2_Ab <- W.RR1_A0 * W.RS2_0b
      W.RR1RR2_Ab <- W.RR1_A0 * W.RR2_0b
      
    }
    
    # a,B niche
    if( niche_aB == 0 ){
      
      #!r to replace 9 lines below
      Wniche[,,'a','B'] <- 0  
      
      # SS1
      W.SS1SS2_aB <- 0
      W.SS1RS2_aB <- 0
      W.SS1RR2_aB <- 0
      # RS1
      W.RS1SS2_aB <- 0
      W.RS1RS2_aB <- 0
      W.RS1RR2_aB <- 0
      # RR1
      W.RR1SS2_aB <- 0
      W.RR1RS2_aB <- 0
      W.RR1RR2_aB <- 0
    }else{
      

      
      # SS1
      W.SS1SS2_aB <- W.SS1_a0 * W.SS2_0B
      W.SS1RS2_aB <- W.SS1_a0 * W.RS2_0B
      W.SS1RR2_aB <- W.SS1_a0 * W.RR2_0B
      # RS1
      W.RS1SS2_aB <- W.RS1_a0 * W.SS2_0B
      W.RS1RS2_aB <- W.RS1_a0 * W.RS2_0B
      W.RS1RR2_aB <- W.RS1_a0 * W.RR2_0B
      # RR1
      W.RR1SS2_aB <- W.RR1_a0 * W.SS2_0B
      W.RR1RS2_aB <- W.RR1_a0 * W.RS2_0B
      W.RR1RR2_aB <- W.RR1_a0 * W.RR2_0B
      
    }
    
    
    ### Calculating fitness determining selection ####  
    ## These are calculated from the individual's fitness by two locus genotype, 
    ## and exposure to niche depending on gender
    
    #todo, I should be able to refactor these ~120 lines
    #by using arrays with named fields
    
    #!r first stage of refactoring to arrays, can reduce further later
    #Windiv['m','SS1','SS2'] <- sum( a['m',,] * Wniche['SS1','SS2',,])
    #Windiv['m','SS1','RS2'] <- sum( a['m',,] * Wniche['SS1','RS2',,])
    
    #!r 2nd stage of refactoring to arrays, this replaces the ~120 lines below
    for( sex in dimnames(Windiv)$sex)
    {
      for( locus1 in dimnames(Windiv)$locus1)
      {
        for( locus2 in dimnames(Windiv)$locus2)
        {
          Windiv[sex,locus1,locus2] <- sum( a[sex,,] * Wniche[locus1,locus2,,])
        }
      }
    }
        
    # Males, SS1, SS2
    W.m.SS1SS2 <- (a.m_00 * W.SS1SS2_00) + 
      (a.m_a0 * W.SS1SS2_a0) + (a.m_A0 * W.SS1SS2_A0) + 
      (a.m_0b * W.SS1SS2_0b) + (a.m_0B * W.SS1SS2_0B) + 
      (a.m_ab * W.SS1SS2_ab) + (a.m_AB * W.SS1SS2_AB) + 
      (a.m_Ab * W.SS1SS2_Ab) + (a.m_aB * W.SS1SS2_aB) 
    
    # Males, SS1, RS2
    W.m.SS1RS2 <- (a.m_00 * W.SS1RS2_00) + 
      (a.m_a0 * W.SS1RS2_a0) + (a.m_A0 * W.SS1RS2_A0) + 
      (a.m_0b * W.SS1RS2_0b) + (a.m_0B * W.SS1RS2_0B) + 
      (a.m_ab * W.SS1RS2_ab) + (a.m_AB * W.SS1RS2_AB) + 
      (a.m_Ab * W.SS1RS2_Ab) + (a.m_aB * W.SS1RS2_aB)
    
    # Males, SS1, RR2
    W.m.SS1RR2 <- (a.m_00 * W.SS1RR2_00) + 
      (a.m_a0 * W.SS1RR2_a0) + (a.m_A0 * W.SS1RR2_A0) + 
      (a.m_0b * W.SS1RR2_0b) + (a.m_0B * W.SS1RR2_0B) + 
      (a.m_ab * W.SS1RR2_ab) + (a.m_AB * W.SS1RR2_AB) + 
      (a.m_Ab * W.SS1RR2_Ab) + (a.m_aB * W.SS1RR2_aB)
    
    # Males, RS1, SS2
    W.m.RS1SS2 <- (a.m_00 * W.RS1SS2_00) + 
      (a.m_a0 * W.RS1SS2_a0) + (a.m_A0 * W.RS1SS2_A0) + 
      (a.m_0b * W.RS1SS2_0b) + (a.m_0B * W.RS1SS2_0B) + 
      (a.m_ab * W.RS1SS2_ab) + (a.m_AB * W.RS1SS2_AB) + 
      (a.m_Ab * W.RS1SS2_Ab) + (a.m_aB * W.RS1SS2_aB)
    
    # Males, RS1, RS2
    W.m.RS1RS2 <- (a.m_00 * W.RS1RS2_00) + 
      (a.m_a0 * W.RS1RS2_a0) + (a.m_A0 * W.RS1RS2_A0) + 
      (a.m_0b * W.RS1RS2_0b) + (a.m_0B * W.RS1RS2_0B) + 
      (a.m_ab * W.RS1RS2_ab) + (a.m_AB * W.RS1RS2_AB) + 
      (a.m_Ab * W.RS1RS2_Ab) + (a.m_aB * W.RS1RS2_aB)
    
    # Males, RS1, RR2
    W.m.RS1RR2 <- (a.m_00 * W.RS1RR2_00) + 
      (a.m_a0 * W.RS1RR2_a0) + (a.m_A0 * W.RS1RR2_A0) + 
      (a.m_0b * W.RS1RR2_0b) + (a.m_0B * W.RS1RR2_0B) + 
      (a.m_ab * W.RS1RR2_ab) + (a.m_AB * W.RS1RR2_AB) + 
      (a.m_Ab * W.RS1RR2_Ab) + (a.m_aB * W.RS1RR2_aB) 
    
    # Males, RR1, SS2
    W.m.RR1SS2 <- (a.m_00 * W.RR1SS2_00) + 
      (a.m_a0 * W.RR1SS2_a0) + (a.m_A0 * W.RR1SS2_A0) + 
      (a.m_0b * W.RR1SS2_0b) + (a.m_0B * W.RR1SS2_0B) + 
      (a.m_ab * W.RR1SS2_ab) + (a.m_AB * W.RR1SS2_AB) + 
      (a.m_Ab * W.RR1SS2_Ab) + (a.m_aB * W.RR1SS2_aB) 
    
    # Males, RR1, RS2
    W.m.RR1RS2 <- (a.m_00 * W.RR1RS2_00) + 
      (a.m_a0 * W.RR1RS2_a0) + (a.m_A0 * W.RR1RS2_A0) +
      (a.m_0b * W.RR1RS2_0b) + (a.m_0B * W.RR1RS2_0B) + 
      (a.m_ab * W.RR1RS2_ab) + (a.m_AB * W.RR1RS2_AB) + 
      (a.m_Ab * W.RR1RS2_Ab) + (a.m_aB * W.RR1RS2_aB) 
    
    # Males, RR1, RR2
    W.m.RR1RR2 <- (a.m_00 * W.RR1RR2_00) + 
      (a.m_a0 * W.RR1RR2_a0) + (a.m_A0 * W.RR1RR2_A0) + 
      (a.m_0b * W.RR1RR2_0b) + (a.m_0B * W.RR1RR2_0B) + 
      (a.m_ab * W.RR1RR2_ab) + (a.m_AB * W.RR1RR2_AB) + 
      (a.m_Ab * W.RR1RR2_Ab) + (a.m_aB * W.RR1RR2_aB)
    
    # female, SS1, SS2
    W.f.SS1SS2 <- (a.f_00 * W.SS1SS2_00) + 
      (a.f_a0 * W.SS1SS2_a0) + (a.f_A0 * W.SS1SS2_A0) +
      (a.f_0b * W.SS1SS2_0b) + (a.f_0B * W.SS1SS2_0B) + 
      (a.f_ab * W.SS1SS2_ab) + (a.f_AB * W.SS1SS2_AB) + 
      (a.f_Ab * W.SS1SS2_Ab) + (a.f_aB * W.SS1SS2_aB) 
    
    # female, SS1, RS2
    W.f.SS1RS2 <- (a.f_00 * W.SS1RS2_00) + 
      (a.f_a0 * W.SS1RS2_a0) + (a.f_A0 * W.SS1RS2_A0) +
      (a.f_0b * W.SS1RS2_0b) + (a.f_0B * W.SS1RS2_0B) + 
      (a.f_ab * W.SS1RS2_ab) + (a.f_AB * W.SS1RS2_AB) + 
      (a.f_Ab * W.SS1RS2_Ab) + (a.f_aB * W.SS1RS2_aB)
    
    # female, SS1, RR2
    W.f.SS1RR2 <- (a.f_00 * W.SS1RR2_00) + 
      (a.f_a0 * W.SS1RR2_a0) + (a.f_A0 * W.SS1RR2_A0) +
      (a.f_0b * W.SS1RR2_0b) + (a.f_0B * W.SS1RR2_0B) + 
      (a.f_ab * W.SS1RR2_ab) + (a.f_AB * W.SS1RR2_AB) + 
      (a.f_Ab * W.SS1RR2_Ab) + (a.f_aB * W.SS1RR2_aB)
    
    # female, RS1, SS2
    W.f.RS1SS2 <- (a.f_00 * W.RS1SS2_00) + 
      (a.f_a0 * W.RS1SS2_a0) + (a.f_A0 * W.RS1SS2_A0) + 
      (a.f_0b * W.RS1SS2_0b) + (a.f_0B * W.RS1SS2_0B) + 
      (a.f_ab * W.RS1SS2_ab) + (a.f_AB * W.RS1SS2_AB) + 
      (a.f_Ab * W.RS1SS2_Ab) + (a.f_aB * W.RS1SS2_aB) 
    
    # female, RS1, RS2
    W.f.RS1RS2 <- (a.f_00 * W.RS1RS2_00) + 
      (a.f_a0 * W.RS1RS2_a0) + (a.f_A0 * W.RS1RS2_A0) + 
      (a.f_0b * W.RS1RS2_0b) + (a.f_0B * W.RS1RS2_0B) +
      (a.f_ab * W.RS1RS2_ab) + (a.f_AB * W.RS1RS2_AB) + 
      (a.f_Ab * W.RS1RS2_Ab) + (a.f_aB * W.RS1RS2_aB)  
    
    # female, RS1, RR2
    W.f.RS1RR2 <- (a.f_00 * W.RS1RR2_00) + 
      (a.f_a0 * W.RS1RR2_a0) + (a.f_A0 * W.RS1RR2_A0) + 
      (a.f_0b * W.RS1RR2_0b) + (a.f_0B * W.RS1RR2_0B) +
      (a.f_ab * W.RS1RR2_ab) + (a.f_AB * W.RS1RR2_AB) + 
      (a.f_Ab * W.RS1RR2_Ab) + (a.f_aB * W.RS1RR2_aB)  
    
    # female, RR1, SS2
    W.f.RR1SS2 <- (a.f_00 * W.RR1SS2_00) +
      (a.f_a0 * W.RR1SS2_a0) + (a.f_A0 * W.RR1SS2_A0) +
      (a.f_0b * W.RR1SS2_0b) + (a.f_0B * W.RR1SS2_0B) +
      (a.f_ab * W.RR1SS2_ab) + (a.f_AB * W.RR1SS2_AB) + 
      (a.f_Ab * W.RR1SS2_Ab) + (a.f_aB * W.RR1SS2_aB) 
    
    # female, RR1, RS2
    W.f.RR1RS2 <- (a.f_00 * W.RR1RS2_00) + 
      (a.f_a0 * W.RR1RS2_a0) + (a.f_A0 * W.RR1RS2_A0) + 
      (a.f_0b * W.RR1RS2_0b) + (a.f_0B * W.RR1RS2_0B) + 
      (a.f_ab * W.RR1RS2_ab) + (a.f_AB * W.RR1RS2_AB) + 
      (a.f_Ab * W.RR1RS2_Ab) + (a.f_aB * W.RR1RS2_aB) 
    
    # female, RR1, RR2
    W.f.RR1RR2 <- (a.f_00 * W.RR1RR2_00) + 
      (a.f_a0 * W.RR1RR2_a0) + (a.f_A0 * W.RR1RR2_A0) +
      (a.f_0b * W.RR1RR2_0b) + (a.f_0B * W.RR1RR2_0B) + 
      (a.f_ab * W.RR1RR2_ab) + (a.f_AB * W.RR1RR2_AB) + 
      (a.f_Ab * W.RR1RR2_Ab) + (a.f_aB * W.RR1RR2_aB)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 		
    ### Loop to run the model from the initial conditions generated above ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    
    #browser()
    
    for (k in 1:max_gen){
      
      # In calibration 1011, between generations 5 and 12 selection is relaxed
      # fitnesses use relaxed selection fitnesses calculated above
      if( calibration == 1011 & i==2 ){
        relax <- TRUE
      }else{
        relax <- FALSE
      }
      
      if( relax == TRUE & (2<k) & (k<12)){
        # relaxed selection fitnesses
        #!r 18 lines below can be replaced with
        Windiv[] <- 0.1
        
        ## Males
        W.m.SS1SS2 <- 0.1 
        W.m.SS1RS2 <- 0.1
        W.m.SS1RR2 <- 0.1
        
        W.m.RS1SS2 <- 0.1
        W.m.RS1RS2 <- 0.1  
        W.m.RS1RR2 <- 0.1  
        
        W.m.RR1SS2 <- 0.1 
        W.m.RR1RS2 <- 0.1 
        W.m.RR1RR2 <- 0.1  
        
        ## Female
        W.f.SS1SS2 <- 0.1
        W.f.SS1RS2 <- 0.1
        W.f.SS1RR2 <- 0.1
        
        W.f.RS1SS2 <- 0.1
        W.f.RS1RS2 <- 0.1 
        W.f.RS1RR2 <- 0.1 
        
        W.f.RR1SS2 <- 0.1
        W.f.RR1RS2 <- 0.1 
        W.f.RR1RR2 <- 0.1
        
      } else if( relax == TRUE & (k>11) & (k<15)){
        
        # non relaxed fitnesses
        #!r 18 lines below can be replaced with
        Windiv[] <- 0.1
        #these do m,f at same time
        Windiv[,'RS1','RS2'] <- 1
        Windiv[,'RS1','RR2'] <- 1
        Windiv[,'RR1','RS2'] <- 1
        Windiv[,'RR1','RR2'] <- 1
                
        ## Males
        W.m.SS1SS2 <- 0.1
        W.m.SS1RS2 <- 0.1
        W.m.SS1RR2 <- 0.1
        
        W.m.RS1SS2 <- 0.1
        W.m.RS1RS2 <- 1  
        W.m.RS1RR2 <- 1  
        
        W.m.RR1SS2 <- 0.1
        W.m.RR1RS2 <- 1 
        W.m.RR1RR2 <- 1 
        
        ## Female
        W.f.SS1SS2 <- 0.1
        W.f.SS1RS2 <- 0.1
        W.f.SS1RR2 <- 0.1
        
        W.f.RS1SS2 <- 0.1
        W.f.RS1RS2 <- 1 
        W.f.RS1RR2 <- 1 
        
        W.f.RR1SS2 <- 0.1
        W.f.RR1RS2 <- 1 
        W.f.RR1RR2 <- 1
        
      } else if( relax == TRUE & (k>14) ){
        
        # relaxed selection fitnesses
        #!r 18 lines below can be replaced with
        Windiv[] <- 0.1
        
        ## Males
        W.m.SS1SS2 <- 0.1 
        W.m.SS1RS2 <- 0.1
        W.m.SS1RR2 <- 0.1
        
        W.m.RS1SS2 <- 0.1
        W.m.RS1RS2 <- 0.1  
        W.m.RS1RR2 <- 0.1  
        
        W.m.RR1SS2 <- 0.1 
        W.m.RR1RS2 <- 0.1 
        W.m.RR1RR2 <- 0.1  
        
        ## Female
        W.f.SS1SS2 <- 0.1
        W.f.SS1RS2 <- 0.1
        W.f.SS1RR2 <- 0.1
        
        W.f.RS1SS2 <- 0.1
        W.f.RS1RS2 <- 0.1 
        W.f.RS1RR2 <- 0.1 
        
        W.f.RR1SS2 <- 0.1
        W.f.RR1RS2 <- 0.1 
        W.f.RR1RR2 <- 0.1
      }
      
      
      # set genotype frequencies as variables
      # extracted from genotype frequency matrix generated above from initial value of P (frequency of R allele)
      # set for male and for female: before first round of selection these are the same values
      # f = frequency before selection
      
      # male
      f.m.SS1SS2 <- genotype.freq[1,]
      f.m.SS1RS2 <- genotype.freq[2,]
      f.m.SS1RR2 <- genotype.freq[3,]
      f.m.RS1SS2 <- genotype.freq[4,]
      f.m.RS1RS2_cis <- genotype.freq[5,]		### cis
      f.m.RS1RS2_trans <- genotype.freq[6,]	### trans
      f.m.RS1RR2 <- genotype.freq[7,]
      f.m.RR1SS2 <- genotype.freq[8,]
      f.m.RR1RS2 <- genotype.freq[9,]
      f.m.RR1RR2 <- genotype.freq[10,]
      # female
      f.f.SS1SS2 <- genotype.freq[1,]
      f.f.SS1RS2 <- genotype.freq[2,]
      f.f.SS1RR2 <- genotype.freq[3,]
      f.f.RS1SS2 <- genotype.freq[4,]
      f.f.RS1RS2_cis <- genotype.freq[5,]		### cis
      f.f.RS1RS2_trans <- genotype.freq[6,]	### trans
      f.f.RS1RR2 <- genotype.freq[7,]
      f.f.RR1SS2 <- genotype.freq[8,]
      f.f.RR1RS2 <- genotype.freq[9,]
      f.f.RR1RR2 <- genotype.freq[10,]
      
      
      # Male
      #male.freq <- (f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2+
      #	  f.m.RS1SS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RS1RR2+
      #	  f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2) 
      #print( (paste("Male frequencies before selection total = ",male.freq) ) )
      # Female
      #female.freq <- (f.f.SS1SS2 + f.f.SS1RS2 + f.f.SS1RR2+
      #	  f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2+
      #	  f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2) 			
      #print( (paste("Female frequencies before selection total = ",female.freq) ) )
      
      ### Prints record of genotype proportions each generation
      genotype[k,1] <- k					
      genotype[k,2] <- f.m.SS1SS2
      genotype[k,3] <- f.m.SS1RS2
      genotype[k,4] <- f.m.SS1RR2
      
      genotype[k,5] <- f.m.RS1SS2
      genotype[k,6] <- f.m.RS1RS2_cis
      genotype[k,7] <- f.m.RS1RS2_trans
      genotype[k,8] <- f.m.RS1RR2
      
      genotype[k,9] <- f.m.RR1SS2
      genotype[k,10] <- f.m.RR1RS2
      genotype[k,11] <- f.m.RR1RR2
      
      #### Printing Results to matrix ####
      # print generation
      results[k,1] <- k
      
      # frequency of resistance allele in males
      # locus 1
      m.R1 <- ( f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 ) + ( 0.5 * (f.m.RS1SS2 + f.m.RS1RS2_trans + f.m.RS1RS2_cis + f.m.RS1RR2 ) ) 
      results[k,2] <- m.R1
      # locus 2
      m.R2 <- ( f.m.SS1RR2 + f.m.RS1RR2 + f.m.RR1RR2 ) + ( 0.5 * (f.m.SS1RS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RR1RS2 ) )
      results[k,3] <- m.R2
      
      
      # frequency of resistance allele in females
      # locus 1
      f.R1 <- ( f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2 ) + ( 0.5 * (f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2 ) ) 
      results[k,5] <- f.R1
      # locus 2
      f.R2 <- ( f.f.SS1RR2 + f.f.RS1RR2 + f.f.RR1RR2 ) + ( 0.5 * (f.f.SS1RS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RR1RS2 ) )
      results[k,6] <- f.R2
      
      
      # total males
      results[k,8] <- ( f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2 +
                          f.m.RS1SS2 + f.m.RS1RS2_cis  + f.m.RS1RS2_trans + f.m.RS1RR2 +
                          f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 )
      
      # total females
      results[k,9] <- ( f.f.SS1SS2 + f.f.SS1RS2 + f.f.SS1RR2 +
                          f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2 +
                          f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2 )
      
      # 1013 - Fig 1. in Curtis, sequential application of insecticide
      # stops loop running if this calibration is selected to change insecticide when condition met
      # condition to change insecticide is that frequency of the R allele is >0.5 at locus under selection
      #if( calibration == 1013 && m.R1 > 0.4999 || calibration == 1013 && m.R2 > 0.4999 ){
      #stop( (paste("Frequency of R allele at or over 0.5, generation = ", k)) )
      #}				  
      
      
      ## Gametes ####
      ### Estimated here from before selection frequencies to estimate linkage disequilibrium ###
      # Gametes produced are estimated by the frequency of the genotype and their contribution to each genotype of gamete
      # 1 - both parts of genotype contribute, 0.5 - half of genotype contributes, 0.0 - neither part of genotype can produce this gamete
      
      # Male Gametes
      G.m.S1.S2 <- 0
      G.m.R1.S2 <- 0
      G.m.S1.R2 <- 0
      G.m.R1.R2 <- 0
      
      # f.m.RS1RS2_cis
      # no recombination
      G.m.R1.R2 <- G.m.R1.R2 + f.m.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      G.m.S1.S2 <- G.m.S1.S2 + f.m.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.m.S1.R2 <- G.m.S1.R2 + f.m.RS1RS2_cis * 0.5 * recomb_rate			
      G.m.R1.S2 <- G.m.R1.S2 + f.m.RS1RS2_cis * 0.5 * recomb_rate
      
      
      # f.m.RS1RS2_trans
      # no recombination
      G.m.R1.S2 <- G.m.R1.S2 + f.m.RS1RS2_trans * 0.5 * ( 1-recomb_rate )		
      G.m.S1.R2 <- G.m.S1.R2 + f.m.RS1RS2_trans * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.m.R1.R2 <- G.m.R1.R2 + f.m.RS1RS2_trans * 0.5 * recomb_rate
      G.m.S1.S2 <- G.m.S1.S2 + f.m.RS1RS2_trans * 0.5 * recomb_rate
      
      
      # SS Gametes
      G.m.S1.S2 <- G.m.S1.S2 +
        (f.m.SS1SS2 * 1.0 +
           f.m.SS1RS2 * 0.5 +
           f.m.SS1RR2 * 0.0 +
           
           f.m.RS1SS2 * 0.5 +
           f.m.RS1RR2 * 0.0 +
           
           f.m.RR1SS2 * 0.0 +
           f.m.RR1RS2 * 0.0 +
           f.m.RR1RR2 * 0.0 )
      # RS Gametes
      G.m.R1.S2 <- G.m.R1.S2 +
        (f.m.SS1SS2 * 0.0 +
           f.m.SS1RS2 * 0.0 +
           f.m.SS1RR2 * 0.0 +
           
           f.m.RS1SS2 * 0.5 +
           f.m.RS1RR2 * 0.0 +
           
           f.m.RR1SS2 * 1.0 +
           f.m.RR1RS2 * 0.5 +
           f.m.RR1RR2 * 0.0 )
      # SR Gametes
      G.m.S1.R2 <- G.m.S1.R2 +
        (f.m.SS1SS2 * 0.0 +
           f.m.SS1RS2 * 0.5 +
           f.m.SS1RR2 * 1.0 +
           
           f.m.RS1SS2 * 0.0 +
           f.m.RS1RR2 * 0.5 +
           
           f.m.RR1SS2 * 0.0 +
           f.m.RR1RS2 * 0.0 +
           f.m.RR1RR2 * 0.0 )
      # RR Gametes
      G.m.R1.R2 <- G.m.R1.R2 +
        (f.m.SS1SS2 * 0.0 +
           f.m.SS1RS2 * 0.0 +
           f.m.SS1RR2 * 0.0 +
           
           f.m.RS1SS2 * 0.0 +
           f.m.RS1RR2 * 0.5 +
           
           f.m.RR1SS2 * 0.0 +
           f.m.RR1RS2 * 0.5 +
           f.m.RR1RR2 * 1.0 )
      
      
      # Female Gametes ###
      G.f.S1.S2 <- 0
      G.f.R1.S2 <- 0
      G.f.S1.R2 <- 0
      G.f.R1.R2 <- 0
      
      # f.f.RS1RS2_cis
      #no recombination
      G.f.R1.R2 <- G.f.R1.R2 + f.f.RS1RS2_cis * 0.5 * ( 1-recomb_rate ) 
      G.f.S1.S2 <- G.f.S1.S2 + f.f.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.f.S1.R2 <- G.f.S1.R2 + f.f.RS1RS2_cis * 0.5 * recomb_rate			
      G.f.R1.S2 <- G.f.R1.S2 + f.f.RS1RS2_cis * 0.5 * recomb_rate
      
      
      # f.f.RS1RS2_trans			
      # no recombination
      G.f.R1.S2 <- G.f.R1.S2 + f.f.RS1RS2_trans * 0.5 * ( 1-recomb_rate )		
      G.f.S1.R2 <- G.f.S1.R2 + f.f.RS1RS2_trans * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.f.R1.R2 <- G.f.R1.R2 + f.f.RS1RS2_trans * 0.5 * recomb_rate
      G.f.S1.S2 <- G.f.S1.S2 + f.f.RS1RS2_trans * 0.5 * recomb_rate
      
      
      # SS Gametes
      G.f.S1.S2 <- G.f.S1.S2 +
        (f.f.SS1SS2 * 1.0 +
           f.f.SS1RS2 * 0.5 +
           f.f.SS1RR2 * 0.0 +
           
           f.f.RS1SS2 * 0.5 +
           f.f.RS1RR2 * 0.0 +
           
           f.f.RR1SS2 * 0.0 +
           f.f.RR1RS2 * 0.0 +
           f.f.RR1RR2 * 0.0 )
      # RS Gametes
      G.f.R1.S2 <- G.f.R1.S2 +
        (f.f.SS1SS2 * 0.0 +
           f.f.SS1RS2 * 0.0 +
           f.f.SS1RR2 * 0.0 +
           
           f.f.RS1SS2 * 0.5 +
           f.f.RS1RR2 * 0.0 +
           
           f.f.RR1SS2 * 1.0 +
           f.f.RR1RS2 * 0.5 +
           f.f.RR1RR2 * 0.0 )
      # SR Gametes
      G.f.S1.R2 <- G.f.S1.R2 +
        (f.f.SS1SS2 * 0.0 +
           f.f.SS1RS2 * 0.5 +
           f.f.SS1RR2 * 1.0 +
           
           f.f.RS1SS2 * 0.0 +
           f.f.RS1RR2 * 0.5 +
           
           f.f.RR1SS2 * 0.0 +
           f.f.RR1RS2 * 0.0 +
           f.f.RR1RR2 * 0.0 )
      # RR Gametes
      G.f.R1.R2 <- G.f.R1.R2 +
        (f.f.SS1SS2 * 0.0 +
           f.f.SS1RS2 * 0.0 +
           f.f.SS1RR2 * 0.0 +
           
           f.f.RS1SS2 * 0.0 +
           f.f.RS1RR2 * 0.5 +
           
           f.f.RR1SS2 * 0.0 +
           f.f.RR1RS2 * 0.5 +
           f.f.RR1RR2 * 1.0 )
      
      
      ### Linkage Disequilibrium ####
      ## Disequibilibrium of resistant allele in gametes ##
      # Male
      ## Frequency of allele patterns
      x.R1.S2 <- G.m.R1.S2/2
      x.S1.R2 <- G.m.R1.S2/2
      x.R1.R2 <- G.m.R1.R2
      ## Frequency of alleles at each locus
      R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
      R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
      
      m.D <- x.R1.R2 - (R1 * R2)
      
      # Female
      ## Frequency of allele patterns
      x.R1.S2 <- G.f.R1.S2/2
      x.S1.R2 <- G.f.R1.S2/2
      x.R1.R2<- G.f.R1.R2
      ## Frequency of alleles at each locus
      R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
      R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
      
      f.D <- x.R1.R2 - (R1 * R2)
      
      # print to results matrix
      # linkage disequilibrium
      results[k,4] <- m.D
      # linkage disequilibrium
      results[k,7] <- f.D
      
      # Finding D'
      D <- m.D		# D is given as male LD
      
      S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
      S2 <- 1 - R2
      
      p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
      p2q1 <- S1 * R2
      
      if( p1q2 < p2q1 ){		#dmax is the lower of these two
        dmax <- p1q2
      }else{
        dmax <- p2q1
      }
      
      negp1q1 <- -( R1 * R2 )	#Find -p1q1 and -p2q2, given conditions above
      negp2q2 <- -( S1 * S2 )
      
      if( negp1q1  > negp2q2 ){	#dmin is the highest of these
        dmin <- negp1q1
      }else{
        dmin <- negp2q2
      }
      
      if( D>0 ){				# if D is greater than 0
        Dprime <- D/dmax		# D' = D/dmax 
      }else{					# if D is less than 0
        Dprime <- D/dmin		# D' = D/dmin
      }
      
      results[k,10] <- Dprime	# prints to column ten of results matrix
      
      ## R2
      denom <- sqrt(R1 * S1 * R2 * S2)	# finds R2 using the allale frequencies calculated above
      r2 <- D/denom						# use this and D to find r2
      
      results[k,11] <- r2					# prints to column eleven of results matrix
      
      ### Frequency following selection ####
      
      if(calibration==103){		## no selection calibration
        ## male
        # SS1
        fs.m.SS1SS2 <- f.m.SS1SS2
        fs.m.SS1RS2 <- f.m.SS1RS2
        fs.m.SS1RR2 <- f.m.SS1RR2
        # RS1
        fs.m.RS1SS2 <- f.m.RS1SS2 
        fs.m.RS1RS2_cis <- f.m.RS1RS2_cis
        fs.m.RS1RS2_trans <- f.m.RS1RS2_trans
        fs.m.RS1RR2 <- f.m.RS1RR2
        # RR2 
        fs.m.RR1SS2 <- f.m.RR1SS2
        fs.m.RR1RS2 <- f.m.RR1RS2
        fs.m.RR1RR2 <- f.m.RR1RR2
        
        ## female
        # SS1
        fs.f.SS1SS2 <- f.f.SS1SS2
        fs.f.SS1RS2 <- f.f.SS1RS2
        fs.f.SS1RR2 <- f.f.SS1RR2
        # RS1
        fs.f.RS1SS2 <- f.f.RS1SS2 
        fs.f.RS1RS2_cis <- f.f.RS1RS2_cis
        fs.f.RS1RS2_trans <- f.f.RS1RS2_trans
        fs.f.RS1RR2 <- f.f.RS1RR2
        # RR2 
        fs.f.RR1SS2 <- f.f.RR1SS2
        fs.f.RR1RS2 <- f.f.RR1RS2
        fs.f.RR1RR2 <- f.f.RR1RR2
        
        
        #### If calibration 103 not set, selection continues as normal
      }else{
        ## W bar - Sum of numerators
        # W bar males
        W.bar.m <- (f.m.SS1SS2 * W.m.SS1SS2) + (f.m.SS1RS2 * W.m.SS1RS2) + (f.m.SS1RR2 * W.m.SS1RR2) +
          (f.m.RS1SS2 * W.m.RS1SS2) + 
          (f.m.RS1RS2_cis * W.m.RS1RS2) + (f.m.RS1RS2_trans * W.m.RS1RS2) + 
          (f.m.RS1RR2 * W.m.RS1RR2) +
          (f.m.RR1SS2 * W.m.RR1SS2) + (f.m.RR1RS2 * W.m.RR1RS2) + (f.m.RR1RR2 * W.m.RR1RR2)
        # W bar females
        W.bar.f <- (f.f.SS1SS2 * W.f.SS1SS2) + (f.f.SS1RS2 * W.f.SS1RS2) + (f.f.SS1RR2 * W.f.SS1RR2) +
          (f.f.RS1SS2 * W.f.RS1SS2) + 
          (f.f.RS1RS2_cis * W.f.RS1RS2) + (f.f.RS1RS2_trans * W.f.RS1RS2) + 
          (f.f.RS1RR2 * W.f.RS1RR2) +
          (f.f.RR1SS2 * W.f.RR1SS2) + (f.f.RR1RS2 * W.f.RR1RS2) + (f.f.RR1RR2 * W.f.RR1RR2)
        
        
        ## Frequencies --- Calculated with selection
        ## Males
        ## SS1
        fs.m.SS1SS2 <- (f.m.SS1SS2 * W.m.SS1SS2) / W.bar.m
        fs.m.SS1RS2 <- (f.m.SS1RS2 * W.m.SS1RS2) / W.bar.m
        fs.m.SS1RR2 <- (f.m.SS1RR2 * W.m.SS1RR2) / W.bar.m
        ## RS1
        fs.m.RS1SS2 <- (f.m.RS1SS2 * W.m.RS1SS2) / W.bar.m
        fs.m.RS1RS2_cis <- (f.m.RS1RS2_cis * W.m.RS1RS2) / W.bar.m
        fs.m.RS1RS2_trans <- (f.m.RS1RS2_trans * W.m.RS1RS2) / W.bar.m
        fs.m.RS1RR2 <- (f.m.RS1RR2 * W.m.RS1RR2) / W.bar.m
        ## RR1
        fs.m.RR1SS2 <- (f.m.RR1SS2 * W.m.RR1SS2) / W.bar.m
        fs.m.RR1RS2 <- (f.m.RR1RS2 * W.m.RR1RS2) / W.bar.m
        fs.m.RR1RR2 <- (f.m.RR1RR2 * W.m.RR1RR2) / W.bar.m
        
        ## Females
        ## SS1
        fs.f.SS1SS2 <- (f.f.SS1SS2 * W.f.SS1SS2) / W.bar.f
        fs.f.SS1RS2 <- (f.f.SS1RS2 * W.f.SS1RS2) / W.bar.f
        fs.f.SS1RR2 <- (f.f.SS1RR2 * W.f.SS1RR2) / W.bar.f
        ## RS1
        fs.f.RS1SS2 <- (f.f.RS1SS2 * W.f.RS1SS2) / W.bar.f
        fs.f.RS1RS2_cis <- (f.f.RS1RS2_cis * W.f.RS1RS2) / W.bar.f
        fs.f.RS1RS2_trans <- (f.f.RS1RS2_trans * W.f.RS1RS2) / W.bar.f
        fs.f.RS1RR2 <- (f.f.RS1RR2 * W.f.RS1RR2) / W.bar.f
        ## RR1
        fs.f.RR1SS2 <- (f.f.RR1SS2 * W.f.RR1SS2) / W.bar.f
        fs.f.RR1RS2 <- (f.f.RR1RS2 * W.f.RR1RS2) / W.bar.f
        fs.f.RR1RR2 <- (f.f.RR1RR2 * W.f.RR1RR2) / W.bar.f
      }
      
      ## Calibration 104, selection on one genotype
      if( calibration == 104 ){
        
        x.m <- select.gen.m				## Setting fitness of genotype to select on as separate variable
        x.f <- select.gen.f				## Not lost in reprinting in next step
        
        ## Frequencies --- Reprinting after selection fitness with before selection to eliminate selection step
        ## Males
        ## SS1
        fs.m.SS1SS2 <- f.m.SS1SS2
        fs.m.SS1RS2 <- f.m.SS1RS2
        fs.m.SS1RR2 <- f.m.SS1RR2
        ## RS1
        fs.m.RS1SS2 <- f.m.RS1SS2
        fs.m.RS1RS2_cis <- f.m.RS1RS2_cis
        fs.m.RS1RS2_trans <- f.m.RS1RS2_trans
        fs.m.RS1RR2 <- f.m.RS1RR2
        ## RR1
        fs.m.RR1SS2 <- f.m.RR1SS2
        fs.m.RR1RS2 <- f.m.RR1RS2
        fs.m.RR1RR2 <- f.m.RR1RR2
        
        ## Females
        ## SS1
        fs.f.SS1SS2 <- f.f.SS1SS2
        fs.f.SS1RS2 <- f.f.SS1RS2
        fs.f.SS1RR2 <- f.f.SS1RR2
        ## RS1
        fs.f.RS1SS2 <- f.f.RS1SS2
        fs.f.RS1RS2_cis <- f.f.RS1RS2_cis
        fs.f.RS1RS2_trans <- f.f.RS1RS2_trans
        fs.f.RS1RR2 <- f.f.RS1RR2
        ## RR1
        fs.f.RR1SS2 <- f.f.RR1SS2
        fs.f.RR1RS2 <- f.f.RR1RS2
        fs.f.RR1RR2 <- f.f.RR1RR2
        
        select.gen.m <- x.m			## Reprinting fitness that is intended to be selected on
        select.gen.f <- x.f			## with after selection fitness saved as variable above
        
      }
      
      ## Check for errors ##
      ## Will print an error message if the genotype frequencies do not total 1.
      # Male
      #male.freq <- (fs.m.SS1SS2 + fs.m.SS1RS2 + fs.m.SS1RR2+
      #	  fs.m.RS1SS2 + fs.m.RS1RS2_cis + fs.m.RS1RS2_trans + fs.m.RS1RR2+
      #	  fs.m.RR1SS2 + fs.m.RR1RS2 + fs.m.RR1RR2) 
      #print( (paste("Male frequencies after selection total = ",male.freq) ) )
      # Female
      #female.freq <- (fs.f.SS1SS2 + fs.f.SS1RS2 + fs.f.SS1RR2+
      #	  fs.f.RS1SS2 + fs.f.RS1RS2_cis + fs.f.RS1RS2_trans + fs.f.RS1RR2+
      #	  fs.f.RR1SS2 + fs.f.RR1RS2 + fs.f.RR1RR2) 			
      #print( (paste("Female frequencies after selection total = ",female.freq) ) )
      
      ## Gametes ####
      ### Estimated here to allow for the next generation to be created through random mating ###
      # Gametes produced are estimated by the frequency of the genotype and their contribution to each genotype of gamete
      # 1 - both parts of genotype contribute, 0.5 - half of genotype contributes, 0.0 - neither part of genotype can produce this gamete
      
      # Male Gametes
      G.m.S1.S2 <- 0
      G.m.R1.S2 <- 0
      G.m.S1.R2 <- 0
      G.m.R1.R2 <- 0
      
      # fs.m.RS1RS2_cis
      # no recombination
      G.m.R1.R2 <- G.m.R1.R2 + fs.m.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      G.m.S1.S2 <- G.m.S1.S2 + fs.m.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.m.S1.R2 <- G.m.S1.R2 + fs.m.RS1RS2_cis * 0.5 * recomb_rate			
      G.m.R1.S2 <- G.m.R1.S2 + fs.m.RS1RS2_cis * 0.5 * recomb_rate
      
      
      # fs.m.RS1RS2_trans
      # no recombination
      G.m.R1.S2 <- G.m.R1.S2 + fs.m.RS1RS2_trans * 0.5 * ( 1-recomb_rate )		
      G.m.S1.R2 <- G.m.S1.R2 + fs.m.RS1RS2_trans * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.m.R1.R2 <- G.m.R1.R2 + fs.m.RS1RS2_trans * 0.5 * recomb_rate
      G.m.S1.S2 <- G.m.S1.S2 + fs.m.RS1RS2_trans * 0.5 * recomb_rate
      
      
      # SS Gametes
      G.m.S1.S2 <- G.m.S1.S2 +
        (fs.m.SS1SS2 * 1.0 +
           fs.m.SS1RS2 * 0.5 +
           fs.m.SS1RR2 * 0.0 +
           
           fs.m.RS1SS2 * 0.5 +
           fs.m.RS1RR2 * 0.0 +
           
           fs.m.RR1SS2 * 0.0 +
           fs.m.RR1RS2 * 0.0 +
           fs.m.RR1RR2 * 0.0 )
      # RS Gametes
      G.m.R1.S2 <- G.m.R1.S2 +
        (fs.m.SS1SS2 * 0.0 +
           fs.m.SS1RS2 * 0.0 +
           fs.m.SS1RR2 * 0.0 +
           
           fs.m.RS1SS2 * 0.5 +
           fs.m.RS1RR2 * 0.0 +
           
           fs.m.RR1SS2 * 1.0 +
           fs.m.RR1RS2 * 0.5 +
           fs.m.RR1RR2 * 0.0 )
      # SR Gametes
      G.m.S1.R2 <- G.m.S1.R2 +
        (fs.m.SS1SS2 * 0.0 +
           fs.m.SS1RS2 * 0.5 +
           fs.m.SS1RR2 * 1.0 +
           
           fs.m.RS1SS2 * 0.0 +
           fs.m.RS1RR2 * 0.5 +
           
           f.m.RR1SS2 * 0.0 +
           f.m.RR1RS2 * 0.0 +
           f.m.RR1RR2 * 0.0 )
      # RR Gametes
      G.m.R1.R2 <- G.m.R1.R2 +
        (fs.m.SS1SS2 * 0.0 +
           fs.m.SS1RS2 * 0.0 +
           fs.m.SS1RR2 * 0.0 +
           
           fs.m.RS1SS2 * 0.0 +
           fs.m.RS1RR2 * 0.5 +
           
           fs.m.RR1SS2 * 0.0 +
           fs.m.RR1RS2 * 0.5 +
           fs.m.RR1RR2 * 1.0 )
      
      # Female Gametes
      G.f.S1.S2 <- 0
      G.f.R1.S2 <- 0
      G.f.S1.R2 <- 0
      G.f.R1.R2 <- 0
      
      # fs.f.RS1RS2_cis
      #no recombination
      G.f.R1.R2 <- G.f.R1.R2 + fs.f.RS1RS2_cis * 0.5 * ( 1-recomb_rate ) 
      G.f.S1.S2 <- G.f.S1.S2 + fs.f.RS1RS2_cis * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.f.S1.R2 <- G.f.S1.R2 + fs.f.RS1RS2_cis * 0.5 * recomb_rate			
      G.f.R1.S2 <- G.f.R1.S2 + fs.f.RS1RS2_cis * 0.5 * recomb_rate
      
      
      # fs.f.RS1RS2_trans			
      # no recombination
      G.f.R1.S2 <- G.f.R1.S2 + fs.f.RS1RS2_trans * 0.5 * ( 1-recomb_rate )		
      G.f.S1.R2 <- G.f.S1.R2 + fs.f.RS1RS2_trans * 0.5 * ( 1-recomb_rate )
      # recombination takes place
      G.f.R1.R2 <- G.f.R1.R2 + fs.f.RS1RS2_trans * 0.5 * recomb_rate
      G.f.S1.S2 <- G.f.S1.S2 + fs.f.RS1RS2_trans * 0.5 * recomb_rate
      
      
      # SS Gametes
      G.f.S1.S2 <- G.f.S1.S2 +
        (fs.f.SS1SS2 * 1.0 +
           fs.f.SS1RS2 * 0.5 +
           fs.f.SS1RR2 * 0.0 +
           
           fs.f.RS1SS2 * 0.5 +
           fs.f.RS1RR2 * 0.0 +
           
           fs.f.RR1SS2 * 0.0 +
           fs.f.RR1RS2 * 0.0 +
           fs.f.RR1RR2 * 0.0 )
      # RS Gametes
      G.f.R1.S2 <- G.f.R1.S2 +
        (fs.f.SS1SS2 * 0.0 +
           fs.f.SS1RS2 * 0.0 +
           fs.f.SS1RR2 * 0.0 +
           
           fs.f.RS1SS2 * 0.5 +
           fs.f.RS1RR2 * 0.0 +
           
           fs.f.RR1SS2 * 1.0 +
           fs.f.RR1RS2 * 0.5 +
           fs.f.RR1RR2 * 0.0 )
      # SR Gametes
      G.f.S1.R2 <- G.f.S1.R2 +
        (fs.f.SS1SS2 * 0.0 +
           fs.f.SS1RS2 * 0.5 +
           fs.f.SS1RR2 * 1.0 +
           
           fs.f.RS1SS2 * 0.0 +
           fs.f.RS1RR2 * 0.5 +
           
           f.f.RR1SS2 * 0.0 +
           f.f.RR1RS2 * 0.0 +
           f.f.RR1RR2 * 0.0 )
      # RR Gametes
      G.f.R1.R2 <- G.f.R1.R2 +
        (fs.f.SS1SS2 * 0.0 +
           fs.f.SS1RS2 * 0.0 +
           fs.f.SS1RR2 * 0.0 +
           
           fs.f.RS1SS2 * 0.0 +
           fs.f.RS1RR2 * 0.5 +
           
           fs.f.RR1SS2 * 0.0 +
           fs.f.RR1RS2 * 0.5 +
           fs.f.RR1RR2 * 1.0 )
      
      # Check male gamete total		
      #G.m.t <- G.m.S1.S2 + G.m.R1.S2 + G.m.S1.R2 + G.m.R1.R2		
      #if ( (G.m.t)!=1 ){			
      #	print( (paste("Error in male gametes, gametes = ",G.m.t) ) )
      
      #	}else{
      #		print( (paste("Male gametes correct, gametes = ",G.m.t) ) )
      #		}
      
      #G.f.t <- G.f.S1.S2 + G.f.R1.S2 + G.f.S1.R2 + G.f.R1.R2		
      #if ( (G.f.t)!=1 ){			
      #	print( (paste("Error in female gametes, gametes = ",G.f.t) ) )
      #	
      #	}else{
      #		print( (paste("Female gametes correct, gametes = ",G.f.t) ) )
      #		}
      
      
      ### To run next loop ####
      ## Random Mating ##
      # set blank variables for frequencies of each genotype
      # Only calculated once and then treated as the same for males and females (when frequencies generated at start of loop)
      f.m.SS1SS2 <- 0
      f.m.SS1RS2 <- 0
      f.m.SS1RR2 <- 0
      
      f.m.RS1SS2 <- 0
      f.m.RS1RS2_cis <- 0			#RS1RS2
      f.m.RS1RS2_trans <- 0		#RS1SR2
      f.m.RS1RR2 <- 0
      
      f.m.RR1SS2 <- 0
      f.m.RR1RS2 <- 0 
      f.m.RR1RR2 <- 0
      # SS male with SS female
      f.m.SS1SS2 <- f.m.SS1SS2 + ( G.m.S1.S2 * G.f.S1.S2 )
      # SS male with SR female
      f.m.SS1RS2 <- f.m.SS1RS2 + ( G.m.S1.S2 * G.f.S1.R2 )
      # SS male with RS female
      f.m.RS1SS2 <- f.m.RS1SS2 + ( G.m.S1.S2 * G.f.R1.S2 )
      # SS male with RR female
      f.m.RS1RS2_cis <- f.m.RS1RS2_cis + ( G.m.S1.S2 * G.f.R1.R2 )
      
      # SR male with SS female
      f.m.SS1RS2 <- f.m.SS1RS2 + ( G.m.S1.R2 * G.f.S1.S2 )
      # SR male with SR female
      f.m.SS1RR2 <- f.m.SS1RR2 + ( G.m.S1.R2 * G.f.S1.R2 )
      # SR male with RS female
      f.m.RS1RS2_trans <- f.m.RS1RS2_trans + ( G.m.S1.R2 * G.f.R1.S2 )
      # SR male with RR female
      f.m.RS1RR2 <- f.m.RS1RR2 + ( G.m.S1.R2 * G.f.R1.R2 )
      
      # RS male with SS female
      f.m.RS1SS2 <- f.m.RS1SS2 + ( G.m.R1.S2 * G.f.S1.S2 )
      # RS male with SR female
      f.m.RS1RS2_trans <- f.m.RS1RS2_trans + ( G.m.R1.S2 * G.f.S1.R2 )
      # RS male with RS female
      f.m.RR1SS2 <- f.m.RR1SS2 + ( G.m.R1.S2 * G.f.R1.S2 )
      # RS male with RR female
      f.m.RR1RS2 <- f.m.RR1RS2 + ( G.m.R1.S2 * G.f.R1.R2 )
      
      # RR male with SS female
      f.m.RS1RS2_cis <- f.m.RS1RS2_cis + ( G.m.R1.R2 * G.f.S1.S2 ) 
      # RR male with SR female
      f.m.RS1RR2 <- f.m.RS1RR2 + ( G.m.R1.R2 * G.f.S1.R2 )
      # RR male with RS female
      f.m.RR1RS2 <- f.m.RR1RS2 + ( G.m.R1.R2 * G.f.R1.S2 )
      # RR male with RR female
      f.m.RR1RR2 <- f.m.RR1RR2 + ( G.m.R1.R2 * G.f.R1.R2 )
      
      # check of total genotype frequencies
      #gen.total <- ( f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2 +
      #			   f.m.RS1SS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RS1RR2 +
      #			   f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 )
      
      #print( paste( "Genotype totals after mating = ",gen.total ) )
      
      
      ## Puts frequencies back into genotype frequency matrix to restart the loop
      if( calibration == 102 ){
        genotype.freq <- genotype.freq 
      }else{
        ## reprints genotype.freq with new frequencies from gametes
        genotype.freq[1,] <- f.m.SS1SS2
        genotype.freq[2,] <- f.m.SS1RS2
        genotype.freq[3,] <- f.m.SS1RR2
        
        genotype.freq[4,] <- f.m.RS1SS2
        genotype.freq[5,] <- f.m.RS1RS2_cis
        genotype.freq[6,] <- f.m.RS1RS2_trans
        genotype.freq[7,] <- f.m.RS1RR2
        
        genotype.freq[8,] <- f.m.RR1SS2
        genotype.freq[9,] <- f.m.RR1RS2
        genotype.freq[10,] <- f.m.RR1RR2
      }
      
    }	# end of generation loop 
    
    
    ## Assign results matrices to lists for multiple runs
    listOut$results[[i]] <- results
    listOut$genotype[[i]] <- genotype
    
    
    ## Plots ####
    if( produce.plots == TRUE ){
      # Plot of R and S allele frequencies over generations
      # Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
      # and same in female at locus 1 (red) and locus 2 (orange)
      genplot <- plotallele.freq( listOut$results[[i]] )
      if (savePlots)
      {
        # Saves plot into same directory as code documents
        dev.copy(png, (paste(i,'freq-Rallele-bygender.png')))		## WARNING: this will overwrite every time, move or rename files! ##
        dev.off()
      }
      
      # Plot of RR, RS and SS at each locus over generations
      # locus 1: SS in pink, RS in orange, RR in red
      # locus 2: SS in cyan, RS in dark blue, RR in green
      genplot <- plothaplotype( listOut$genotype[[i]] )
      
      if (savePlots)
      {
        # Saves plot into same directory as code documents
        dev.copy(png,(paste(i,'haplotype-frequencies.png')))		## WARNING: this will overwrite every time, move or rename files! ##
        dev.off()
      }
      
      # Plot of LD over time
      genplot <- plotlinkage( listOut$results[[i]] )
      if (savePlots)
      {
        # Saves plot into same directory as code documents
        dev.copy(png,(paste(i,'LD.png')))		## WARNING: this will overwrite every time, move or rename files! ##
        dev.off()
      }
    }
    
    #### Prints fitnesses caluclated by niche by genotype to matrix ####
    ## To save in .csv, enter save.param as TRUE above ##
    if( coll.fitvals == 1 ){
      fbn <- matrix( ncol=9, nrow=9 )
      colnames(fbn) <- c("-,-", "a,-", "A,-", "-,b", "-,B", "a,b", "A,B", "A,b", "a,B")
      #my order is 00 a0 A0 0b ab Ab 0B aB AB
      rownames(fbn) <- c("SS1SS2", "SS1RS2", "SS1RR2",
                         "RS1SS2", "RS1RS2", "RS1RR2",
                         "RR1SS2", "RR1RS2", "RR1RR2" )
      #!r for refactoring testing
      fbn2 <- fbn
      
      
      # SS1			 
      fbn[1,1] <- W.SS1SS2_00
      fbn[1,2] <- W.SS1SS2_a0
      fbn[1,3] <- W.SS1SS2_A0
      fbn[1,4] <- W.SS1SS2_0b
      fbn[1,5] <- W.SS1SS2_0B
      fbn[1,6] <- W.SS1SS2_ab
      fbn[1,7] <- W.SS1SS2_AB
      fbn[1,8] <- W.SS1SS2_Ab
      fbn[1,9] <- W.SS1SS2_aB
      
      fbn[2,1] <- W.SS1RS2_00
      fbn[2,2] <- W.SS1RS2_a0
      fbn[2,3] <- W.SS1RS2_A0
      fbn[2,4] <- W.SS1RS2_0b
      fbn[2,5] <- W.SS1RS2_0B
      fbn[2,6] <- W.SS1RS2_ab
      fbn[2,7] <- W.SS1RS2_AB
      fbn[2,8] <- W.SS1RS2_Ab
      fbn[2,9] <- W.SS1RS2_aB
      
      fbn[3,1] <- W.SS1RR2_00
      fbn[3,2] <- W.SS1RR2_a0
      fbn[3,3] <- W.SS1RR2_A0
      fbn[3,4] <- W.SS1RR2_0b
      fbn[3,5] <- W.SS1RR2_0B
      fbn[3,6] <- W.SS1RR2_ab
      fbn[3,7] <- W.SS1RR2_AB
      fbn[3,8] <- W.SS1RR2_Ab
      fbn[3,9] <- W.SS1RR2_aB						  
      
      # RS1
      fbn[4,1] <- W.RS1SS2_00
      fbn[4,2] <- W.RS1SS2_a0
      fbn[4,3] <- W.RS1SS2_A0
      fbn[4,4] <- W.RS1SS2_0b
      fbn[4,5] <- W.RS1SS2_0B
      fbn[4,6] <- W.RS1SS2_ab
      fbn[4,7] <- W.RS1SS2_AB
      fbn[4,8] <- W.RS1SS2_Ab
      fbn[4,9] <- W.RS1SS2_aB
      
      fbn[5,1] <- W.RS1RS2_00
      fbn[5,2] <- W.RS1RS2_a0
      fbn[5,3] <- W.RS1RS2_A0
      fbn[5,4] <- W.RS1RS2_0b
      fbn[5,5] <- W.RS1RS2_0B
      fbn[5,6] <- W.RS1RS2_ab
      fbn[5,7] <- W.RS1RS2_AB
      fbn[5,8] <- W.RS1RS2_Ab
      fbn[5,9] <- W.RS1RS2_aB
      
      fbn[6,1] <- W.RS1RR2_00
      fbn[6,2] <- W.RS1RR2_a0
      fbn[6,3] <- W.RS1RR2_A0
      fbn[6,4] <- W.RS1RR2_0b
      fbn[6,5] <- W.RS1RR2_0B
      fbn[6,6] <- W.RS1RR2_ab
      fbn[6,7] <- W.RS1RR2_AB
      fbn[6,8] <- W.RS1RR2_Ab
      fbn[6,9] <- W.RS1RR2_aB		
      
      fbn[7,1] <- W.RR1SS2_00
      fbn[7,2] <- W.RR1SS2_a0
      fbn[7,3] <- W.RR1SS2_A0
      fbn[7,4] <- W.RR1SS2_0b
      fbn[7,5] <- W.RR1SS2_0B
      fbn[7,6] <- W.RR1SS2_ab
      fbn[7,7] <- W.RR1SS2_AB
      fbn[7,8] <- W.RR1SS2_Ab
      fbn[7,9] <- W.RR1SS2_aB
      
      fbn[8,1] <- W.RR1RS2_00
      fbn[8,2] <- W.RR1RS2_a0
      fbn[8,3] <- W.RR1RS2_A0
      fbn[8,4] <- W.RR1RS2_0b
      fbn[8,5] <- W.RR1RS2_0B
      fbn[8,6] <- W.RR1RS2_ab
      fbn[8,7] <- W.RR1RS2_AB
      fbn[8,8] <- W.RR1RS2_Ab
      fbn[8,9] <- W.RR1RS2_aB
      
      fbn[9,1] <- W.RR1RR2_00
      fbn[9,2] <- W.RR1RR2_a0
      fbn[9,3] <- W.RR1RR2_A0
      fbn[9,4] <- W.RR1RR2_0b
      fbn[9,5] <- W.RR1RR2_0B
      fbn[9,6] <- W.RR1RR2_ab
      fbn[9,7] <- W.RR1RR2_AB
      fbn[9,8] <- W.RR1RR2_Ab
      fbn[9,9] <- W.RR1RR2_aB						  
      
      
      #!r if I have niches & loci in correct order
      #I should be able to refactor the 81 lines above with :
      #initially fails test
      for( locus1 in dimnames(Wniche)$locus1)
      {
        for( locus2 in dimnames(Wniche)$locus2)
        {
          #this is a good way of doing but the columns end in a different order
          #which wouldn't be a problem except that initially I'm trying
          #to keep results identical to Beths
          #fbn2[paste0(locus1,locus2),] <- Wniche[locus1,locus2,,]
          #so instead go through each niche
          for( niche1 in dimnames(Wniche)$niche1)
          {
            for( niche2 in dimnames(Wniche)$niche2)
            {
              #now need to convert 0 to - and insert commas
              columnName <- paste0(niche1,",",niche2)
              columnName <- gsub('0','-',columnName)
              fbn2[paste0(locus1,locus2),columnName] <- Wniche[locus1,locus2,niche1,niche2]
            }
          }
        }
      }
      
      #now compare fbn and fbn2
      identical(fbn,fbn2)
      
      
      listOut$fitness[[i]] <- fbn2
      
    }					  
    if( save.fitvals==1 ){
      write.csv ( fbn, (paste(i,"two-locus_fitness-scores.csv")), row.names=T)
    }
    
    
  }		### loop through columns of parameter input table - produces results lists ###
  
  #return list of outputs
  invisible(listOut)
  
   
}