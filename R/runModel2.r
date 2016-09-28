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
#' input <- setInputOneScenario(max_gen=5)
#' tst <- runModel2(input)
#' #sex linked
#' input <- setInputOneScenario(sexLinked=1)
#' tst <- runModel2(input) 
#' #sequential insecticide use
#' #? should I assume that exposure to the 2nd insecticide has been set to 0
#' #? I should probably check
#' #setting calibration & exposures to insecticides as the same for mf
#' #this doesn't quite seem to work yet, freq of R stays at 0
#' input <- setInputOneScenario(calibration=1013, 
#'                              a.m_A0 = 0.9, a.m_00 = 0.1, a.m_AB = 0, 
#'                              a.f_A0 = 0.9, a.f_00 = 0.1, a.f_AB = 0)
#' #can either set calibration to 1013 as an arg to setInput or runModel2()
#' tst <- runModel2(input)
#' @return a list of 3 lists of one or more scenarios: results, genotype and fitness. e.g. listOut$results[1] gives a results matrix for the first scenario
#' @export

runModel2 <- function(input,
                     calibration,
                     produce.plots = FALSE,
                     savePlots=FALSE){
 
  ### Lists to store results ####
  #replacing 3 results lists with a list of 3 lists
  listOut <- list( results=list(), fitness=list(), genotype=list(), input=input )

  ## Scenario loop
  ## inputs for each scenario are stored in columns of 'input'
  for (i in 1:ncol( input ) ){
    
    if (i%%10==0) cat("in runModel2() scenario",i,"/",ncol( input ),'\n')
    
    # Calibrations
    calibration <- input[1,i]
    
    # max generations
    max_gen <- input[2,i]
    
    # not used, now always output fitness, input rows 3 & 4 now free
    # coll.fitvals <- input[3,i]
    # not used anymore
    # save.fitvals <- input[4,i]		
    
    # frequency of resistance allele at locus 1&2 respectively
    P_1 <- input[5,i]	
    P_2	<- input[6,i]	
    if ( P_1 > 1 | P_2 >1 ) warning('input resistance frequence >1 P_1:',P_1,' P_2:',P_2,'\n')
    
    # Recombination
    recomb_rate <- input[7,i]		# recombination rate
    
    ## named arrays created to store model components
    # exposure
    a       <- createArray2( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
    # fitness by locus
    Wloci   <- createArray2( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    # fitness by niche    
    Wniche  <- createArray2( locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )    
    # fitness by individual
    Windiv  <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
    # insecticide niche toggle
    niche   <- createArray2( niche1=c('0','a','A'), niche2=c('0','b','B') )
    
    # selection coefficient
    s       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi') ) #or just lo hi
    # fitness of one locus (baseline)
    phi     <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    # dominance coefficient
    h       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    # fitness cost of resistance allele in no insecticide
    z       <- createArray2(locusNum=c(1,2))
    

    ## Exposure levels of males and females to each insecticide niche
    # lower case = low concentration, upper case = high concentration, 0 = absence   
    
    # males
    a['m','0','0'] <- input[8,i]
    
    a['m','a','0'] <- input[9,i]
    a['m','A','0'] <- input[10,i]
    
    a['m','0','b'] <- input[11,i]
    a['m','0','B'] <- input[12,i]
    
    a['m','a','b'] <- input[13,i]
    a['m','A','B'] <- input[14,i]    
    
    a['m','A','b'] <- input[15,i]
    a['m','a','B'] <- input[16,i]      
    
    #allow rounding errors
    if ( !isTRUE( all.equal(1, sum(a['m',,])  ))){
    #if ( sum(a['m',,]) != 1 ){		 
    	stop( paste("Error in male exposures: must total one: ", sum(a['m',,])) )
    	}
    
    # females
    a['f','0','0'] <- input[17,i]
    
    a['f','a','0'] <- input[18,i]
    a['f','A','0'] <- input[19,i]
    
    a['f','0','b'] <- input[20,i]
    a['f','0','B'] <- input[21,i]
    
    a['f','a','b'] <- input[22,i]
    a['f','A','B'] <- input[23,i] 
    
    a['f','A','b'] <- input[24,i]
    a['f','a','B'] <- input[25,i] 

    #allow rounding errors
    if ( !isTRUE( all.equal(1, sum(a['f',,])  ))){    
    #if ( sum(a['f',,]) != 1 ){		 
      stop( paste("Error in female exposures: must total one: ", sum(a['f',,])) )
    }
    
    
    ## Fitness Values
    # Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
    # User entered fitness values to allow some survival of homozygote susceptible due to chance
    phi[1,'lo'] <- input[26,i]
    phi[1,'hi'] <- input[27,i]
    phi[2,'lo'] <- input[28,i]
    phi[2,'hi'] <- input[29,i]
    
    # fitness of SS in environment with no insecticide are set to 1
    Wloci['SS1','no'] <- input[30,i]
    Wloci['SS2','no'] <- input[31,i]    

    # h = dominance coefficient
    h[1,'no'] <- input[32,i]
    h[1,'lo'] <- input[33,i]
    h[1,'hi'] <- input[34,i]
    h[2,'no'] <- input[35,i]
    h[2,'lo'] <- input[36,i]
    h[2,'hi'] <- input[37,i]    
    
    # s = selection coefficient
    s[1,'lo'] <- input[38,i]
    s[1,'hi'] <- input[39,i]
    s[2,'lo'] <- input[40,i]
    s[2,'hi'] <- input[41,i]
    
    # z = fitness cost of resistance allele in insecticide free environment
    z[1] <- input[42,i]
    z[2] <- input[43,i]
    #todo but this could perhaps be
    #s[1,'no'] <- input[42,i]
    #s[2,'no'] <- input[43,i]    
    
    ## Toggle Insecticide Niches on and off
    # allows setting specific combinations of insecticide niches
    # if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    # even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    
    niche['0','0'] <- input[44,i]
    niche['a','0'] <- input[45,i]
    niche['A','0'] <- input[46,i]
    niche['0','b'] <- input[47,i]
    niche['0','B'] <- input[48,i]
    niche['a','b'] <- input[49,i]
    niche['A','B'] <- input[50,i]
    niche['A','b'] <- input[51,i]
    niche['a','B'] <- input[52,i]
    
    #andy to read new sexLinked parameter, if not present set to FALSE
    sexLinked <- FALSE
    if (nrow(input) > 52)
      sexLinked <- as.logical(input[53,i]) #0 to FALSE, 1 to TRUE 
    #extra check in case NA value gets in
    if ( is.na(sexLinked) | !is.logical(sexLinked) ) sexLinked <- FALSE
    
    ## end of reading inputs into parameters
    
    # Set up matrices to output results to
    # matrix:results - for overall freq of R and S allele per locus per sex, LD and overall allele freq (i.e. 1)
    results <- matrix ( nrow = max_gen, ncol = 12 )
    colnames( results ) <- c( "Gen", "m.R1", "m.R2", "m.LD", 
                              "f.R1", "f.R2", "f.LD", "M", "F", "dprime.m", "r2", "dprime.f" )
    
    # matrix:fitness - fitness scores for each niche for each genotype
    fitness <- matrix ( nrow = 10, ncol = 9, c(rep(0,90)))
    colnames(fitness) <- c( "-,-", "a,-", "A,-", "b,-", "B,-", "a,b", "A,B", "A,b", "a,B" )
    rownames(fitness) <- c( "SS1SS2", "SS1RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
    
    # matrix:genotype - frequencies of each of the 10 two locus genotypes each generation
    genotype <- matrix( nrow=max_gen, ncol=11 )
    colnames(genotype) <- c("gen", "SS1SS2", "SS1RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
    

    ## genotype frequencies before selection (f) and after (fs)
    
    # HW equilibrium 
    genotype.freq <- make.genotypemat ( P_1, P_2 )
    namesLoci <- rownames( genotype.freq )
    f <- createArray2( sex=c("m","f"), loci=namesLoci )
    
    #setting genotype freq at start to same for m & f 
    f['m', ] <- f['f', ] <- genotype.freq[]
    
    # warning allows for rounding differences
    # only check 'm' because m&f set same above
    if ( !isTRUE( all.equal(1, sum(f['m',])  )))
      warning("genotype frequencies before selection total != 1 ", sum(f['m',]) ) 

    
    ## Single locus fitnesses
    Wloci <- fitnessSingleLocus(Wloci=Wloci,
                                h = h,
                                s = s,
                                phi = phi,
                                z = z)
    
    
    ## Two locus niche fitness in two insecticide Niche
    Wniche <- fitnessNiche( Wloci = Wloci,
                            niche = niche,
                            Wniche = Wniche )
    
    
    ## Individual fitness based on exposure to niche & 2 locus fitness
    Windiv <- fitnessIndiv( Wniche = Wniche, a = a, Windiv = Windiv )
 
    #testing
    # print("testing indiv fitness for exposure:")
    # df_indiv <- as.data.frame(Windiv)
    # # [1,] just prints males
    # print(as.data.frame(a)[1,]) #exposure
    # print(df_indiv[1,])

           
    ##################
    ## generation loop
    for (k in 1:max_gen){
      
      # In calibration 1011, selection relaxed for a set time
      if( calibration == 1011 & i==2 ) Windiv <- relaxSelection(Windiv, k)      
      
      # save record of genotype proportions each generation
      genotype[k,1] <- k	
      #genotype[k,2:11] <- f['m',]
      # 27/9/16 changing this to output mean of m&f
      genotype[k,2:11] <- sum( 0.5*(f['f',] + f['m',]))
      
      # saving results 
      results[k,1] <- k #generation number
      
      ##################################
      ## frequency of resistance alleles
      #todo this can be refactored further
      #ian jan2015 there may be redundancy here if this also calc for gametes
      m.R1 <- sum(f['m',grep("RR1",colnames(f))]) + ( 0.5 * sum(f['m',grep("RS1",colnames(f))]))
      m.R2 <- sum(f['m',grep("RR2",colnames(f))]) + ( 0.5 * sum(f['m',grep("RS2",colnames(f))]))
      f.R1 <- sum(f['f',grep("RR1",colnames(f))]) + ( 0.5 * sum(f['f',grep("RS1",colnames(f))]))
      f.R2 <- sum(f['f',grep("RR2",colnames(f))]) + ( 0.5 * sum(f['f',grep("RS2",colnames(f))]))   
      results[k,2] <- m.R1
      results[k,3] <- m.R2
      results[k,5] <- f.R1
      results[k,6] <- f.R2
      
      # record total fitnesses for m & f
      # which are always 1, not sure why Beth has here ?
      results[k,8] <- sum(f['m',])
      results[k,9] <- sum(f['f',])
      
      # previous sequential insecticide code that was here now done post-processing
      
      ## linkage calculations
      results <- linkage_calc( freq=f, recomb_rate=recomb_rate, gen_num=k, results=results )

      ## selection
      fs <- selection( freq=f, Windiv=Windiv, calibration=calibration)

      ## Gametes from after selection
      G <- createGametes( f = fs, recomb_rate = recomb_rate ) 
      
      ## Random Mating
      # males & females will only be different if sexLinked=TRUE
      
      # males (initially by calculating 'expanded' genotypes)
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=TRUE)
      f['m',] <- genotypesLong2Short(fGenotypeExpanded)
      
      # females
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=FALSE)
      f['f',] <- genotypesLong2Short(fGenotypeExpanded)      
      
      #calibration 102 : genotype frequencies reset to what they were at start of loop
      if( calibration == 102 ){ f['m', ] <- f['f', ] <- genotype.freq[] }
      
    }	## end of generation loop 
    
    ## Assign results matrices to lists for multiple runs
    listOut$results[[i]] <- results
    listOut$genotype[[i]] <- genotype
    listOut$fitness[[i]] <- fitnessOutput( Wniche )    
    
    ## Plots
    if( produce.plots ) plot_outputs_all( listOut=listOut, scen_num=i, savePlots=savePlots)

  }		## end of scenarios loop (each column in input) 
  
  
  # return list of outputs
  invisible(listOut)
}