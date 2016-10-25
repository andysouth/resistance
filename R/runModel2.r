#' run the model scenarios specified in the input object (refactored)
#' 
#' refactored version starting to use genotype and niche arrays to reduce code volume
#' run model scenarios and put the results for each in a results object
#' 
#' @param input a matrix with parameters in rows and scenarios in columns
#' @param produce.plots whether to produce plots
#' @param savePlots whether to save plots to hardcoded filenames

#' @examples 
#' input <- setInputOneScenario()
#' listOut <- runModel2(input)
#' input <- setInputOneScenario(max_gen=5)
#' listOut <- runModel2(input)
#' #sex linked
#' input <- setInputOneScenario(sexLinked=1)
#' listOut <- runModel2(input) 
#' @return a list of 3 lists of one or more scenarios: results, genotype and fitness. e.g. listOut$results[1] gives a results matrix for the first scenario
#' @export

runModel2 <- function(input = NULL,
                     produce.plots = FALSE,
                     savePlots=FALSE){
 
  # to allow default run
  if (is.null(input)) input <- setInputOneScenario()
  
  ## Lists to store results
  listOut <- list( results=list(), fitness=list(), genotype=list(), input=input )

  ## Scenario loop : each scenario from 1 column of 'input'
  for (scen_num in 1:ncol( input ) ){
    
    if (scen_num%%10==0) cat("in runModel2() scenario",scen_num,"/",ncol( input ),'\n')
    
    # calibrations - allows run to be modified : not used in new runs
    calibration <- input[1,scen_num]
    
    # max generations
    max_gen <- input[2,scen_num]
    
    # not used, now always output fitness, input rows 3 & 4 now free
    # coll.fitvals <- input[3,scen_num]
    # not used anymore
    # save.fitvals <- input[4,scen_num]		
    
    # frequency of resistance allele at locus 1&2
    P_1 <- input[5,scen_num]	
    P_2	<- input[6,scen_num]	
    if ( P_1 > 1 | P_2 >1 ) warning('input resistance frequence >1 P_1:',P_1,' P_2:',P_2,'\n')
    
    recomb_rate <- input[7,scen_num]		# recombination rate
    
    ## named arrays to store model components
    # fitness by locus
    a_fitloc   <- createArray2( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    # fitness by niche    
    a_fitnic  <- createArray2( locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )    
    # fitness by genotype
    a_fitgen  <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
    # insecticide niche toggle
    a_nichetog   <- createArray2( niche1=c('0','a','A'), niche2=c('0','b','B') )
    
    # selection coefficient
    a_sel       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi') ) #or just lo hi
    # fitness of one locus (baseline)
    a_effect     <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    # dominance coefficient
    a_dom       <- createArray2(locusNum=c(1,2), exposure=c('no','lo','hi'))
    # fitness cost of resistance allele in no insecticide
    a_cost       <- createArray2(locusNum=c(1,2))
    
    # exposure to insecticides
    a_expos <- setExposureFromInput( input, scen_num=scen_num )    
    
    # Effectiveness, fitness of SS in each insecticide/concentration
    a_effect[1,'lo'] <- input[26,scen_num]
    a_effect[1,'hi'] <- input[27,scen_num]
    a_effect[2,'lo'] <- input[28,scen_num]
    a_effect[2,'hi'] <- input[29,scen_num]
    
    # fitness of SS in environment with no insecticide are set to 1
    a_fitloc['SS1','no'] <- input[30,scen_num]
    a_fitloc['SS2','no'] <- input[31,scen_num]    

    # dominance = dominance coefficient
    a_dom[1,'no'] <- input[32,scen_num]
    a_dom[1,'lo'] <- input[33,scen_num]
    a_dom[1,'hi'] <- input[34,scen_num]
    a_dom[2,'no'] <- input[35,scen_num]
    a_dom[2,'lo'] <- input[36,scen_num]
    a_dom[2,'hi'] <- input[37,scen_num]    
    
    # a_sel = selection coefficient
    a_sel[1,'lo'] <- input[38,scen_num]
    a_sel[1,'hi'] <- input[39,scen_num]
    a_sel[2,'lo'] <- input[40,scen_num]
    a_sel[2,'hi'] <- input[41,scen_num]
    
    # a_cost = fitness cost of resistance allele in insecticide free environment
    a_cost[1] <- input[42,scen_num]
    a_cost[2] <- input[43,scen_num]
    #todo but this could perhaps be
    #a_sel[1,'no'] <- input[42,scen_num]
    #a_sel[2,'no'] <- input[43,scen_num]    
    
    ## Toggle Insecticide Niches on and off
    # if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
    # even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
    
    a_nichetog['0','0'] <- input[44,scen_num]
    a_nichetog['a','0'] <- input[45,scen_num]
    a_nichetog['A','0'] <- input[46,scen_num]
    a_nichetog['0','b'] <- input[47,scen_num]
    a_nichetog['0','B'] <- input[48,scen_num]
    a_nichetog['a','b'] <- input[49,scen_num]
    a_nichetog['A','B'] <- input[50,scen_num]
    a_nichetog['A','b'] <- input[51,scen_num]
    a_nichetog['a','B'] <- input[52,scen_num]
    
    #andy to read new sexLinked parameter, if not present set to FALSE
    sexLinked <- FALSE
    if (nrow(input) > 52)
      sexLinked <- as.logical(input[53,scen_num]) #0 to FALSE, 1 to TRUE 
    #extra check in case NA value gets in
    if ( is.na(sexLinked) | !is.logical(sexLinked) ) sexLinked <- FALSE
    
    ## end of reading inputs into parameters
    
    # Set up matrices to output results
    # matrix:results - for overall freq of R and S allele per locus per sex, LD and overall allele freq (i.e. 1)
    results <- matrix ( nrow = max_gen, ncol = 12 )
    colnames( results ) <- c( "Gen", "m.R1", "m.R2", "m.LD", 
                              "f.R1", "f.R2", "f.LD", "M", "F", "dprime.m", "r2", "dprime.f" )
    
    # matrix:fitness used to be created here & not used, now created in fitnessOutput()
    
    # matrix:genotype - frequencies of each of the 10 two locus genotypes each generation
    genotype <- matrix( nrow=max_gen, ncol=11 )
    colnames(genotype) <- c("gen", "SS1SS2", "SS1RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
    
    ## genotype frequencies before selection (a_gtypes) and after (a_gtypes_s)
    
    # HW equilibrium 
    genotype.freq <- make.genotypemat ( P_1, P_2 )
    a_gtypes <- createArray2( sex=c("m","f"), loci=rownames( genotype.freq ) )
    
    #setting genotype freq at start to same for m & f 
    a_gtypes['m', ] <- a_gtypes['f', ] <- genotype.freq[]
    
    # warning allows for rounding differences
    # only check 'm' because m&f set same above
    if ( !isTRUE( all.equal(1, sum(a_gtypes['m',])  )))
      warning("genotype frequencies before selection total != 1 ", sum(a_gtypes['m',]) ) 

    ## Single locus fitnesses in each niche
    a_fitloc <- fitnessSingleLocus(a_fitloc = a_fitloc,
                                   a_dom = a_dom,
                                   a_sel = a_sel,
                                   a_effect = a_effect,
                                   a_cost = a_cost)
    
    ## fitness of each genotype in each niche
    a_fitnic <- fitnessNiche( a_fitloc = a_fitloc,
                              a_nichetog = a_nichetog,
                              a_fitnic = a_fitnic )
    
    ## fitness of each genotype by sex
    a_fitgen <- fitnessGenotype( a_fitnic = a_fitnic, a_expos = a_expos, a_fitgen = a_fitgen )
 
    #testing
    # print("testing indiv fitness for exposure:")
    # df_indiv <- as.data.frame(a_fitgen)
    # # [1,] just prints males
    # print(as.data.frame(a_expos)[1,]) #exposure
    # print(df_indiv[1,])

           
    ##################
    ## generation loop
    for (gen_num in 1:max_gen){
      
      # In calibration 1011, selection relaxed for a set time
      if( calibration == 1011 & scen_num==2 ) a_fitgen <- relaxSelection(a_fitgen, gen_num)      
      
      # save record of genotype proportions each generation
      genotype[gen_num,1] <- gen_num	
      #genotype[gen_num,2:11] <- a_gtypes['m',]
      # 27/9/16 changing this to output mean of m&f
      #genotype[gen_num,2:11] <- sum( 0.5*(a_gtypes['f',] + a_gtypes['m',]))
      # 24/10/16 correcting bug above sum()
      genotype[gen_num,2:11] <- 0.5*(a_gtypes['f',] + a_gtypes['m',])
      
      # saving results 
      results <- resistance_freq_count( a_gtypes=a_gtypes, gen_num=gen_num, results=results )
      
      # previous sequential insecticide code that was here now done post-processing
      
      ## linkage calculations
      results <- linkage_calc( a_gtypes=a_gtypes, recomb_rate=recomb_rate, gen_num=gen_num, results=results )

      ## selection
      a_gtypes_s <- selection( a_gtypes=a_gtypes, a_fitgen=a_fitgen, calibration=calibration)

      ## Gametes from after selection
      G <- createGametes( a_gtypes=a_gtypes_s, recomb_rate=recomb_rate ) 
      
      ## Random Mating
      # males & females will only be different if sexLinked=TRUE
      
      # males (initially by calculating 'expanded' genotypes)
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=TRUE)
      a_gtypes['m',] <- genotypesLong2Short(fGenotypeExpanded)
      
      # females
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=FALSE)
      a_gtypes['f',] <- genotypesLong2Short(fGenotypeExpanded)      
      
      #calibration 102 : genotype frequencies reset to what they were at start of loop
      if( calibration == 102 ){ a_gtypes['m', ] <- a_gtypes['f', ] <- genotype.freq[] }
      
    }	## end of generation loop 
    
    ## Assign results matrices to lists for multiple runs
    listOut$results[[scen_num]] <- results
    listOut$genotype[[scen_num]] <- genotype
    # does this really output what we want ?
    listOut$fitness[[scen_num]] <- fitnessOutput( a_fitnic )    
    
    #25/0/16 can I add output of fitness by genotype and generation
    #BEWARE that this doesn't mess other stuff up
    listOut$fit_time_genotype <- fit_time_genotype(genotype, a_fitgen)
    
    ## Plots
    if( produce.plots ) plot_outputs_all( listOut=listOut, scen_num=scen_num, savePlots=savePlots)

  }		## end of scenarios loop (each column in input) 
  
  
  # return list of outputs
  invisible(listOut)
}