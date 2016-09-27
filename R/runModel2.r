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
    
    # to save parameters in a matrix, set coll.fitvals to 1
    coll.fitvals <- input[3,i]
    # to save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
    save.fitvals <- input[4,i]		
    
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
    
    #### end of reading inputs into parameters
    
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
    f <- fs <- createArray2( sex=c("m","f"), loci=namesLoci )
    
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
    ##################    
    
    for (k in 1:max_gen){
      
      # In calibration 1011, selection relaxed for a set time
      if( calibration == 1011 & i==2 ) Windiv <- relaxSelection(Windiv, k)      
      
      # save record of genotype proportions each generation
      genotype[k,1] <- k	
      # todo : question is it right that only male frequencies seem to be saved ?
      genotype[k,2:11] <- f['m',]
      
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
      
      # record total fitnesses for males and females
      #question aren't these always 1
      results[k,8] <- sum(f['m',])
      results[k,9] <- sum(f['f',])
      
      # previous sequential insecticide code that was here now done post-processing
      
      # linkage calculations
      results <- linkage_calc( freq=f, recomb_rate=recomb_rate, gen_num=k, results = results )

            
      #####################################################
      ## calculate genotype frequencies following selection
      
      if(calibration==103){		## no selection calibration
        
        # copy after selection frequencies from those before selection to eliminate selection step
        fs <- f
        
      }else{
        
        #todo : address comment from Ian on ms 12/2015
        #W.bar may not be necessary
        #I had originally normalised these finesses by dividing by  Wbar. 
        #In retrospect this was not necessary
        #Ian said thi is necessary at this stage to ensure that the gamete frequencies in each sex sum to 1
        
        # W bar - Sum of numerators
        W.bar <- createArray2(sex=c('m','f'))
        
        for( sex in dimnames(Windiv)$sex)
        {
          for( locus1 in dimnames(Windiv)$locus1)
          {
            for( locus2 in dimnames(Windiv)$locus2)
            {
              #have to do cis/trans specially
              if ( locus1=='RS1' & locus2=='RS2' )
              {
                W.bar[sex] = W.bar[sex] + (f[sex,'RS1RS2_cis'] * Windiv[sex,locus1,locus2])
                W.bar[sex] = W.bar[sex] + (f[sex,'RS1RS2_trans'] * Windiv[sex,locus1,locus2])
              }else
              {
                W.bar[sex] = W.bar[sex] + (f[sex,paste0(locus1,locus2)] * Windiv[sex,locus1,locus2])                
              }
            }
          }
        }
        
        # doing calculation using W.bar from above
        
        for( sex in dimnames(Windiv)$sex)
        {
          for( locus1 in dimnames(Windiv)$locus1)
          {
            for( locus2 in dimnames(Windiv)$locus2)
            {
              #have to do cis/trans specially
              if ( locus1=='RS1' & locus2=='RS2' )
              {
                fs[sex,'RS1RS2_cis']   <- (f[sex,'RS1RS2_cis'] * Windiv[sex,locus1,locus2]) / W.bar[sex]
                fs[sex,'RS1RS2_trans'] <- (f[sex,'RS1RS2_trans'] * Windiv[sex,locus1,locus2]) / W.bar[sex]
              }else
              {
                fs[sex,paste0(locus1,locus2)] <- (f[sex,paste0(locus1,locus2)] * Windiv[sex,locus1,locus2]) / W.bar[sex]                
              }
            }
          }
        }
      }
      
      # check that genotype frequencies total 1.
      for(sex in c('m','f'))
      {
        # allow for rounding differences
        if ( !isTRUE( all.equal(1, sum(fs[sex,])  )))
          warning(sex," genotype frequencies after selection total != 1 ", sum(fs[sex,]) )         
      }

      
      ###############################
      ## Gametes from after selection
      # fs = frequency of genotypes after selection
      G <- createGametes( f = fs, recomb_rate = recomb_rate ) 
      
      
      ###################
      ## Random Mating ##
      # males & females will only be different if sexLinked=TRUE
      
      # males
      # initially by calculating 'expanded' genotypes
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=TRUE)
      f['m',] <- genotypesLong2Short(fGenotypeExpanded)
      
      # females
      fGenotypeExpanded <- randomMating(G, sexLinked=sexLinked, isMale=FALSE)
      f['f',] <- genotypesLong2Short(fGenotypeExpanded)      
      
      
      #these warnings allow for rounding differences
      if ( !isTRUE( all.equal(1, sum(f['m',])  )))
        warning("Male genotype frequencies generation",k,", total != 1 ", sum(f['m',]) ) 
      if ( !isTRUE( all.equal(1, sum(f['f',])  )))
        warning("Female genotype frequencies generation",k,", total != 1 ", sum(f['f',]) )  
      
      #calibration 102 : genotype frequencies reset to what they were at start of loop
      if( calibration == 102 ){
        f['m', ] <- genotype.freq[]
        f['f', ] <- genotype.freq[]
      }
      
      
    }	####### end of generation loop 
    
    
    ## Assign results matrices to lists for multiple runs
    listOut$results[[i]] <- results
    listOut$genotype[[i]] <- genotype
    
    
    ## Plots ####
    if( produce.plots ) plot_outputs_all( listOut=listOut, scen_num=i, savePlots=savePlots)

    
    ## Prints fitnesses calculated by niche by genotype to matrix ##
    ## To save in .csv, enter save.param as TRUE above ##
    if( coll.fitvals == 1 ){
      
      fbn <- matrix( ncol=9, nrow=9 )
      
      colnames(fbn) <- c("-,-", "a,-", "A,-", "-,b", "-,B", "a,b", "A,B", "A,b", "a,B")
      #default order is different : 00 a0 A0 0b ab Ab 0B aB AB
      #potential problems in ordering of rows & cols avoided by filling by name below
      
      rownames(fbn) <- c("SS1SS2", "SS1RS2", "SS1RR2",
                         "RS1SS2", "RS1RS2", "RS1RR2",
                         "RR1SS2", "RR1RS2", "RR1RR2" )
      
      
      #!r refactored 81 lines of previous code with

      for( locus1 in dimnames(Wniche)$locus1)
      {
        for( locus2 in dimnames(Wniche)$locus2)
        {
          #this is a good way of doing but the columns end in a different order
          #which wouldn't be a problem except that initially I'm trying
          #to keep results identical to Beths
          #fbn[paste0(locus1,locus2),] <- Wniche[locus1,locus2,,]
          #so instead go through each niche
          for( niche1 in dimnames(Wniche)$niche1)
          {
            for( niche2 in dimnames(Wniche)$niche2)
            {
              #get columnName by converting 0 to - and inserting commas
              columnName <- paste0(niche1,",",niche2)
              columnName <- gsub('0','-',columnName)
              #get rowname by pasting
              rowName <- paste0(locus1,locus2)
              
              fbn[rowName,columnName] <- Wniche[locus1,locus2,niche1,niche2]
            }
          }
        }
      }
      
      
      listOut$fitness[[i]] <- fbn
      
    }					  
    if( save.fitvals==1 ){
      write.csv ( fbn, (paste(i,"two-locus_fitness-scores.csv")), row.names=T)
    }
    
    
  }		### loop through columns of parameter input table - produces results lists ###
  
  #return list of outputs
  invisible(listOut)
  
   
}