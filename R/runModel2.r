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
    

    ## genotype frequencies
    
    # frequencies of genotypes before selection - in HW equilibrium 
    genotype.freq <- make.genotypemat ( P_1, P_2 )
    
    namesLoci <- rownames( genotype.freq )
    sex2 <- c("m","f")
    # f  = genotype frequencies before selection
    # fs = genotype frequencies after selection
    f <- createArray2( sex=sex2, loci=namesLoci )
    fs <- createArray2( sex=sex2, loci=namesLoci )      
    
    #setting genotype freq at start to same for m & f 
    f['m', ] <- genotype.freq[]
    f['f', ] <- genotype.freq[]
    
    
    #these warnings allow for rounding differences
    if ( !isTRUE( all.equal(1, sum(f['m',])  )))
      warning("Male frequencies before selection total != 1 ", sum(f['m',]) ) 
    if ( !isTRUE( all.equal(1, sum(f['f',])  )))
      warning("Female frequencies before selection total != 1 ", sum(f['f',]) )  
    
    

    #########################
    ## Single locus fitnesses
    
    Wloci <- fitnessSingleLocus(Wloci=Wloci,
                                h = h,
                                s = s,
                                phi = phi,
                                z = z)
    
    
    ##################################################
    ### calculate two locus niche fitness in two insecticide Niche
    
    # multiply fitness of two insecticides
    # niches can be toggled off to get fitness of 0

    #refactored to replace 250+ lines in earlier version
#     for( niche1 in dimnames(Wniche)$niche1)
#     {
#       for( niche2 in dimnames(Wniche)$niche2)
#       {
    for( nicheNum1 in 1:3 ) #todo get this 1:3 from somewhere
    {
      for( nicheNum2 in 1:3 ) #todo get this 1:3 from somewhere
      { 
        #temporary solution
        #to get both niche (one of 0aAbB)
        #and exposure (one of no,lo,hi)
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
              ###########################################################################
              #6/1/16 i think ians new insecticide interaction parameter can just go here
              #does in need to be just one param or 4 ?
              #ΛAB, ΛAb, ΛaB or Λab 
              #Wniche[locus1,locus2,niche1,niche2] <- interaction * Wloci[locus1,exposure1] * Wloci[locus2,exposure2]
              Wniche[locus1,locus2,niche1,niche2] <- Wloci[locus1,exposure1] * Wloci[locus2,exposure2]
            }
          }          
        }
      }
    }
    
    #error check for fitnesses > 1 or < 0
    if ( any(Wniche > 1  ) ) 
      warning( sum(Wniche > 1  ), " niche fitness values (Wniche) are >1 ")
    if ( any( Wniche < 0 ) ) 
      warning( sum( Wniche < 0 ), " niche fitness values (Wniche) are <0")    
    
    #####################################################################
    ## calculate individual fitness based on exposure to niche & 2 locus fitness
    #weighted average of fitness depending on exposure
    
    for( sex in dimnames(Windiv)$sex)
    {
      for( locus1 in dimnames(Windiv)$locus1)
      {
        for( locus2 in dimnames(Windiv)$locus2)
        {
          # multiplies exposure by fitness for all niches & then sums
          # creates a weighted average of exposure in each niche
          Windiv[sex,locus1,locus2] <- sum( a[sex,,] * Wniche[locus1,locus2,,])
        }
      }
    }

    #error check for fitnesses > 1 or < 0
    if ( any(Windiv > 1 ) ) 
      warning( sum(Windiv > 1  ), " individual fitness values (Wloci) are >1")
    if ( any( Windiv < 0 ) ) 
      warning( sum( Windiv < 0 ), " individual fitness values (Wloci) are <0")
    
    #######################################################
    ## generation loop to run model from initial conditions
    #######################################################    
    
    #browser()
    
    for (k in 1:max_gen){
      
      # In calibration 1011, selection relaxed for a set time
      if( calibration == 1011 & i==2 ) Windiv <- relaxSelection(Windiv, k)      
      
      #genotype frequency code that was here now moved to before the loop start
      
      # save record of genotype proportions each generation
      genotype[k,1] <- k	
      #question is it right that only male frequencies seem to be saved ?
      genotype[k,2:11] <- f['m',]
      
      # Printing Results to matrix 
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
      

      # Gametes from before selection frequencies to estimate linkage disequilibrium 
      
      G <- createGametes( f = f, recomb_rate = recomb_rate ) 
      
      ## Linkage Disequilibrium of resistant allele in gametes
      # Male
      # Frequency of allele patterns
      x.R1.S2 <- G['m','R1','S2']/2
      x.S1.R2 <- G['m','R1','S2']/2
      x.R1.R2 <- G['m','R1','R2']
      
      # Frequency of alleles at each locus
      R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
      R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
      m.D <- x.R1.R2 - (R1 * R2)

      
      #Matt: Code reordered and edited to calculate D' for males and females separately. 
      
      # Finding D' for males
      D <- m.D	
      
      S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
      S2 <- 1 - R2
      
      p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
      p2q1 <- S1 * R2
      
      #dmax is the lower of these two
      if( p1q2 < p2q1 ){		
        dmax <- p1q2
      }else{
        dmax <- p2q1
      }
      
      p1q1 <- ( R1 * R2 )	#Find p1q1 and p2q2, given conditions above
      p2q2 <- ( S1 * S2 )
      
      if( p1q1  > p2q2 ){	#dmin is the lowest of these
        dmin <- p2q2
      }else{
        dmin <- p1q1
      }
      
      if( D>0 ){				# if D is greater than 0
        dprime.m <- D/dmax		# D' = D/dmax 
      }else{					# if D is less than 0
        dprime.m <- D/dmin		# D' = D/dmin
      }
      
      results[k,10] <- dprime.m	# prints to column ten of results matrix

      
      # Female
      # Frequency of allele patterns
      x.R1.S2 <- G['f','R1','S2']/2
      x.S1.R2 <- G['f','R1','S2']/2
      x.R1.R2 <- G['f','R1','R2']

      # Frequency of alleles at each locus
      R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
      R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
      f.D <- x.R1.R2 - (R1 * R2)
      
      # save linkage disequilibrium results
      results[k,4] <- m.D
      results[k,7] <- f.D

      
      # Matt: Finding D' for females
      D <- f.D		# D is given as male LD
      
      S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
      S2 <- 1 - R2
      
      p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
      p2q1 <- S1 * R2
      
      #dmax is the lower of these two
      if( p1q2 < p2q1 ){		
        dmax <- p1q2
      }else{
        dmax <- p2q1
      }
      
      p1q1 <- ( R1 * R2 )	#Find p1q1 and p2q2, given conditions above
      p2q2 <- ( S1 * S2 )
      
      if( p1q1  > p2q2 ){	#dmin is the lowest of these
        dmin <- p2q2
      }else{
        dmin <- p1q1
      }
      
      if( D>0 ){				# if D is greater than 0
        dprime.f <- D/dmax		# D' = D/dmax 
      }else{					# if D is less than 0
        dprime.f <- D/dmin		# D' = D/dmin
      }
      
      results[k,12] <- dprime.f	# prints to column twelve of results matrix
      
      
      ## R2
      denom <- sqrt(R1 * S1 * R2 * S2)	# finds R2 using the allale frequencies calculated above
      r2 <- D/denom						# use this and D to find r2
      
      results[k,11] <- r2					# prints to column eleven of results matrix
      
      
      
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
      
      # calibration 104, selection on one genotype
      if( calibration == 104 ){
        
        x.m <- select.gen.m				## Setting fitness of genotype to select on as separate variable
        x.f <- select.gen.f				## Not lost in reprinting in next step
        
        # copy after selection frequencies from those before selection to eliminate selection step
        fs <- f
        
        select.gen.m <- x.m			## Reprinting fitness that is intended to be selected on
        select.gen.f <- x.f			## with after selection fitness saved as variable above
        
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

      # Gametes produced are estimated by the frequency of the genotype and their contribution to each genotype of gamete
      # 1 - both parts of genotype contribute, 
      # 0.5 - half of genotype contributes, 
      # 0 - neither part of genotype can produce this gamete
      
      #note this uses fs, frequency of genotypes after selection
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