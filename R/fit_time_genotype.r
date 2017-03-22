#' calculate fitness by genotype over time
#' 
#' allows calling from runModel2() and independently 

#' @param genotype matrix of genotype frequencies over time
#' @param a_fitgen array of genotype fitnesses
#' @param plot whether to plot

#' 
#' @examples 
#' fit_time_genotype()
#' #to get fitness over time for females
#' #listOut$fit_time_genotype[,'f',]

#' @return fitness values in an array
#' @export

fit_time_genotype <- function ( genotype = NULL,
                                a_fitgen = NULL,
                                plot = FALSE )
{
  #I'm not sure if these default args will work
  # to allow this function to be called with no args
  if ( is.null(genotype) )
  {
    max_gen <- 10
    genotype <- matrix( 0,nrow=max_gen, ncol=11 )
    colnames(genotype) <- c("gen", "SS1SS2", "SS1RS2", "SS1RR2", 
                            "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                            "RR1SS2", "RR1RS2", "RR1RR2")
  }
  
  if ( is.null(a_fitgen) )
  {
    # empty array to fill
    a_fitgen  <- array_named( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
  }

  #what format do I want the output to be in ?
  #an array or a dataframe ? might be too many dimensions for a dataframe
  #a_fit_time_gen[gen_num,sex,locus1,locus2]
  #a_fit_time_gen <- 
  # could be a bit like the genotype matrix (1 less column for no cis/trans)
  # but would need an extra dimension for gender
  # maybe start by doing for just m
  # a_fit_time_gen <- matrix( nrow=max_gen, ncol=10 )
  # colnames(a_fit_time_gen) <- c("gen", "SS1SS2", "SS1RS2", "SS1RR2", 
  #                         "RS1SS2", "RS1RS2", "RS1RR2",
  #                         "RR1SS2", "RR1RS2", "RR1RR2")
  
  
  max_gen <- nrow(genotype) 
  
  a_fit_time_gen  <- array_named( gen = c(1:max_gen), 
                                   sex=c('m','f'), 
                                   genotype = c("SS1SS2", "SS1RS2", "SS1RR2", 
                                                "RS1SS2", "RS1RS2", "RS1RR2",
                                                "RR1SS2", "RR1RS2", "RR1RR2", "variance") )
  
  #for each generation 
  for (gen_num in 1:max_gen)
  {
    for( sex in dimnames(a_fitgen)$sex)
    {
      for( locus1 in dimnames(a_fitgen)$locus1)
      {
        for( locus2 in dimnames(a_fitgen)$locus2)
        {
          # multiply genotype frequency by fitness by genotype
          # have to do cis/trans specially
          if ( locus1=='RS1' & locus2=='RS2' )
          {
            #TODO check that it's ok to ad this and not to calc the mean
            #ok adding the freqs but not sure about the fitness component
            a_fit_time_gen[gen_num, sex, paste0(locus1,locus2)] <- 
              genotype[gen_num, 'RS1RS2_cis'] * a_fitgen[sex,locus1,locus2] +
              genotype[gen_num, 'RS1RS2_trans'] * a_fitgen[sex,locus1,locus2]      

          }else
          {
            a_fit_time_gen[gen_num,sex,paste0(locus1,locus2)] <- genotype[gen_num, paste0(locus1,locus2)] * a_fitgen[sex,locus1,locus2]
          }
        }
      }
    }
  } #end gen_num loop

  # to calculate variance in genotype fitness over time
  # using a loop because I'm tired !
  # BUT ian says the variance wants to be calcualted in a different way, not just from the 9 numbers
  # instead it needs to take into account that there are very few of the low frequency ones 
  # for (gen_num in 1:max_gen)
  # {
  #   for( sex in dimnames(a_fitgen)$sex)
  #   {
  #     a_fit_time_gen[gen_num,sex,'variance'] <- var(a_fit_time_gen[gen_num,sex,]) 
  #   }
  # }
 
  # 25/10/16 Ians new variance calc
  # similar (but not same) to what hapens in selection function
  #mean fitness = mean( fitness per genotype * genotype frequency )
  #andy I think this may be the mean of the genotype columns a_fit_time_gen 
  #ian said freqs must be before selection. Yes the genotype freq matrix is from before selection
  #sum( (fitness of each genotype - mean fitness)2 * freq_of_genotype ) #squared
  #TODO Ian expects this curve to go up & down, it still just goes up
  #probably a problem ith the eq in line 115 below
  #AHA 2nd problem was that genotype includes cis & trans
  #quick fix
  #TODO make a safer version of this !!
  gen2 <- genotype
  gen2[,'RS1RS2_cis'] <- gen2[,'RS1RS2_cis'] + gen2[,'RS1RS2_trans']
  colnames(gen2)[ which(colnames(gen2)=='RS1RS2_cis')] <- 'RS1RS2'
  gen2 <- gen2[,- which(colnames(gen2)=='RS1RS2_trans')]
  
  for (gen_num in 1:max_gen)
  {
    for( sex in dimnames(a_fitgen)$sex)
    {
      sum_fit_genotype <- sum( a_fit_time_gen[gen_num,sex,-which(dimnames(a_fit_time_gen)$genotype=='variance')] )
      
      #BEWARE DANGEROUS this relies on genotype columns being in same order
      #-1 is to miss out the first column of the matrix with gen num in
      
      #TODO try to break this equation down so it's easier to test what it's doing
      #AHA! problem with this maybe that a_fit_time_gen[gen_num,sex,] includes the variance column

      intermediate <- ((a_fit_time_gen[gen_num,sex,-which(dimnames(a_fit_time_gen)$genotype=='variance')] - sum_fit_genotype)^2) * gen2[gen_num,-1]
      
      a_fit_time_gen[gen_num,sex,'variance'] <- sum( intermediate )  
    }
  }
  
  #Ian suggested new potential way by replicating each fitness value by 10,000 * frequency
  #at least I could try that & see if it gives same results as before
  # for (gen_num in 1:max_gen)
  # {
  #   for( sex in dimnames(a_fitgen)$sex)
  #   {
  #     
  #     #TODO try to break this equation down so it's easier to test what it's doing
  #     intermediate <- ((a_fit_time_gen[gen_num,sex,] - sum_fit_genotype)^2) * genotype[gen_num,-1]
  #     
  #     a_fit_time_gen[gen_num,sex,'variance'] <- sum( intermediate )  
  #   }
  # }  
  
  
  #testing
  #cat("in fitnessGenotype\n")
  #df_indiv <- as.data.frame(a_fitgen)
  #print(df_indiv[1,]) #just m
  
  if (plot)
  {
    layout(matrix(c(1:2),2,1))
    plot(a_fit_time_gen[,'f','variance'])
    plot(a_fit_time_gen[,'f','RR1RR2'])
  }
  
  return(a_fit_time_gen)
}