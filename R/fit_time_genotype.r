#' calculate fitness by genotype over time
#' 
#' allows calling from runModel2() and independently 

#' @param genotype matrix of genotype frequencies over time
#' @param a_fitgen array of genotype fitnesses
#' @param plot whether to plot

#' 
#' @examples 
#' fit_time_genotype()

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
    a_fitgen  <- createArray2( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
  }

  #what format do I want the output to be in ?
  #an array or a dataframe ? might be too many dimensions for a dataframe
  #a_fit_time_gen[gen_num,sex,locus1,locus2]
  #a_fit_time_gen <- 
  # could be a bit like the genotype matrix (1 less column for no cis/trans)
  # but would need an extra dimension for gender
  # maybe start by doing for just m
  a_fit_time_gen <- matrix( nrow=max_gen, ncol=10 )
  colnames(a_fit_time_gen) <- c("gen", "SS1SS2", "SS1RS2", "SS1RR2", 
                          "RS1SS2", "RS1RS2", "RS1RR2",
                          "RR1SS2", "RR1RS2", "RR1RR2")
  
  #for each generation 
  max_gen <- nrow(genotype) 
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
            a_fit_time_gen[gen_num, sex,locus1,locus2] <- 
              genotype[gen_num, 'RS1RS2_cis'] * a_fitgen[sex,locus1,locus2]) +
              genotype[gen_num, 'RS1RS2_trans'] * a_fitgen[sex,locus1,locus2])      

          }else
          {
            a_fit_time_gen[gen_num,sex,locus1,locus2] <- genotype[gen_num, paste0(locus1,locus2)] * a_fitgen[sex,locus1,locus2])
          }
          
          
          

        }
      }
    }
  
  #error check for fitnesses > 1 or < 0
  if ( any(a_fitgen > 1 ) ) 
    warning( sum(a_fitgen > 1  ), " individual fitness values (a_fitgen) are >1")
  if ( any( a_fitgen < 0 ) ) 
    warning( sum( a_fitgen < 0 ), " individual fitness values (a_fitgen) are <0")
  
  #testing
  #cat("in fitnessGenotype\n")
  #df_indiv <- as.data.frame(a_fitgen)
  #print(df_indiv[1,]) #just m
  
  if (plot)
  {
    #transpose to get in useable format
    cat('plot')
  }
  
  return(a_fit_time_gen)
}