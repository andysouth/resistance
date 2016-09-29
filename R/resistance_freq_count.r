#' count resistance allele frequencies and optionally save to results
#' 
#' 
#' 
#' @param fgenotypes array of genotype frequencies
#' @param gen_num generation number
#' @param results matrix
#' 
#' @examples 
#' genotype.freq <- make.genotypemat ( 0.5, 0.5 )
#' fgenotypes <- createArray2( sex=c("m","f"), loci=rownames( genotype.freq ) )
#' fgenotypes['m', ] <- fgenotypes['f', ] <- genotype.freq[]
#' resistance_freq_count(fgenotypes)

#' @return results matrix with values for this generation added OR an array with just these results
#' @export

resistance_freq_count <- function ( fgenotypes, 
                                    gen_num = NULL,
                                    results = NULL ){	

  
  #this could be extended to have >2 loci
  arr_freq <- createArray2( sex=c('m','f'), locus = c('R1','R2') ) 
   
  ## frequency of resistance alleles
  # SS1SS2,SS1RS2,SS1RR2,RS1SS2,RS1RS2_cis,RS1RS2_trans,RS1RR2,RR1SS2,RR1RS2,RR1RR2
  names_genotypes <- colnames(fgenotypes)
  arr_freq['m','R1'] <- sum(fgenotypes['m',grep("RR1",names_genotypes)]) + ( 0.5 * sum(fgenotypes['m',grep("RS1",names_genotypes)]))
  arr_freq['m','R2'] <- sum(fgenotypes['m',grep("RR2",names_genotypes)]) + ( 0.5 * sum(fgenotypes['m',grep("RS2",names_genotypes)]))
  arr_freq['f','R1'] <- sum(fgenotypes['f',grep("RR1",names_genotypes)]) + ( 0.5 * sum(fgenotypes['f',grep("RS1",names_genotypes)]))
  arr_freq['f','R2'] <- sum(fgenotypes['f',grep("RR2",names_genotypes)]) + ( 0.5 * sum(fgenotypes['f',grep("RS2",names_genotypes)]))   
  
  # returning results, either to the passed results matrix or as a new object
  if ( !is.null(results) & !is.null(gen_num) )
  {
    results[gen_num,2] <- arr_freq['m','R1']
    results[gen_num,3] <- arr_freq['m','R2']
    results[gen_num,5] <- arr_freq['f','R1']
    results[gen_num,6] <- arr_freq['f','R2']
    
    # record total fitnesses for m & f
    # which are always 1, not sure why Beth has here ?
    results[gen_num,8] <- sum(fgenotypes['m',])
    results[gen_num,9] <- sum(fgenotypes['f',])
    
  } else 
  {
    results <- arr_freq
  }
  
  return(results)
  
}	