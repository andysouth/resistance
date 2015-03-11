#' Return allele frequencies
#' 
#' extract just the R allele frequencies at a specified locus, for all scenarios run

#' @param locus locus of interest (i.e. 1 or 2)
#' @param max_gen number of generations 
#'    (should be the maximum number used across all runs, i.e. the longest any run is set for)
#' @param results.list list containing the results matrices generated for each scenario run
#' @param input todo  
#' 
#' @return matrix with rows for each generation, and columns for each scenario
#' @export


singlealleleFrequency <- function(locus, max_gen, results.list, input){
  
  k <- ncol(input)  	#number of scenarios is column number of input matrix
  
  freq <- matrix( ncol=k, nrow=max_gen )	#matrix with column for each scenario of interest and row for freq of R at each generation
  rownames(freq) <- c(1:max_gen)
  colnames(freq) <- colnames(input)
  
  
  if(locus==1){
    col <- 2			#if locus 1 is selected, the results are extracted from column 2 of the results matrix
  }else{
    col <- 3			#if locus 2 is selected, the results are extracted from column 3 of the results matrix
  }
  
  for (i in 1:k){
    mat <- results.list[[i]]		# for each scenario, extracts matrix of results from results list
    res <- mat[,col]				# extracts column of results for desired locus from the matrix
    
    if( length(res) != max_gen ){		# if the length of the vector produced is not equal to the value set for max_gen
      diff <- max_gen - length(res)	# finds difference in lengths between them
      add <- c(rep(NA,diff))			# generates a vector of NA's of the length of the difference
      res <- c(res,add)				# concatenates this vector to make the vector the same length as the maximum length generated
    }
    
    freq[,i] <- res					# for that scenario run, prints to corresponding column in the allele frequency matrix
  }
  
  return( freq )
}