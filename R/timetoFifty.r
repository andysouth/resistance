#' Find number of generations to 0.5
#' 
#' Takes the matrix produced by single allele frequency function 
#' and finds the number of generations it takes for the R allele frequency to reach 0.5

#' @param locus locus of interest (i.e. 1 or 2)
#' @param max_gen number of generations 
#'    (should be the maximum number used across all runs, i.e. the longest any run is set for)
#' @param results.list list containing the results matrices generated for each scenario run
#' @param input todo  
#' 
#' @return matrix with a column for each scenario, giving the generation number at which frequency of R is first >= 0.5
#' @export


timetoFifty <- function( locus, max_gen, results.list, input ){
  
  frequencymat <- singlealleleFrequency( locus, max_gen, results.list, input )#produces matrix of single allele frequencies 
  # then uses this to run the time to fifty function to find the first generation at which frequency of R is >= 0.5
  k <- ncol(frequencymat)
  # create matrix to print results to
  resmat <- matrix( ncol = k, nrow=1 )
  rownames(resmat) <- "Gens to 0.5"
  colnames(resmat) <- colnames(input)  	#sets colnames as colnames in input to describe each scenario
  
  for( k in 1:ncol(frequencymat)){		#loops through columns of frequency mat( i.e. each scenario )
    col <- frequencymat[,k]				#sets each column in turn as a vector
    y <- which(col>=0.5)				#finds all generation values where frequency is greater than or equal to 0.5
    z <- y[1]							#finds the first generation at which frequency is greater than or euql to 0.5
    resmat[,k] <- z						#in the corresponding scenario column in the new matrix, prints the first gen at which >= 0.5
  }
  
  return(resmat)
}