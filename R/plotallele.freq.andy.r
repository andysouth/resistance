#' plot allele frequencies over time for single or multiple scenarios
#' 
#' plot of resistance allele frequencies over time
#' Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
#' and same in female at locus 1 (red) and locus 2 (orange)
#' For multiple scenarios multipl;e lines are plotted.
#' todo : for multiple scenarios currently it accepts listOut, and gets results from listOut$results, I might instead want it to accept listOut$results.

#' @param mat results matrix or list of multiple matrices
#' @examples 
#' #single scenario
#' listOut <- resistSimple()
#' plotallele.freq.andy(listOut$results[[1]])
#' #mutliple scenarios
#' listOut <- resistanceMaster()
#' plotallele.freq.andy(listOut)
#' @return list of plot components
#' @export

plotallele.freq.andy <- function ( mat ){
  
  #if a list of multiple scenarios, get the max across all
  if (class(mat) == "list") 
    max_gen <- max( sapply(mat$results,nrow) )
  else 
    max_gen = nrow( mat )
  
  par(pty="s")   		
  plot( 1, 1, type="n", axes=T,						## Blank square 1,1 plot
        xlim=c(1,max_gen), ylim=c(0,1),
        xlab="Generation", ylab="Resistance Allele Frequency", main="Development of insecticide resistance")
  
  #if a list of multiple scenarios, plot the lines for each
  if (class(mat) == "list") 
    lapply(mat$results, function(x) plotalleleLinesOneScenario(x) )
  #if a single scenario just plot the lines for that
  else
    plotalleleLinesOneScenario(mat)
  
  
  legend( "bottomright", legend=c("locus1 male", "locus1 female", "locus2 male", "locus2 female"), 
          col=c("darkblue", "red", "green", "orange"), pch=c(16,16,16,16), bty="n" )
  
}

#' helper function to plot allele frequency lines for one scenario
#' 
#' @param mat results matrix
#' 
#' @return nothing
plotalleleLinesOneScenario <- function ( mat ){

  #line types(lty) to show all if overlapping, odd digits=line, even=gap
  # Males
  lines( mat[,1], mat[,2], col="darkblue", lwd=2, lty=13 )
  lines( mat[,1], mat[,3], col="green", lwd=2, lty=14 )
  # Females
  lines( mat[,1], mat[,5], col="red",lwd=2, lty=15 )
  lines( mat[,1], mat[,6], col="orange", lwd=2, lty=16 )
  
}

