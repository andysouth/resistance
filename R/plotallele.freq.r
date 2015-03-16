#' plot allele frequencies over time
#' 
#' plot of R and S allele frequencies over time
#' Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
#' and same in female at locus 1 (red) and locus 2 (orange)

#' @param mat results matrix
#' 
#' @return list of plot components
#' @export

plotallele.freq <- function ( mat ){
  
  #andy added
  max_gen = nrow( mat )
  
  par(pty="s")   		
  plot( 1, 1, type="n", axes=T,						## Blank square 1,1 plot
        xlim=c(1,max_gen), ylim=c(0,1),
        xlab="Generation", ylab="Allele Frequency", main="Frequency of R allele across 2 loci by sex.")
  
  # Resistant Allele
  # Males
  lines( mat[,1], mat[,2], col="darkblue", lwd=2 )
  lines( mat[,1], mat[,3], col="green", lwd=2 )
  # Females
  lines( mat[,1], mat[,5], col="red",lwd=2 )
  lines( mat[,1], mat[,6], col="orange", lwd=2 )
  
  legend( "topleft", legend=c("Male - L1", "Male - L2", "Female - L1", "Female - L2"), 
          col=c("darkblue", "green", "red", "orange"), pch=c(16,16,16,16), bty="n" )
  
}