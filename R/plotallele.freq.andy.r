#' plot allele frequencies over time, version modified by andy
#' 
#' plot of R and S allele frequencies over time
#' Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
#' and same in female at locus 1 (red) and locus 2 (orange)

#' @param mat results matrix
#' 
#' @return list of plot components
#' @export

plotallele.freq.andy <- function ( mat ){
  
  #andy added
  max_gen = nrow( mat )
  
  par(pty="s")   		
  plot( 1, 1, type="n", axes=T,						## Blank square 1,1 plot
        xlim=c(1,max_gen), ylim=c(0,1),
        xlab="Generation", ylab="Resistance Allele Frequency", main="Development of insecticide resistance")
  
  # Resistant Allele
  #line types(lty) to show all if overlapping, odd digits=line, even=gap
  # Males
  lines( mat[,1], mat[,2], col="darkblue", lwd=2, lty=13 )
  lines( mat[,1], mat[,3], col="green", lwd=2, lty=14 )
  # Females
  lines( mat[,1], mat[,5], col="red",lwd=2, lty=15 )
  lines( mat[,1], mat[,6], col="orange", lwd=2, lty=16 )
  
  legend( "bottomright", legend=c("locus1 male", "locus1 female", "locus2 male", "locus2 female"), 
          col=c("darkblue", "red", "green", "orange"), pch=c(16,16,16,16), bty="n" )
  
}