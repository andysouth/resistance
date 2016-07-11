#' plot Linkage Disequilibrium over generations
#' 
#' @param mat results matrix
#' 
#' @examples 
#' #single scenario
#' listOut <- resistSimple()
#' plotlinkage(listOut$results[[1]])

#' @return list of plot components
#' @export

plotlinkage <- function ( mat ){	
  
  par(pty="s") 		
  LD <- mat[,4]	 ## set LD column as vector (m LD = f LD)	
  gens <- mat[,1]									
  plot( gens, LD, type="l", axes=T,						## Blank square 1,1 plot
        xlim=c(1,(max(gens))), ylim=c(min(LD),max(LD)),
        xlab="Generation", ylab="Linkage Disequilibrium (D)", main="Linkage disequilibrium over generations.")	
}	