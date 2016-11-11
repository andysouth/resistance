#' plot linkage disequilibrium over time
#' 
#' Points plotted in purple indicate generations where selection is relaxed
#' 
#' @param nrelaxmat results matrix without relaxation of selection
#' @param relaxmat results matrix with relaxation of selection 
#' @param gencol column number containing generation (1)
#' @param ldcol column number containing linkage disequilibrium (4)
#' 
#' @return list of plot components
#' @export

plotcurtis_ld <- function(nrelaxmat, relaxmat, gencol, ldcol){
  
  par(pty="s") ##, mfrow=c(2,1))		# two rows, one column 
  
  ## LD plot at top
  ymin <- log10(0.000001)
  ymax <- log10(0.01)
  
  res <- log10( nrelaxmat[,ldcol])
  rel <- log10( relaxmat[,ldcol])
  
  gens <- nrelaxmat[,gencol]
  
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
        xlim=c(1,(max(nrelaxmat[,gencol]))), ylim=c(ymin,ymax),
        xlab="Generation", ylab="Index of Linkage Disequilibrium - D", main="")#"Linkage Disequilibrium")
  
  lines( gens, res )		#plot log10 LD against generation
  lines( gens, rel )
  
  points( c(3:12), rel[3:12], col="darkviolet", pch=16 )		# add purple points on relaxed line to indicate relaxed generations
  points( c(15:18), rel[15:18], col="darkviolet", pch=16 )
  
  axis( side=1, at=c(0,2,4,6,8,10,12,14,16,18), labels=c(0,2,4,6,8,10,12,14,16,18), tick=T, cex=0.7 )
  
  labs <- c(0.000001,0.00001,0.0001,0.001,0.01)
  labpos <- log10(labs)
  
  axis( side=2, at=labpos, labels=labs, tick=T )
  
  pos <- log10( 0.01 )
  legend( 7, pos , legend=c("Linkage disequilbrium","With relaxed selection"), 
          col=c("black", "darkviolet"),lty=c(1,0), pch=c(NA,16), bty="n", cex=0.7 )
  
  box()
  
}