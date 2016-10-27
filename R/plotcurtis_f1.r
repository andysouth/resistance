#' plot Resistance Allele frequencies over time
#' 
#' Recreates figure 1 of Curtis(1985)
#' Plot of R at each locus (R1=R2, as in Curtis fig 1)
#' Prints total frequency of R allele at locus 1 (red) and locus 2 (blue)
#' Points plotted in purple indicate generations where selection is relaxed
#' 
#' @param nrelaxmat results matrix without relaxation of selection
#' @param relaxmat results matrix with relaxation of selection 
#' @param gencol column number containing generation
#' @param r1col column number containing frequency of allele at R1
#' 
#' @return list of plot components
#' @export

plotcurtis_f1 <- function ( nrelaxmat, relaxmat, gencol, r1col ){
  
  f <- c(0.1,0.5,1,5)				## for y axis 
  fl <- log10(f)
  labs <- c("0.1%", "0.5%", "1%", "5%")	
  
  # set frequencies in vectors as log10 percentages from frequencies
  nrelax <- log10( 100 * nrelaxmat[,r1col] )			# only prints results from males, m = f, A[R] = B[R], so only prints one col
  gens <- nrelaxmat[,gencol]	
  
  #tcl for smaller ticks (def=-0.5)
  #mgp margin line for axis title, labels and line.
  #mgp[1] affects title whereas mgp[2:3] affect axis. default is c(3, 1, 0).
  par(pty="s", tcl=-0.2, mgp=c(2.5, 0.5, 0) ) #, plt=c(0.1,0.8,0.1,0.8)) 
  
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
        xlim=c(1,(max(nrelaxmat[,gencol]))), ylim=c((min(fl)),(max(fl))),
        xlab="", ylab="", main="")
  
  #to put titles closer to axes
  title(xlab="Generation", ylab="Allele Frequency",line=2)
  
  axis( side=1, at=c(0,2,4,6,8,10,12,14,16,18), labels=c(0,2,4,6,8,10,12,14,16,18), tick=T )
  axis( side=2, at=fl, labels=labs, tick=T )
  
  lines( gens, nrelax, col="black" )
  
  ## relaxed selection
  relax <- relaxmat[,r1col]
  relax <- log10( 100 * relax )
  
  lines( gens, relax )
  points( c(3:12), relax[3:12], col="darkviolet", pch=16 )
  points( c(16:18), relax[16:18], col="darkviolet", pch=16 )
  
  #abline( h=(log10(5) ) )
  
  
  pos <- log10( 4.5 )
  legend( 7, pos , legend=c("selection by mixture", "selection relaxed"), 
          col=c("black", "darkviolet"),lty=c(1,0), pch=c(NA,16), bty="n", cex=0.7 )
  
  box()
  
}