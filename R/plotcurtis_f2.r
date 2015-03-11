#' plot Resistance Allele frequencies over time
#' 
#' Recreates figure 2 of Curtis(1985)
#' Plot of R at each locus 
#' Prints total frequency of R allele at locus 1 (red) and locus 2 (blue)
#' 
#' @param combmat todo
#' @param bmat todo 
#' @param amat todo
#' @param gencol column number containing generation
#' @param r1col column number containing frequency of allele at R1
#' @param r2col column number containing frequency of allele at R2
#' 
#' @return list of plot components
#' @export

plotcurtis_f2 <- function( combmat, bmat, amat, gencol, r1col, r2col ){
  
  f <- c(1:100)				## for y axis 
  fl <- log10(f)				
  par(pty="s") 
  # blank plot with x axis max as max_gen for combination scenario, and y axis max as log10(100) (for 100%)			
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
        xlim=c(1,(max(combmat[,gencol]))), ylim=c(0,(max(fl))),
        xlab="Generation", ylab="Allele Frequency", main="Frequency of R allele through time.")
  
  # plotting combination scenario (in combmat)
  # set frequencies in vectors as log10 percentages from frequencies
  R1 <- log10( 100 * combmat[,r1col] )
  R2 <- log10( 100 * combmat[,r2col] )
  # set generations in vector
  gens <- combmat[,gencol]		
  # add alele frequencies as lines to the plot
  lines( gens, R1, col="red" )
  lines( gens, R2, col="darkblue" )
  # add line at 50%
  abline( h=(log10(50) ) )
  # add axis labels
  axis( side=1, at=c(0,20,40,60,80,100,120,140,160), tick=T )
  ylabs <- c(1,5,10,50)
  ylabsnames <- c("1%", "5%", "10%", "50%")
  logylabs <- log10( ylabs )
  axis( side=2, at=logylabs, labels=ylabsnames, tick=T )
  
  ## Single insecticide scenarios
  # Only plots up to generation where 0.5 is first reached/exceeded, so this generation is found
  hch_cutoff <- min(which((bmat[,r2col])>0.5))
  ddt_cutoff <- min(which((amat[,r1col])>0.5))
  
  # set results columns as vectors from single insecticide treatment
  hch <- bmat[1:hch_cutoff,r2col]	# trim vector to point where freq of R > 0.5
  hch <- log10( 100 * hch )
  ddt <- amat[1:ddt_cutoff,r1col]	# trim vector to point where freq of R > 0.5
  ddt <- log10( 100 * ddt )
  
  hch_gens <- c(1:hch_cutoff)
  ddt_gens <- c((hch_cutoff):(hch_cutoff+(ddt_cutoff-1)))
  
  
  # add these as lines
  lines( hch_gens, hch, col="blue", lty=2 )
  lines( ddt_gens, ddt, col="red", lty=2 )
  
  abline( v = 31, col="black" )
  
  yval <- log10( 2 )
  legend( 105, yval, legend=c("Sequential", "Combination","R at Locus 1 (DDT)", "R at Locus 2(HCH)"), 
          col=c("black","black","red", "darkblue"), lty=c(2,1,1,1), bty="n", cex=0.7 )
  
  yval <- log10( 3 )			
  text(27, yval, "Switch to DDT", srt=90, cex=0.75) 			
  
  box()
  
}