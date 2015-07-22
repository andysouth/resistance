#' plot Resistance Allele frequencies over time
#' 
#' Recreates figure 2 of Curtis(1985)
#' Plot of R at each locus 
#' Prints total frequency of R allele at locus 1 (red) and locus 2 (blue)
#' 
#' @param combmat combined insecticide results matrix 
#' @param bmat single insecticide (hch) results matrix 
#' @param amat single insecticide (ddt) results matrix
#' @param gencol column number containing generation
#' @param r1col column number containing frequency of allele at R1
#' @param r2col column number containing frequency of allele at R2
#' @param main title for the plot
#' @param criticalPoints critical resistance points to be displayed in the plot
#' @param addCombinedStrategy whether to add a line for the combined mixture/individual strategy, makes plot confusing so set to FALSE by default
#' 
#' @examples
#' inputAndResultsMix <- sensiAnPaperPart( 2, insecticideUsed = 'mixture' )
#' inputAndResultsI1 <- sensiAnPaperPart( 2, insecticideUsed = 'insecticide1' )
#' inputAndResultsI2 <- sensiAnPaperPart( 2, insecticideUsed = 'insecticide2' )
#' plotcurtis_f2_generic( listOutMix$results[[1]], listOutI2$results[[1]], listOutI1$results[[1]] )
#' 
#' @return list of plot components
#' @export

plotcurtis_f2_generic <- function( combmat, bmat, amat, gencol=1, r1col=2, r2col=3,
                                   main="Rise of resistance; sequential insecticides v mixtures",
                                   criticalPoints = c(0.1,0.25,0.5),
                                   addCombinedStrategy = FALSE
                                   ){
  
  #find generations to reach max resistance points
  #to help with setting x axis
  maxGensI1 <- findResistancePoints(amat, locus=1, criticalPoints = criticalPoints)
  maxGensI2 <- findResistancePoints(bmat, locus=2, criticalPoints = criticalPoints)  
  maxGensMix <- findResistancePoints(combmat, locus='both', criticalPoints = criticalPoints)    
  
  maxGens <- max( (maxGensI1+maxGensI2), maxGensMix )
  
  minf <- min( combmat[,r1col], combmat[,r2col] )
  #cat("in plotcurtis_f2_generic minf:",minf," ", log10(minf),"\n")
  
  f <- c((100*minf):100)				## for y axis
  #f <- c(1:100)				## for y axis 
  fl <- log10(f)				
  par(pty="s") 
  # blank plot with x axis max as max_gen for combination scenario, and y axis max as log10(100) (for 100%)			
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
        #xlim=c(1,maxGens), ylim=c(0,(max(fl))),
        xlim=c(1,maxGens), ylim=c(min(fl),(max(fl))),
        #xlim=c(1,(max(combmat[,gencol]))), ylim=c(0,(max(fl))),
        xlab="Generation", ylab="Allele Frequency", main=main)
  
  # plotting combination scenario (in combmat)
  # set frequencies in vectors as log10 percentages from frequencies
  R1 <- log10( 100 * combmat[,r1col] )
  R2 <- log10( 100 * combmat[,r2col] )
  # set generations in vector
  gens <- combmat[,gencol]		
  # add allele frequencies as lines to the plot
  lines( gens, R1, col="red" )
  lines( gens, R2, col="blue" )
  
  # add line at maximum of critical points
  abline( h=(log10(100*max(criticalPoints)) ) )
  
  # add axis labels
  #axis( side=1, at=c(0,20,40,60,80,100,120,140,160), tick=T )
  #default x axis
  axis( side=1 )
#   ylabs <- c(1,5,10,50)
#   ylabsnames <- c("1%", "5%", "10%", "50%")
  ylabs <- c(0.1,1,5,10,50)
  ylabsnames <- c("0.1%", "1%", "5%", "10%", "50%")
  logylabs <- log10( ylabs )
  axis( side=2, at=logylabs, labels=ylabsnames, tick=T )
  
  ## Single insecticide scenarios
  #andy remember that in this from Beth hch is insecticide1 & ddt is insecticide2
  
  # Only plots up to generation where 0.5 is first reached/exceeded, so this generation is found
  hch_cutoff <- min(which((amat[,r1col])>max(criticalPoints)))
  ddt_cutoff <- min(which((bmat[,r2col])>max(criticalPoints)))
  
  # set results columns as vectors from single insecticide treatment
  hch <- amat[1:hch_cutoff,r1col]	# trim vector to point where freq of R > 0.5
  hch <- log10( 100 * hch )
  ddt <- bmat[1:ddt_cutoff,r2col]	# trim vector to point where freq of R > 0.5
  ddt <- log10( 100 * ddt )
  
  hch_gens <- c(1:hch_cutoff)
  ddt_gens <- c((hch_cutoff):(hch_cutoff+(ddt_cutoff-1)))
  
  # add these as lines
  lines( hch_gens, hch, col="red", lty=2 )
  lines( ddt_gens, ddt, col="blue", lty=2 )

  #andy added
  if( addCombinedStrategy )
  {
    cutoff_i1_mix <- min(which((combmat[,r1col])>max(criticalPoints)))
    cutoff_i2_mix <- min(which((combmat[,r2col])>max(criticalPoints)))      
    
    #trying to add a dotted line for the combined strategy
    #it should start at the point that the resistance to the first insecticide is reached
    #and go parallel to the other individual insecticide after
    #but remember that resistance may be reached for insecticide2 first
    if ( cutoff_i1_mix < cutoff_i2_mix )
    {
      #finding which generations
      #initially just use maxGens for end
      #gens <- c( cutoff_i1_mix : (maxGens-cutoff_i1_mix) )
      gens <- c( cutoff_i1_mix : (cutoff_i1_mix+length(ddt)-1 ) )
      #this is tricky
      #have to log the starting point
      #find resistance level for i2 at the point the cutoff is reached for i1
      startR <- log10( 100 * combmat[cutoff_i1_mix,r2col] )      
      #add the individual use line, with the difference between the indiv start and the new start
      lines( gens, ddt-ddt[1]+startR, col="blue", lty=3 )
      
    } else #if cutoff_i2_mix <= cutoff_i1_mix
    {
      
      gens <- c( cutoff_i2_mix : (cutoff_i2_mix+length(hch)-1 ) )

      #find resistance level for i1 at the point the cutoff is reached for i2            
      startR <- log10( 100 * combmat[cutoff_i2_mix,r1col] )      
      #add the individual use line, with the difference between the indiv start and the new start     
      lines( gens, hch-hch[1]+startR, col="red", lty=3 )   
      
    }
    

  }  
  
  
  
  #when the switch is made, was hardcoded by Beth
  #abline( v = 31, col="black" )
  #yval <- log10( 3 )			
  #text(27, yval, "Switch to DDT", srt=90, cex=0.75) 
  
  #when switch made from I1 to I2
  abline( v = max(maxGensI1), col="grey", lty=2 )  
  
#   yval <- log10( 2 )
#   legend( 105, yval, legend=c("Sequential", "Combination","R at Locus 1 (DDT)", "R at Locus 2(HCH)"), 
#           col=c("black","black","red", "darkblue"), lty=c(2,1,1,1), bty="n", cex=0.7 )
  if( addCombinedStrategy )
  {
    #added a dotted line for combined
    legend( 'bottomright', legend=c("sequential", "mixture", "combined", "insecticide 1", "insecticide 2"), 
            col=c("black","black","black","red", "blue"), lty=c(2,1,3,1,1), bty="n", cex=0.7 )     
  } else
  {
    legend( 'bottomright', legend=c("sequential", "mixture", "insecticide 1", "insecticide 2"), 
            col=c("black","black","red", "blue"), lty=c(2,1,1,1), bty="n", cex=0.7 )    
  }
 

    			
  
  box()
  
}