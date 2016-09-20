#' plot Haplotype
#' 
#' Plots numbers of homozygote S, heterozygote and homozygote R at each generation (from genotype matrix)
#' locus 1: SS in pink, RS in orange, RR in red
#' locus 2: SS in cyan, RS in dark blue, RR in green

#' @param mat results matrix
#' @examples 
#' input <- setInputOneScenario()
#' listOut <- runModel2(input)
#' plothaplotype( listOut$genotype[[1]] )
#' 
#' @return  list of plot components
#' @export


plothaplotype <- function( mat ){

  #andy moved
  max_gen = nrow( mat )
    
  par(pty="s")   		
  plot( 1,1, type="n", axes=T,						## Blank square 1,1 plot
        xlim=c(0,max_gen), ylim=c(0,1),
        xlab="Generation", ylab="Frequency of haplotype", main="Frequency of haplotypes across 2 loci by generation.")

  toplot <- matrix( nrow = max_gen, ncol=7 )			## matrix of generation and haplotype frequency 
  
  for (k in 1:nrow(mat)){			
    ## haplotype at locus 1
    m.SS1 <- sum( mat[k,2], mat[k,3], mat[k,4] ) #SS1SS2, SS1SR2, SS1RR2
    m.RS1 <- sum( mat[k,5] + mat[k,6] + mat[k,7] + mat[k,8] )
    m.RR1 <- sum( mat[k,9] + mat[k,10] + mat[k,11] )
    ## haplotype at locus 2
    m.SS2 <- sum( mat[k,2] + mat[k,5] + mat[k,9] )
    m.RS2 <- sum( mat[k,3] + mat[k,6] + mat[k,7] + mat[k,10] )
    m.RR2 <- sum( mat[k,4] + mat[k,8] + mat[k,11] )
    ## printing to matrix for each generation
    toplot[k,1] <- mat[k,1]  ##generation number in column one
    toplot[k,2] <- m.SS1
    toplot[k,3] <- m.RS1
    toplot[k,4] <- m.RR1
    toplot[k,5] <- m.SS2
    toplot[k,6] <- m.RS2
    toplot[k,7] <- m.RR2
  }
  
  # plot locus 1
  lines ( toplot[,1], toplot[,2], pch=16, col="deeppink" )
  lines ( toplot[,1], toplot[,3], pch=16, col="darkorange" )
  lines ( toplot[,1], toplot[,4], pch=16, col="firebrick1" )
  # plot locus 2
  lines ( toplot[,1], toplot[,5], pch=16, col="cyan4" )
  lines ( toplot[,1], toplot[,6], pch=16, col="darkblue" )
  lines ( toplot[,1], toplot[,7], pch=16, col="darkgreen" )
  # add legend	
  legend( "topleft", legend=c("L1 - SS", "L1 - RS", "L1 - RR", 
                              "L2 - SS", "L2  - RS", "L2 - RR"), 
          col=c("deeppink", "darkorange", "firebrick1",
                "cyan4", "darkblue", "darkgreen"), pch=c(16,16,16,16,16,16), bty="n" )
}