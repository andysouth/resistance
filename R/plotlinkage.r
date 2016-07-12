#' plot Linkage Disequilibrium over generations
#' 
#' @param mat results matrix
#' @param plot_d
#' @param plot_dprime
#' @param main plot title
#' @param xlab xlab
#' @param ylab ylab
#' @param addLegend whether to add a legend inside plot
#' @param legpos legend position
#' 
#' @examples 
#' #single scenario
#' listOut <- resistSimple()
#' plotlinkage(listOut$results[[1]])

#' @return list of plot components
#' @export

plotlinkage <- function ( mat, 
                          plot_d = TRUE,
                          plot_dprime = TRUE,
                          main="",
                          xlab="Generation",
                          ylab="Linkage Disequilibrium",
                          addLegend=TRUE,
                          legpos="topright"){	
  
  par(pty="s") #makes plot square	
  
  ld_male <- mat[,4]	 # 4 = male
  ld_female <- mat[,7]   # 7 = female
  ld_prime_m <- mat[,10]  #10 = male prime
  ld_prime_f <- mat[,12]  #10 = female prime
  
  ld_concat <- NULL
  
  if (plot_d) ld_concat <- c(ld_male, ld_female)
    
  if (plot_dprime) ld_concat <- c(ld_concat, ld_prime_m)
  
  gens <- mat[,1]	
  
  plot( gens, ld_male, type="n", axes=T,						## Blank square plot
        xlim=c(1,(max(gens))), ylim=c(min(ld_concat),max(ld_concat)),
        xlab=xlab, ylab=ylab, main=main ) #"Linkage disequilibrium over generations.")	
  
  if (plot_d)
  {
    lines( gens, ld_female, col=adjustcolor("red", alpha.f = 0.5), lty=13 )
    lines( gens, ld_male, col=adjustcolor("blue", alpha.f = 0.5), lty=14 )    
  }

  if (plot_dprime)
  {  
    lines( gens, ld_prime_f, col=adjustcolor("red", alpha.f = 0.5), lty=15 )
    lines( gens, ld_prime_m, col=adjustcolor("blue", alpha.f = 0.5), lty=16 )
  }
  
  if (plot_d & plot_dprime){
    legend( legpos, legend=c("female","male","prime_f","prime_m"), 
            col=c("red", "blue", "red", "blue"), lty=c(13,14,15,16), bty="n", cex=0.7 )
    
  } else if (plot_d)
  {
    legend( legpos, legend=c("female","male"), 
            col=c("red", "blue"), lty=c(13,14), bty="n", cex=0.7 )    
  } else if (plot_dprime)
  {
    legend( legpos, legend=c("prime_f","prime_m"), 
            col=c("red","blue"), lty=c(15,16), bty="n", cex=0.7 )   
  }
  
}	