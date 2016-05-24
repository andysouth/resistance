#' plot allele frequencies over time for single or multiple scenarios
#' 
#' plot of resistance allele frequencies over time
#' Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
#' and same in female at locus 1 (red) and locus 2 (orange)
#' For multiple scenarios multipl;e lines are plotted.
#' todo : for multiple scenarios currently it accepts listOut, and gets results from listOut$results, I might instead want it to accept listOut$results.

#' @param mat results matrix or list of multiple matrices
#' @param sex which sexes to plot options : "mf","m","f","mf_mean"
#' @param locus which loci to plot options : 1","2" or "12" for both
#' @param add_legend whether to add legend
#' @examples 
#' #single scenario
#' listOut <- resistSimple()
#' plotallele.freq.andy(listOut$results[[1]])
#' #mutliple scenarios
#' listOut <- resistanceMaster()
#' plotallele.freq.andy(listOut)
#' @return list of plot components
#' @export

plotallele.freq.andy <- function ( mat,
                                   sex = c("mf","m","f","mf_mean"),
                                   locus = c("12","1","2"),
                                   add_legend = TRUE){
  # check inputs
  sex = match.arg( sex )
  locus = match.arg( locus )
  
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
    lapply(mat$results, function(x) plotalleleLinesOneScenario(x, sex=sex, locus=locus) )
  #if a single scenario just plot the lines for that
  else
    plotalleleLinesOneScenario(mat, sex=sex, locus=locus)
  
  if (add_legend)
  {
    legend = NULL
    col = NULL
   
    if (sex=="m" | sex=="mf" )
    {
      if (locus == "1" || locus == "12" )
      {
        legend=c(legend, "locus1 male") 
        col <- c(col,"darkblue")
      }
      if (locus == "2" || locus == "12" )
      {
        legend=c(legend, "locus2 male")
        col <- c(col,"green")
      }
    }
    
    # F
    if (sex=="f" | sex=="mf" )
    {
      if (locus == "1" || locus == "12" )
      {
        legend=c(legend, "locus1 female") 
        col <- c(col, "red")
      }
      if (locus == "2" || locus == "12" )
      {
        legend=c(legend, "locus2 female")
        col <- c(col, "orange")
      }
    } 
        
    if (sex=="mf_mean" )
    {
      if (locus == "1" || locus == "12" )
      {
        legend=c(legend, "locus1") 
        col <- c(col,"darkblue")
      }
      if (locus == "2" || locus == "12" )
      {
        legend=c(legend, "locus2")
        col <- c(col,"green")
      }
    }    
    

    legend( "bottomright", legend=legend, col=col, pch=16, bty="n" )    
    
    # legend( "bottomright", legend=c("locus1 male", "locus1 female", "locus2 male", "locus2 female"), 
    #         col=c("darkblue", "red", "green", "orange"), pch=16, bty="n" )
    
  } #end add_legend 
}

#' helper function to plot allele frequency lines for one scenario
#' 
#' @param mat results matrix
#' @param sex which sexes to plot options : "mf","m","f","mf_mean"
#' @param locus which loci to plot options : 1","2" or "12" for both
#' 
#' @return nothing
plotalleleLinesOneScenario <- function ( mat,
                                         sex = c("mf","m","f","mf_mean"),
                                         locus = c("12","1","2")){
  # check inputs
  sex = match.arg( sex )
  locus = match.arg( locus )
  
  #i should write an access function for getting mf locus 1 & 2 out of the results list
  
  #line types(lty) to show all if overlapping, odd digits=line, even=gap
  
  # M
  if (sex=="m" | sex=="mf" )
  {
    if (locus == "1" || locus == "12" )
    {
      lines( mat[,1], mat[,2], col="darkblue", lwd=2, lty=13 )  #m locus1      
    }
    if (locus == "2" || locus == "12" )
    {
      lines( mat[,1], mat[,3], col="green", lwd=2, lty=14 )  #m locus2 
    }
  }

  # F
  if (sex=="f" | sex=="mf" )
  {
    if (locus == "1" || locus == "12" )
    {
      lines( mat[,1], mat[,5], col="red",lwd=2, lty=15 )  #f locus1    
    }
    if (locus == "2" || locus == "12" )
    {
      lines( mat[,1], mat[,6], col="orange", lwd=2, lty=16 )  #f locus2
    }
  }  
  
  if (sex=="mf_mean" )
  {
    if (locus == "1" || locus == "12" )
    {
      lines( mat[,1], 0.5*(mat[,2]+mat[,5]), col="darkblue", lwd=2, lty=13 )  #m locus1  
    }
    if (locus == "2" || locus == "12" )
    {
      lines( mat[,1], 0.5*(mat[,3]+mat[,6]), col="green", lwd=2, lty=14 )  #m locus2 
    }
  }   

  
}

