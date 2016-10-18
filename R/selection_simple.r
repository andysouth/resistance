#' simple simulation of selection of one allele
#' 
#' used to help produce fig1 in paper1

#' @param freq starting freq of resistance
#' @param selection_co selection coefficient
#' @param dominance dominance
#' @param num_gens number generations
#' @param plot whether to plot

#' @examples 
#' selection_simple( freq=0.01, selection_co = 1, dominance = 0.5, num_gens = 10)
#' 
#' @return dataframe of frequency results
#' @export

selection_simple <- function( freq=0.01, 
                              selection_co = 1, 
                              dominance = 1, 
                              num_gens = 10,
                              plot = TRUE)
{
  #initialise storage for results to 0 and include the inputs to help with plotting later
  df1 <- data.frame(generation=1:num_gens, freq=0, SS=0, SR=0, RR=0, totfit=0,
                    selection_co=selection_co, dominance=dominance)
  
  df1$freq[1] <- freq
  
  for( gen in 1:num_gens )
  {
    df1$SS[gen] <- (1-df1$freq[gen])^2
    df1$SR[gen] <- 2 * df1$freq[gen] * (1-df1$freq[gen]) * (1+dominance*selection_co)
    #2*H15*(1-H15)*(1+$A$10*$A$7)
    df1$RR[gen] <- (df1$freq[gen])^2 * (1+selection_co)
    df1$totfit[gen] <- df1$SS[gen] + df1$SR[gen] + df1$RR[gen]
    
    if (gen<num_gens) 
    {
      df1$freq[gen+1] <- (0.5*df1$SR[gen]+df1$RR[gen])/df1$totfit[gen]
    }
  }
  
  
  if (plot)
  {
    print( ggplot(df1, aes(x=generation, y=freq, colour=factor(dominance))) + 
             theme_bw() +  
             labs(colour = "dominance") +
             ylab("Resistance Allele Frequency") +   
             #coord_trans(y = "log10") +    
             geom_line()
    )
  }
  
  return(df1)
}