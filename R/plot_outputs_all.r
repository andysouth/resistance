#' plot all outputs from one run of the model
#' 
#' Beth's code to save all plots, used to be at end of runModel2()

#' @param listOut list of results matrices
#' @param scen_num scenario number
#' @param savePlots whether to save plots as named pngs in working directory with scen_num at start
#' 
#' @return list of plot components
#' @export

plot_outputs_all <- function ( listOut, scen_num, savePlots ){
  
  # Plot of R and S allele frequencies over generations
  plotallele.freq( listOut$results[[scen_num]] )
  
  if (savePlots)
  {
    # saves plot into working directory
    dev.copy(png, (paste(scen_num,'freq-Rallele-bygender.png')))		
    dev.off()
  }
  
  # Plot of RR, RS and SS at each locus over generations
  genplot <- plothaplotype( listOut$genotype[[scen_num]] )
  
  if (savePlots)
  {
    # saves plot into working directory
    dev.copy(png,(paste(scen_num,'haplotype-frequencies.png')))
    dev.off()
  }
  
  # Plot of LD over time
  genplot <- plotlinkage( listOut$results[[scen_num]] )
  if (savePlots)
  {
    # saves plot into working directory
    dev.copy(png,(paste(scen_num,'LD.png')))		
    dev.off()
  }
  
}