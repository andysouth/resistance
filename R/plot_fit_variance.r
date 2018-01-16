#' plot variance in fitness over generations
#' 
#' I could try to get this to accept multiple scenarios & put them on the same graph.
#' 
#' @import ggplot2
# if in here must be in imports too taking out because causing shinyapps problem  
#' @importFrom cowplot plot_grid
#' @importFrom tidyr gather
#' @examples 
#' input <- setInputOneScenario()
#' listOut <- runModel2(input)
#' plot_fit_variance(listOut)
#' 
#' @param listOut list of results matrices, including results including matrix of fitness and variance in fitness over generaions
#' @param plot_resist whether to add a plot of resistance frequency
#' 
#' @return the ggplot object of the plot
#' @export

plot_fit_variance <- function ( listOut,
                                plot_resist = TRUE){
  

  #quick & dirty way using base graphics
  #just for females
  #plot(listOut$fit_time_genotype[,'f','variance'])
  
  # make dataframe of variances for ggplot
  
  # just for females
  # df_var <- data.frame(generation=as.numeric(dimnames(listOut$fit_time_genotype)[[1]]), 
  #                      fitness_variance=listOut$fit_time_genotype[,'f','variance'])
  
  #TODO BEWARE it's a bit weird that fit_time_genotype only seems to be saved
  #for one input scenario ?
  
  # to cope with males & females
  df_tmp <- data.frame(generation=as.numeric(dimnames(listOut$fit_time_genotype)[[1]]), 
                       f=listOut$fit_time_genotype[,'f','variance'],
                       m=listOut$fit_time_genotype[,'m','variance'])
  
  #although sex not likely to be diff anyway
  df_var <- gather(df_tmp, key=sex, value=fitness_variance, gather_cols=f,m)
  
  # jittered m&f
  # gg <- ggplot(df_var, aes(x=generation, y=fitness_variance, colour=sex)) +
  #   geom_jitter(alpha=0.5, shape=21) 
  
  #list to put plots in for cowplot
  plotlist <- list(2)
  
  #removing the colour, & putting m&f on top of each other
  plotlist[[1]] <- ggplot(df_var, aes(x=generation, y=fitness_variance, colour=sex)) +
    geom_point(alpha=0.5, shape=21)
  
  #would be good to have resistance freq (sum of R) side-by-side with this
  #maybe could fact in ggplot ?
  
  # can I optionally add in resistance frequency and use facet to show that 
  # as a plot below ?
  # BEWARE this just works for first scenario if >1
  df_tmp2 <- data.frame(generation=as.numeric(dimnames(listOut$fit_time_genotype)[[1]]), 
                        f.R1=listOut$results[[1]][,"f.R1"],
                        f.R2=listOut$results[[1]][,"f.R2"])  
  
  #resistance frequency BEWARE initially just for F
  df_res <- gather(df_tmp2, key=locus, value=resistance, gather_cols=f.R1,f.R2)  
  
  plotlist[[2]] <- ggplot(df_res, aes(x=generation, y=resistance, colour=locus)) +
    geom_point(alpha=0.5, shape=21)  
  
  #cowplot to put the figures side by side
  if (plot_resist)
  {
    # to remove x axis label from top plot
    plotlist[[1]] <- plotlist[[1]] + xlab("")
    #warning("cowplot::plot_grid disabled to fix shinyapps problem")
    cowplot::plot_grid( plotlist[[1]],plotlist[[2]],ncol=1, rel_heights=c(1,1), labels='AUTO')    
  } else
  {
    print(plotlist[[1]])
  }

  
  #print(gg)
  
}