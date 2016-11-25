#' plot variance in fitness over generations
#' 
#' I could try to get this to accept multiple scenarios & put them on the same graph.
#' 
#' @param listOut list of results matrices, including results including matrix of fitness and variance in fitness over generaions
#' 
#' @return the ggplot object of the plot
#' @export

plot_fit_variance <- function ( listOut ){
  

  #quick & dirty way using base graphics
  #just for females
  #plot(listOut$fit_time_genotype[,'f','variance'])
  
  # make dataframe of variances for ggplot
  
  # just for females
  # df_var <- data.frame(generation=as.numeric(dimnames(listOut$fit_time_genotype)[[1]]), 
  #                      fitness_variance=listOut$fit_time_genotype[,'f','variance'])
  
  # to cope with males & females
  df_tmp <- data.frame(generation=as.numeric(dimnames(listOut$fit_time_genotype)[[1]]), 
                       f=listOut$fit_time_genotype[,'f','variance'],
                       m=listOut$fit_time_genotype[,'m','variance'])
  
  #although sex not likely to be diff anyway
  df_var <- gather(df_tmp, key=sex, value=fitness_variance, gather_cols=f,m)
  
  # jittered m&f
  # gg <- ggplot(df_var, aes(x=generation, y=fitness_variance, colour=sex)) +
  #   geom_jitter(alpha=0.5, shape=21) 
  
  #removing the colour, & putting m&f on top of each other
  gg <- ggplot(df_var, aes(x=generation, y=fitness_variance, colour=sex)) +
    geom_point(alpha=0.5, shape=21)
  
  #would be good to have resistance freq (sum of R) side-by-side with this
  #maybe could fact in ggplot ?
  
  
  print(gg)
  
}