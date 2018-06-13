#' plot time-to-resistance for 'windows of selection' following simulation run
#' 
#' @param dfsim data  on concentration and time-to-resistance output from simulation run from wos_sim()
#' @param x name of column of x data
#' @param xlabel label of column of x data for plot
#' @param y name of column of y data
#' @param ylabel label of column of y data for plot 
#' @param title title for ggplot
# @param legendpos where to add legend
#' @param plot whether to plot
#' @param xreverse whether to reverse x axis for declining concentration
#' 
#' @import ggplot2 tidyverse patchwork
#' 
#' @examples 
#' dfsim <- wos_diagram(out=TRUE, rr_cost=0, plot=FALSE)
#' wos_plot_timetor(dfsim)
#' 
#' @return ggplot object
#' @export
#' 
wos_plot_timetor <- function(dfsim,
                             x='conc',
                             y='time_to_resistance0.5',
                             ylab='time to\nresistance',
                             xlab = NULL,
                             title = "simulation results",
                             #legendpos = 'bottom', #'right'
                             plot = TRUE,
                             xreverse = TRUE
) {

#plot simulation results
gg <- dfsim %>%
  
  ggplot(aes_string(x=x, y=y))+
  geom_point() +
  #geom_line(lwd=2,alpha=0.6) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(title = title) +
  #labs(title = paste0("simulation, exposure=",exposure," starting resistance frequency=",startfreq)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='', y=ylab) 
#labs(x='Time or declining insecticide concentration')
if (xreverse) gg <- gg + scale_x_continuous(trans='reverse')

if(plot) plot(gg)

invisible(gg)

}