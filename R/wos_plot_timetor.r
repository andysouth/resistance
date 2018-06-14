#' plot time-to-resistance for 'windows of selection' following simulation run
#' 
#' @param dfsim data  on concentration and time-to-resistance output from simulation run from wos_sim()
#' @param x name of column of x data
#' @param xlab label of column of x data for plot
#' @param xtxt whether to number x axis 
#' @param y name of column of y data
#' @param ylab label of column of y data for plot 
#' @param title title for ggplot
# @param legendpos where to add legend
#' @param plot whether to plot
#' @param xreverse whether to reverse x axis for declining concentration
#' @param xlog whether to log x axis
#' 
#' @import ggplot2 tidyverse patchwork scales
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
                             xtxt = FALSE,
                             title = "simulation results",
                             #legendpos = 'bottom', #'right'
                             plot = TRUE,
                             xreverse = TRUE,
                             xlog = FALSE
) {

#to allow reverse logscale
#library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv, 
                    scales::log_breaks(base = base), 
                    domain = c(1e-100, Inf))
}  
  
#plot simulation results
gg <- dfsim %>%
  
  ggplot(aes_string(x=x, y=y))+
  geom_point() +
  #geom_line(lwd=2,alpha=0.6) +
  theme_minimal() +
  #theme(axis.text.x = element_blank()) +
  labs(title = title) +
  #labs(title = paste0("simulation, exposure=",exposure," starting resistance frequency=",startfreq)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='', y=ylab) 
#labs(x='Time or declining insecticide concentration')
if (xreverse & xlog) gg <- gg + scale_x_continuous(trans=reverselog_trans(10))
else if (xreverse) gg <- gg + scale_x_continuous(trans='reverse')
else if (xlog) gg <- gg + scale_x_continuous(trans='log')

if (!xtxt) gg <- gg + theme(axis.text.x = element_blank())

if(plot) plot(gg)

invisible(gg)

}