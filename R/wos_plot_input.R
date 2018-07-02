#' plot one simulation input for 'windows of selection' following simulation run
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
#' dfsim <- wos_diagram(sim=TRUE, rr_cost=0, plot=FALSE)
#' wos_plot_input(dfsim, y='dom_resist', ylab='dominance')
#' 
#' @return ggplot object
#' @export

wos_plot_input <- function( dfsim,
                         x='conc',  
                         y = "dom_resist",
                         xlab = NULL,
                         ylab = NULL,                         
                         title = "selected simulation inputs",
                         #legendpos = 'bottom', #'right'
                         plot = TRUE,
                         xreverse = TRUE
) {


# plot dominance
gg <- dfsim %>%
  ggplot(aes_string(x=x, y=y))+
  #geom_point(shape=1, size=0.6) + #open circle
  geom_line(linetype=4) +
  theme_minimal() +
  #scale_x_continuous(sec.axis = dup_axis()) +

  theme(axis.text.x = element_blank(),
        #axis.title.y = element_text(size = rel(0.7)),
        #2ry x axis for ylab
        axis.title.x.top = element_text(hjust=0)) +
        #axis.title.y = element_text(size = rel(0.7),angle=0, hjust=0)) +
  #annotate("text", x = Inf, y = Inf, label = ylab) + #, angle=90) +
  ggtitle(title) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,1)) +
  labs(y='',x=xlab)

#2ry x axis is used to squeeze in ylabel
if (xreverse) gg <- gg + scale_x_continuous(trans='reverse',
                                            sec.axis = dup_axis(name=ylab)) 
else gg <- gg + scale_x_continuous(sec.axis = dup_axis(name=ylab))

if(plot) plot(gg)

invisible(gg)

}