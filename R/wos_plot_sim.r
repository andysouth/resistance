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
#' @param ylog whether to log y axis
#' @param no_sr option for dealing with data with no SR heteroxzygotes plots dominance 0&1 with diff symbols
#' 
#' @import ggplot2 tidyverse patchwork scales
#' 
#' @examples 
#' dfsim <- wos_diagram(sim=TRUE, conc_n=10, rr_cost=0, plot=FALSE)
#' wos_plot_sim(dfsim)
#' 
#' @return ggplot object
#' @export
#' 
wos_plot_sim <- function(dfsim,
                             x='conc',
                             y='time_to_resistance0.5',
                             shape=NULL,
                             ylab='time to\nresistance',
                             xlab = NULL,
                             xtxt = FALSE,
                             title = "simulation results",
                             #legendpos = 'bottom', #'right'
                             plot = TRUE,
                             xreverse = TRUE,
                             xlog = FALSE,
                             ylog = FALSE,                         
                             no_sr = FALSE
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

# if (no_sr) symbol <- 'dom_resist'
# else symbol <- NULL
  

#plot simulation results

#diff symbols for points dominance 0&1 if no SR
#if (!no_sr) 
if (is.null(shape))   
{
  gg <- dfsim %>% ggplot(aes_string(x=x, y=y))
} else 
{
  #dfsim$dominance <- factor(dfsim$dom_resist)
  
  #converting shape column to a factor
  #rlang::sym solved tidyeval faffing 
  #sym is a handy function to convert a string to a quo
  #!! unquotes a variable name
  #tst <- dfsim %>% mutate(shapefac = factor(!!rlang::sym(shape)))
  
  #this black magic works to keep the factor column name the same
  #from here https://stackoverflow.com/a/45572292/1718356
  #tst <- dfsim2 %>% 
  #       mutate((!!sym(shape)) := factor(!!rlang::sym(shape)))
  
  gg <- dfsim %>% 
    
    mutate((!!sym(shape)) := factor(!!rlang::sym(shape))) %>% 
    ggplot(aes_string(x=x, y=y, shape=shape))
    
    # mutate(shapefac = factor(!!rlang::sym(shape))) %>% 
    # ggplot(aes_string(x=x, y=y, shape='shapefac')) 
  
    #guides(fill = guide_legend(title = 'test')) #sym(shape)))
    #scale_shape_manual(name=shape)
  
    #now apply scale_shape_manual outside of function to allow flexibility for diff no. of cats
    #scale_shape_manual(values=c(6,1,3,2)) #symbol 2 uptriangle, 6 downtriangle
}

gg <- gg +
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

#log selective advantage
if (ylog) gg <- gg + scale_y_continuous(trans='log')

if (!xtxt) gg <- gg + theme(axis.text.x = element_blank())

#if (!is.null(shape)) gg <- gg + guides(fill = guide_legend(title = shape))

if(plot) plot(gg)

invisible(gg)

}