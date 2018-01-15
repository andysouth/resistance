#' plot fitness calculation based on model inputs
#' 
#' accepts inputs

#' @param effectiveness insecticide effectiveness 0-1
#' @param resistance_restoration resistance restoration 0-1
#' @param dominance_restoration dominance of resistance restoration 0-1
#' @param dominance_cost dominance of resistance cost 0-1
#' @param cost cost of resistance
#' @param simple default FALSE whether to create a simpler plot e.g. for public health journal
#' @param title optional title for the plot
# @param yblank whether to remove y axis title & labels
# @param size text size for RS labels, default 4
#' 
#' #grid needed for title
#' @import ggplot2 grid
#' @importFrom gridExtra grid.arrange 
#' 
#' @examples 
#' plot_fit_calc(effectiveness=0.5, resistance_restoration=0.5, dominance=0.5)
#' 
#' 
#' @return gtable plot object
#' @export

#plot_fit_calc <- function ( effectiveness=0.8, resistance_restoration=0.6, dominance_restoration=0.6, dominance_cost=0.5, cost=0.3 ){
plot_fit_calc <- function ( effectiveness=0.75, 
                            resistance_restoration=0.75, 
                            dominance_restoration=0.4, 
                            dominance_cost=0.6, 
                            cost=0.2,
                            simple = FALSE,
                            title = "Fitness calculation for each genotype in each generation for one insecticide"){
    
  
  #library(gridExtra)
  #library(grid)

  
  #x axis SS=1, SR=2, RR=3
  #y axis fitness 0 to 1
  
  # In presence of insecticide
  ss <- 1-effectiveness
  rr <- 1-effectiveness + (effectiveness*resistance_restoration)
  sr <- 1-effectiveness + (effectiveness*resistance_restoration*dominance_restoration)
  
  dfg <- data.frame(x1 = c('SS',  'SR',  'RR'), 
                    x2 = c('SS',  'SR',  'RR'),                    
                    y1 = c(1,  ss, ss), 
                    y2 = c(ss, sr, rr))

  #set up different labels for dominance in simple & other plot
  if (simple) {dom_rest_lab <- dom_cost_lab <- " dominance"} else {
               dom_rest_lab <- " dominance of\n  resistance"
               dom_cost_lab <- " dominance of\n  cost" }
    
  
  gg1 <- ggplot(dfg, aes(x = x2, y = y2)) +    
        ylim(0,1) +
        #points
        geom_point(size=3, colour='red4') +
        #arrows
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size=0.9, arrow = arrow(length = unit(3,"mm")), colour="grey40") +
        #dotted horizontal lines showing calculations
        annotate("segment", x='SS', xend='RR', y=ss, yend=ss, linetype="dotted", colour = "red4") +
        annotate("segment", x='SR', xend='RR', y=rr, yend=rr, linetype="dotted", colour = "red4") +
        #dotted vertical for dominance
        annotate("segment", x='SR', xend='SR', y=sr, yend=rr, linetype="dotted", colour = "red4") +    
        ylab('fitness') +
        xlab('genotype') +
        ggtitle("exposed to insecticide") + #colour set in theme below
        #to set ordering of x axis
        scale_x_discrete(limits = c('SS',  'SR',  'RR')) +
        #text annotations
        # annotate("text", x = 'SS', y = ss+(1-ss)/2, hjust=0, label = " effectiveness") +
        # annotate("text", x = 'SR', y = sr+(rr-sr)/2, hjust=0, label = " dominance") + 
        # annotate("text", x = 'RR', y = ss+(rr-ss)/2, hjust=0, label = " resistance\n  restoration") + 
        annotate("label", x = 'SS', y = ss+(1-ss)/2, size=3, label = " effectiveness") +
        annotate("label", x = 'SR', y = ss+(sr-ss)/2, size=3, label = dom_rest_lab) + 
        annotate("label", x = 'RR', y = ss+(rr-ss)/2, size=3, label = " resistance\n  restoration") + 
    
        theme_bw() +
        theme(
              axis.title.y = element_text(size = rel(1.5)),
              axis.text.x = element_text(size = rel(1.5)),
              #axis.title.x = element_blank(),
              #axis.line.x = element_blank(),
              #axis.ticks.x = element_blank(),
              plot.title = element_text(colour = "red4"), #size = 40, face = "bold"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
        )

  if (!simple){
    gg1 <- gg1 +
      scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), limits=c(0,1)) +
      # add 0,1 labels for dominance and resistance restoration
      annotate("text", x = 'SS', y = 1, hjust=0, label = " 0", colour="grey40") +
      annotate("text", x = 'SS', y = 0, hjust=0, label = " 1", colour="grey40") +   
      annotate("text", x = 'SR', y = ss, hjust=0, label = " 0", colour="grey40") +
      annotate("text", x = 'SR', y = rr, hjust=0, label = " 1", colour="grey40") +
      annotate("text", x = 'RR', y = ss, hjust=0, label = " 0", colour="grey40") +
      annotate("text", x = 'RR', y = 1, hjust=0, label = " 1", colour="grey40") 
    
  } else if (simple){
    
    gg1 <- gg1 +    
      scale_y_continuous(breaks=c(0,0.5,1), limits=c(0,1), labels=c('low','','high')) +
      scale_x_discrete(limits = c('SS',  'SR',  'RR'), labels = c('susceptible',  'SR',  'resistant'))
  }
  
  
  
  # insecticide absent
  ss <- 1
  rr <- 1-cost
  sr <- 1-cost + (1-dominance_cost)*cost  

  dfg <- data.frame(x1 = c('SS',  'SR',  'RR'), 
                    x2 = c('SS',  'SR',  'RR'),                    
                    y1 = c(1,  ss, ss), 
                    y2 = c(ss, sr, rr))
    
  gg2 <- ggplot(dfg, aes(x = x2, y = y2)) +    
    ylim(0,1) +
    #points
    geom_point(size=3, colour='navy') +
    #arrows
    #geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size=1, arrow = arrow(length = unit(3,"mm"))) +
    #because I don't want an arrow for SS, easier to do with annotate
    annotate("segment", x='SR', xend='SR', y=ss, yend=sr, size=0.9, arrow=arrow(length = unit(3,"mm")), colour="grey40") +
    annotate("segment", x='RR', xend='RR', y=ss, yend=rr, size=0.9, arrow=arrow(length = unit(3,"mm")), colour="grey40") +
    #dotted horizontal lines showing calculations
    annotate("segment", x='SS', xend='RR', y=ss, yend=ss, linetype="dotted", colour="navy") +
    annotate("segment", x='SR', xend='RR', y=rr, yend=rr, linetype="dotted", colour="navy") +
    #dotted vertical for dominance
    annotate("segment", x='SR', xend='SR', y=sr, yend=rr, linetype="dotted", colour="navy") +    
    #ylab('fitness') +
    xlab('genotype') +
    ggtitle("not exposed to insecticide") + 
    #to set ordering of x axis
    scale_x_discrete(limits = c('SS',  'SR',  'RR')) +
    #text annotations "label" creates box "text"doesn't
    #annotate("text", x = 'SS', y = ss+(1-ss)/2, hjust=0, label = " effectiveness") +
    annotate("label", x = 'SR', y = sr+(ss-sr)/2, size=3, label = dom_cost_lab) + 
    annotate("label", x = 'RR', y = ss-(ss-rr)/2, size=3, label = " resistance\n  cost") + 
    
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(1.5)),
      #axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      #axis.line.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(colour = "navy"), #size = 40, face = "bold"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )  

  if (!simple){
    gg2 <- gg2 +
      scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), limits=c(0,1)) +
      # add 0,1 labels for dominance and cost
      annotate("text", x = 'SR', y = ss, hjust=0, label = " 0", colour="grey40") +
      annotate("text", x = 'SR', y = rr, hjust=0, label = " 1", colour="grey40") +
      annotate("text", x = 'RR', y = 1, hjust=0, label = " 0", colour="grey40") +
      annotate("text", x = 'RR', y = 0, hjust=0, label = " 1", colour="grey40") 
    
  } else if (simple){
    
    gg2 <- gg2 +    
      #scale_y_continuous(breaks=c(0,0.5,1), limits=c(0,1), labels=c('low','','high')) +
      scale_x_discrete(limits = c('SS',  'SR',  'RR'), labels = c('susceptible',  'SR',  'resistant'))
  }    
  
  if (is.null(title))
  {
    gridExtra::grid.arrange(gg1, gg2, nrow=1, widths=c(1,0.85))
  } else
  {
    #fontface = "plain", "bold", "italic"
    gridExtra::grid.arrange(gg1, gg2, nrow=1, widths=c(1,0.85), top=textGrob(title, gp=gpar(fontsize=15, fontface='plain'))) #col=
  }    
  
   
}