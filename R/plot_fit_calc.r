#' plot fitness calculation based on model inputs
#' 
#' accepts inputs

#' @param effectiveness insecticide effectiveness 0-1
#' @param resistance_restoration resistance restoration 0-1
#' @param dominance_restoration dominance of resistance restoration 0-1
#' @param dominance_cost dominance of resistance cost 0-1
#' @param cost cost of resistance
#' @param title optional title for the plot
# @param yblank whether to remove y axis title & labels
# @param size text size for RS labels, default 4
#' 
#' @import ggplot2 gridExtra
#' 
#' @examples 
#' plot_fit_calc(effectiveness=0.5, resistance_restoration=0.5, dominance=0.5)
#' 
#' 
#' @return gtable plot object
#' @export

plot_fit_calc <- function ( effectiveness=0.8, resistance_restoration=0.5, dominance_restoration=0.5, dominance_cost=0.5, cost=0.3 ){
 
  
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
                    y1 = c(1,  rr, ss), 
                    y2 = c(ss, sr, rr))

  gg1 <- ggplot(dfg, aes(x = x2, y = y2)) +    
        ylim(0,1) +
        #points
        geom_point(size=3, colour='blue') +
        #arrows
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size=0.9, arrow = arrow(length = unit(3,"mm"))) +
        #dotted horizontal lines showing calculations
        annotate("segment", x='SS', xend='RR', y=ss, yend=ss, linetype="dotted", colour = "blue") +
        annotate("segment", x='SR', xend='RR', y=rr, yend=rr, linetype="dotted", colour = "blue") +
        ylab('fitness') +
        xlab('genotype') +
        #theme(axis.title.y = element_text(size = rel(3))) +
        ggtitle("exposed to insecticide") +
        #to set ordering of x axis
        scale_x_discrete(limits = c('SS',  'SR',  'RR')) +
        #text annotations
        # annotate("text", x = 'SS', y = ss+(1-ss)/2, hjust=0, label = " effectiveness") +
        # annotate("text", x = 'SR', y = sr+(rr-sr)/2, hjust=0, label = " dominance") + 
        # annotate("text", x = 'RR', y = ss+(rr-ss)/2, hjust=0, label = " resistance\n  restoration") + 
        annotate("label", x = 'SS', y = ss+(1-ss)/2, size=3, label = " effectiveness") +
        annotate("label", x = 'SR', y = sr+(rr-sr)/2, size=3, label = " 1-dominance of\n  restoration") + 
        annotate("label", x = 'RR', y = ss+(rr-ss)/2, size=3, label = " resistance\n  restoration") + 
    
        theme_bw() +
        theme(
              axis.title.y = element_text(size = rel(1.5)),
              axis.text.x = element_text(size = rel(1.5)),
              #axis.title.x = element_blank(),
              #axis.line.x = element_blank(),
              #axis.ticks.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
        )

  # insecticide absent
  ss <- 1
  rr <- 1-cost
  sr <- 1-cost + (1-dominance_cost)*cost  

  dfg <- data.frame(x1 = c('SS',  'SR',  'RR'), 
                    x2 = c('SS',  'SR',  'RR'),                    
                    y1 = c(1,  rr, ss), 
                    y2 = c(ss, sr, rr))
    
  gg2 <- ggplot(dfg, aes(x = x2, y = y2)) +    
    ylim(0,1) +
    #points
    geom_point(size=3, colour='blue') +
    #arrows
    #geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size=1, arrow = arrow(length = unit(3,"mm"))) +
    #because I don't want an arrow for SS, easier to do with annotate
    annotate("segment", x='SR', xend='SR', y=rr, yend=sr, size=0.9, arrow=arrow(length = unit(3,"mm"))) +
    annotate("segment", x='RR', xend='RR', y=ss, yend=rr, size=0.9, arrow=arrow(length = unit(3,"mm"))) +
    #dotted horizontal lines showing calculations
    annotate("segment", x='SS', xend='RR', y=ss, yend=ss, linetype="dotted", colour = "blue") +
    annotate("segment", x='SR', xend='RR', y=rr, yend=rr, linetype="dotted", colour = "blue") +
    #ylab('fitness') +
    xlab('genotype') +
    ggtitle("not exposed to insecticide") + 
    #to set ordering of x axis
    scale_x_discrete(limits = c('SS',  'SR',  'RR')) +
    #text annotations "label" creates box "text"doesn't
    #annotate("text", x = 'SS', y = ss+(1-ss)/2, hjust=0, label = " effectiveness") +
    annotate("label", x = 'SR', y = sr+(rr-sr)/2, size=3, label = " 1-dominance\n  of cost") + 
    annotate("label", x = 'RR', y = ss+(rr-ss)/2, size=3, label = " resistance\n  cost") + 
    
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(1.5)),
      #axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      #axis.line.x = element_blank(),
      axis.ticks.y = element_blank(),
      #panel.grid.major.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )  
  
  
  grid.arrange(gg1, gg2, nrow=1, widths=c(1,0.85))  
  #print(gg)
  
  
   
}