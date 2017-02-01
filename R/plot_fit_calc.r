#' plot fitness calculation based on model inputs
#' 
#' accepts inputs

#' @param effectiveness
#' @param resistance_restoration
#' @param dominance
#' @param title optional title for the plot
# @param yblank whether to remove y axis title & labels
# @param size text size for RS labels, default 4
#' 
#' @examples 
#' plot_fit_calc(effectiveness=0.5, resistance_restoration=0.5, dominance=0.5)
#' 
#' 
#' @return 
#' @export

plot_fit_calc <- function ( effectiveness=0.5, resistance_restoration=0.5, dominance=0.5 ){
 
  
  #x axis SS=1, SR=2, RR=3
  #y axis fitness 0 to 1
  
  ss <- 1-effectiveness
  rr <- 1-effectiveness + (effectiveness*resistance_restoration)
  sr <- 1-effectiveness + (effectiveness*resistance_restoration*dominance)
  
  dfg <- data.frame(x1 = c('SS',  'SR',  'RR'), 
                    x2 = c('SS',  'SR',  'RR'),                    
                    y1 = c(1,  rr, ss), 
                    y2 = c(ss, sr, rr))

  gg <- ggplot(dfg, aes(x = x2, y = y2)) +    
        ylim(0,1) +
        #points
        geom_point(size=3, colour='blue') +
        #arrows
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),size=1, arrow = arrow(length = unit(3,"mm"))) +
        #dotted line at SS fitness, need to sort how to subset data
        #geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2))
        ylab('fitness') +
        #ggtitle(title) + 
        #scale_x_discrete(labels = c("1"="SS","2"="SR","3"="RR")) +
        #to set ordering of x axis
        scale_x_discrete(limits = c('SS',  'SR',  'RR')) +
        #add text annotations
        annotate("text", x = 'SS', y = 0.9, label = "effectiveness") +
          
        theme_bw() +
        theme(
              axis.title.x = element_blank(),
              #axis.line.x = element_blank(),
              #axis.ticks.x = element_blank(),
              #panel.grid.major.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
        )

  
  print(gg)
  
  
   
}