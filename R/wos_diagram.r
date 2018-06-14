#' plot idealised diagram to illustrate 'windows of selection'
#' 
#' mortality over time or declining concentration for RR,SR,SS
#' 
#' @param conc_n number of x intervals default 50, will influence smoothness of lines, converted to 0-1
#' @param conc_rr_mort0 concentration killing none of rr
#' @param conc_ss_mort0 concentration killing none of ss
#' @param mort_slope = 3 slope of mortality curves
#' @param sr whether to include heterozygotes
#' @param dom_resistance dominance of resistance 0-1 determines position of sr between rr & ss but it's not really dominance
#' @param rr_cost cost of resistance to rr (simply added to rr mort)
#' @param dom_cost dominance of cost
#' @param exposure proportion of popn exposed to insecticide only used in the simulations
#' @param max_gen maximum generations to use in the simulations
#' @param startfreq starting frequency to use in the simulations
#' @param title title for ggplot
#' @param addwindow whether to add dotted lines for opening and shutting of window
#' @param addshading whether to shade window
#' @param addlabels whether to add labels
#' @param legendpos where to add legend
#' @param plot whether to plot
#' @param out whether to calculate and output resistance model inputs
#' @param adv whether to calculate and output selective advantage
#' @param logadv whether to use log scale in selective advantage plots
#' @param xreverse whether to reverse x axis for declining concentration
#' 
#' @import ggplot2 tidyverse patchwork
#' 
#' @examples 
#' wos_diagram(mort_slope=3, dom_resist=0.9)
#' dfsim <- wos_diagram(out=TRUE, rr_cost=0)
#' 
#' @return ggplot object
#' @export


wos_diagram <- function( conc_n = 50,
                      #conc_rr_mort1 <- 0.7 # concentration killing all of rr
                      #conc_ss_mort1 <- 0.3 # concentration killing all of ss
                      #or try mort 0 because fits better in y=mx+c
                      conc_rr_mort0 = 0.6, # concentration killing none of rr
                      conc_ss_mort0 = 0.1, # concentration killing none of ss
                      mort_slope = 3, #0.5 # slope of mortality curves
                      sr = TRUE, # whether to include heterozygotes
                      dom_resistance = 0.5, #1 # dominance of resistance 0-1, it's not really dominance
                      rr_cost = 0, #.1, #cost of resistance to rr (simply added to rr mort)
                      dom_cost = 0, #dominance of cost
                      exposure = 0.5, #only used in the simulation
                      max_gen = 1000, #used in simulations
                      startfreq = 0.001, 
                      title = "Window of selection",
                      addwindow = TRUE,
                      addshading = TRUE,
                      addlabels = TRUE,
                      legendpos = 'bottom', #'right'
                      plot = TRUE,
                      out = FALSE,
                      adv = FALSE,
                      logadv = TRUE,
                      xreverse = TRUE
) {
  
  if (rr_cost > 0 & out) warning('running simulation with rr_cost>0 can cause it to crash') 
  
  concs <- seq(0,1,length=conc_n)
  #trying (and failing) to base directly on helps2017
  #mort_rr <- -log(1+ 10^conc_rr_mort1 * concs^mort_slope)
  #mort_ss <- -log(1+ 10^conc_ss_mort1 * concs^mort_slope)
  #try simpler version based on my own logic to find points based on where they cross y axis
  mort_rr <- mort_slope * (concs-conc_rr_mort0) #+0 #y=m(x-x1)+y1 slope and a point where y=0
  mort_ss <- mort_slope * (concs-conc_ss_mort0)
  #mort_ss <- mort_slope * concs + conc_ss_mort0 #y=mx+c
  
  #could use ifelse to constrain at 0 & 1 (but must be better way)
  mort_rr <- ifelse(mort_rr>1,1,ifelse(mort_rr<0,0,mort_rr))
  mort_ss <- ifelse(mort_ss>1,1,ifelse(mort_ss<0,0,mort_ss))
  
  #add cost of resistance : the (1-mort_rr) is a bit of a hack to stop it going above 1
  mort_rr <- mort_rr + rr_cost*(1-mort_rr)
  
  #sr
  #set where it crosses mort0 to be between ss & rr according to dominance
  #(although the dominance param isn't really dominance)
  conc_sr_mort0 <- conc_ss_mort0 + dom_resistance*(conc_rr_mort0-conc_ss_mort0)
  mort_sr <- mort_slope * (concs-conc_sr_mort0) #+0 #y=m(x-x1)+y1 slope and a point where y=0
  mort_sr <- ifelse(mort_sr>1,1,ifelse(mort_sr<0,0,mort_sr))
  #this creates a different pattern from the science paper when dom=0.5
  #i.e. the SR crosses mort1 at same position as RR and mort0 as SS
  #mort_sr <- (dom_resistance*mort_rr)+((1-dom_resistance)*mort_ss)
  
  #remember selection is greatest when heterozygotes survive
  
  #to calculate where window opens and closes for labels
  #min conc where ssmort1 & rrmort1 == 1
  #win_opens <- min(which(mort_rr==1)) #beware vulnerable if either not 1
  win_opens <- concs[max(which(mort_ss > mort_rr))]
  #min conc where mort_ss > mort_rr
  win_closes <- concs[min(which(mort_ss > mort_rr))]
  
  sr_win_opens <- concs[max(which(mort_ss > mort_sr))]
  sr_win_closes <- concs[min(which(mort_ss > mort_sr))]
  
  dfdiag <- dplyr::data_frame( conc = c(concs,concs,concs),
                        genotype = c(rep('rr',conc_n),
                                     rep('ss',conc_n),
                                     rep('sr',conc_n)),
                        #1st line for resistant, 2nd for susceptible
                        mortality = c(mort_rr,
                                      mort_ss,
                                      mort_sr))
  
  #to allow window polygon plotting
  win_indices <- which(mort_ss > mort_rr)
  dfwindow <- data_frame( conc = concs[win_indices],
                          rr = mort_rr[win_indices],
                          ss = mort_ss[win_indices])
  #experimenting with window for sr
  win_indices <- which(mort_ss > mort_sr)
  dfwin_sr <- data_frame( conc = concs[win_indices],
                          sr = mort_sr[win_indices],
                          ss = mort_ss[win_indices])
  
  #if not plotting sr just filter it out here
  if (!sr) dfdiag <- filter(dfdiag, genotype!='sr')
  
  gg <- dfdiag %>%
    
    ggplot(aes(x=conc, y=mortality, col=genotype, linetype=genotype))+
    geom_line(lwd=2,alpha=0.6) +
    theme_minimal() +
    labs(title = title) +
    labs(x='Time or declining insecticide concentration') +
    #scale_x_continuous(trans='log')
    
    #make plot cleaner
    theme( legend.position = legendpos,
           #panel.grid = element_blank(),
           axis.text.x = element_blank(),
           axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
           #plot.title = element_text(hjust = 0.5),
           axis.line.y = element_line())
           #panel.background = element_rect(colour = 'grey60', fill=NA, size=0.5))
  
  #shading window of selection, i'm not sure whether helpful to shade between
  #the curves or vertically
  #experimenting with chnaging dfwindow above just to show sr window
  if (addshading) gg <- gg + geom_ribbon(data=dfwindow,aes(x=conc, ymin=rr, ymax=ss),inherit.aes=FALSE, fill = "orange", alpha = .1, show.legend = FALSE)
  #polygon way didn't work
  #geom_polygon(data=filter(dfdiag,genotype!='sr' & conc<=win_opens & conc>=win_closes),aes(x=conc, y=mortality),inherit.aes=FALSE, fill = "orange", alpha = .1, show.legend = FALSE) +
  
  if (addshading & sr ) gg <- gg + geom_ribbon(data=dfwin_sr,aes(x=conc, ymin=sr, ymax=ss),inherit.aes=FALSE, fill = "red", alpha = .1, show.legend = FALSE)
  
  #vertical lines for window opening and closing
  if (addwindow) gg <- gg + geom_vline( xintercept = c(win_opens,win_closes), linetype='dotted', lwd=2)
  
  
  if (addlabels)
  {
    gg <- gg +
      annotate("text", x = win_opens+0.05, y = 0.75, label = "window opens", angle=90) +
      annotate("text", x = win_closes+0.05, y = 0.75, label = "window closes", angle=90) +
      annotate("text", x = 0.5, y = 0.55, label = "selection for\nresistance", col='red') +
      annotate("text", x = win_opens+0.1, y = 0.5, label = "S & R killed no selection", cex=3, angle=90) +
      annotate("text", x = win_closes-0.1, y = 0.5, label = "selection against resistance (if a cost)", cex=3, angle=90)
    #annotate("text", x = win_opens+0.1, y = 0.9, label = "S & R killed\nno selection", cex=2.5) +
    #annotate("text", x = win_closes-0.1, y = 0.07, label = "selection against\nresistance (if\na cost)", cex=2.5)
  }
  
  if (xreverse) gg <- gg + scale_x_continuous(trans='reverse')
  
  # add calculations of inputs for resistance model
  # not quite sure how I'm going to get out yet ...
  # effectivess, resistance_restoraion and dominance
  # if I do for all elements of conc that will facilitate creating a time-to-resistance plot on same scale
  # these eqs from paper2 based on fitness so need 1-mort
  # 1. Effectiveness | 1 - SS
  # 2. Exposure, can't be claculated will need to be provided
  # 3. Resistance restoration  | (RR-SS) / Effectiveness 
  # 4. Dominance of resistance | (SR-SS)/(RR-SS) 
  if (out)
  {
    #temp to stop sim running if just want advantage
    if (!adv | plot) dfsim <- wos_sim(concs = concs,
                     mort_rr = mort_rr,
                     mort_sr = mort_sr,
                     mort_ss = mort_ss,
                     exposure = exposure,
                     max_gen = max_gen,
                     startfreq = startfreq,
                     plot = FALSE
                     )

    dfadv <- wos_advantage(concs = concs,
                     mort_rr = mort_rr,
                     mort_sr = mort_sr,
                     mort_ss = mort_ss,
                     exposure = exposure,
                     #max_gen = max_gen,
                     startfreq = startfreq)     
        
    
    #library(patchwork)

    #plot(gg / gg_dom/ gg_timetor + plot_layout(ncol = 1, heights = c(5,1,5)))
    if (plot)
    {
      gg_timetor <- wos_plot_timetor(dfsim,
                                     title=paste0("simulation, exposure=",exposure," starting resistance frequency=",startfreq),
                                     plot=FALSE)
      
      #selective advantage
      #gg_adv <- wos_plot_timetor(dfadv, x='conc', y='selective_advantage', ylab='selective\nadvantage', 
      gg_adv <- wos_plot_timetor(dfadv, x='conc', y='relative_fitness', ylab='relative\nfitness',                                  
                                     title=paste0("relative fitness, exposure=",exposure," starting resistance frequency=",startfreq),
                                     plot=FALSE)
      #log selective advantage
      if (logadv) gg_adv <- gg_adv + scale_y_continuous(trans='log')
      
      gg_dom <- wos_plot_input(dfsim, y='dom_resist', ylab='dominance', plot=FALSE)
      gg_rr <- wos_plot_input(dfsim, y='resist_restor', ylab='resistance restoration', title=NULL, plot=FALSE)
      gg_ef <- wos_plot_input(dfsim, y='effectiveness', ylab='effectiveness', title=NULL, plot=FALSE)
      
      #plot(gg / gg_timetor / gg_dom / gg_rr / gg_ef + plot_layout(ncol = 1, heights = c(5,5,1,1,1)))
      #plot(gg / gg_timetor / gg_adv / gg_dom / gg_rr / gg_ef + plot_layout(ncol = 1, heights = c(5,5,5,1,1,1)))      

      #missing out input plots for now
      plot(gg / gg_timetor / gg_adv + plot_layout(ncol = 1, heights = c(5,5,5)))        
            
    }
            
    if (adv) invisible(dfadv)
    else invisible(dfsim) 
    #invisible(listOut)
    #invisible(resistPoints)
  } else 
  {
    if (plot) plot(gg)
    
    invisible(gg)    
  }



}