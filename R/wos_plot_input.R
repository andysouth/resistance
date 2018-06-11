
#gg_dom <- wos_plot_input(dfout, input='dom_resist', label='dominance')

wos_plot_input <- function( dfout,
                         name = "dom_resist",
                         label = "dominance",
                         title = "selected simulation inputs",
                         addwindow = TRUE,
                         addshading = TRUE,
                         addlabels = TRUE,
                         legendpos = 'bottom', #'right'
                         plot = TRUE,
                         xreverse = TRUE
) {


# plot dominance
gg <- dfout %>%
  ggplot(aes_string(x='conc', y=name))+
  #geom_point(shape=1, size=0.6) + #open circle
  geom_line(linetype=4) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.7))) +      
  ggtitle(title) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,1)) +
  #labs(y='',x='')
  labs(y=label,x='')
if (xreverse) gg <- gg + scale_x_continuous(trans='reverse')  

if(plot) plot(gg)

invisible(gg)

}