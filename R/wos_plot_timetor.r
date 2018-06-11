
wos_plot_timetor <- function()
{

#plot simulation results
gg_timetor <- dfout %>%
  
  ggplot(aes(x=conc, y=time_to_resistance0.5))+
  geom_point() +
  #geom_line(lwd=2,alpha=0.6) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(title = paste0("simulation, exposure=",exposure," starting resistance frequency=",startfreq)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='') 
#labs(x='Time or declining insecticide concentration')
if (xreverse) gg_timetor <- gg_timetor + scale_x_continuous(trans='reverse')

}