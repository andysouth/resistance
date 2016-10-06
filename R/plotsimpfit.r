#' bar plot simple vis of fitness and how it is influenced by inputs
#' 
#' DEPRECATED see plot_fit_rs() which does it a better way with points rather than bars
#' initially for a single insecticide
#' 
#' @param exposure prop vectors exposed to insecticide
#' @param eff1 effectiveness1
#' @param eff2 effectiveness2
#' @param dom1 dominance1
#' @param dom2 dominance2
#' @param rr_1 resistance restoration 1 selection coefficient = resistance restoration * effectiveness
#' @param rr_2 resistance restoration 2#' 
#' @examples 
#' #

#' @return list of plot components
#' @export

plotsimpfit <- function ( exposure = 0.8, 
                          eff1 = 0.8,
                          eff2 = 0.8,
                          dom1 = 0.8,
                          dom2 = 0.8,                          
                          rr_1 = 0.8,
                          rr_2 = 0.8){	

# todo decide whether to do for a single locus or 2  
a_fitloc <- fitnessSingleLocus( eff1 = eff1,
                                eff2 = eff2,
                                dom1 = dom1,
                                dom2 = dom2,                          
                                rr_1 = rr_1,
                                rr_2 = rr_2 )
  
#remove blank borders
par(mar = c(0,0,0,0),oma = c(0, 0, 0, 0))

#create blank plot
#can change extents here to allow in more or less things in borders
#making the extents bigger makes the plotted shapes smaller, but makes it smaller in y too ...
#plot(c(-0.2,1.25),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)
plot(c(-0.2,1.5),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)


rect(xleft = 0, xright = 0.05, ybottom = 0, ytop = 1, col=rgb(0, 0, 0.5, alpha=0.5))

text(x=c(0,0), y=c(0,1), labels=c(0,1), pos=2) #pos=1234 bltr

#potential problem with this is that the x pos varies with width of the plot
#text(x=c(0.05,0.2,0.35), y=c(1,1,1), labels=c('RR','RS','SS'), pos=4) #pos=1234 bltr

text(x=c(0.05,0.2,0.35), y=a_fitloc[c('RR1','RS1','SS1'),'hi'], labels=c('RR','RS','SS'), pos=4) #pos=1234 bltr

axis(4, at= a_fitloc[c('RR1','RS1','SS1'),'hi'], labels=FALSE, lwd=0, lwd.ticks=1, pos = 0.05, tcl = -0.06)

# tcl=tickmark length, +ve =towards plot
#axis(2, at=c(0, 1-man, 1), labels=FALSE, lwd=3, pos = -0.1, tcl = 1)


# old way I was going to do :

# df <- data.frame(
#   xmin = rep(0,3),
#   xmax = rep(0.1,3),
#   ymin = c(0, 1-exposure, 1-(exposure*effectiveness)),
#   ymax = c(1-exposure, 1-(exposure*effectiveness),1),
#   z=c(1:3) #determines colours of blocks
# )
# #add rectangles
# rect(xleft = df$xmin, xright = df$xmax, ybottom = df$ymin, ytop = df$ymax, col=df$z)

}