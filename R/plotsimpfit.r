#' plot simple vis of fitness and how it is influenced by inputs
#' 
#' initially for a single insecticide
#' 
#' @param exposure prop vectors exposed to insecticide
#' @param effectivness prop exposed susceptible vectors killed
#' @param dominance dominance of resistance allele
#' @param rr_restoration not used yet, ability of homozygous resistance to restore fitness
#' 
#' @examples 
#' #

#' @return list of plot components
#' @export

plotsimpfit <- function ( exposure = 0.8, 
                          effectiveness1 = 0.8,
                          effectiveness2 = 0.8,
                          dominance = 0.8,
                          rr_restoration1 = 0.8 ){	

# todo decide whether to do for a single locus or 2  
a_fitloc <- fitnessSingleLocus( effectiveness1 = effectiveness1,
                                effectiveness2 = effectiveness2,
                                dominance = dominance,
                                selection_co = rr_restoration1 * effectiveness1,
                                cost = 0 )
  
#remove blank borders
par(mar = c(0,0,0,0),oma = c(0, 0, 0, 0))

#create blank plot
#can change extents here to allow in more or less things in borders
#making the extents bigger makes the plotted shapes smaller, but makes it smaller in y too ...
#plot(c(-0.2,1.25),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)
plot(c(-0.2,1.5),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)


rect(xleft = 0, xright = 0.05, ybottom = 0, ytop = 1, col=rgb(r=0, g=0, b=0.5, a=0.5))

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