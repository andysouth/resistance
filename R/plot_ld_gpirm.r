#' try to plot genotype frequencies to show linkage disequilibrium
#' 
#' as done in GPIRM p122
#' BEWARE this initial version assumes dominance=1

#' @param genotype genotype frequencies by generation
#' @param gen_num which generation to plot for
#' @param labgen whether to add label for generation number
#' 
#' @examples 
#' input <- setInputOneScenario()
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=8)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=11)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=7:12)
#' 
#' #for single insecticide
#' a <- setExposure( exposure=0.9,  insecticideUsed = 'insecticide1' )
#' input <- setInputOneScenario( a=a )
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=c(2:9))
#' 
#' #for mixture & modifying main params
#' #exposure 0.5, effectiveness 0.8, dominances 1
#' a <- setExposure( exposure=0.5,  insecticideUsed = 'mixture' )
#' input <- setInputOneScenario( a=a, phi.SS1_A0=0.8, phi.SS2_0B=0.8, h.RS1_A0=1, h.RS2_0B=1)
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=seq(from = 5, to = 50, by =5))
#' 
#' TODO this seems to expose an error in my plotting routine
#' OR can this plot idea not cope with our data ?
#' is this caused by having different effectivenesses for each insecticide ?
#' SEE whitespace between generations 20 & 40
#' #exposure 0.5, eff1 0.8, eff2 0.5, dominances 1
#' a <- setExposure( exposure=0.5,  insecticideUsed = 'mixture' )
#' input <- setInputOneScenario( a=a, phi.SS1_A0=0.8, phi.SS2_0B=0.5, h.RS1_A0=1, h.RS2_0B=1)
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=seq(from=5, to=50, by=5))
#' 
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=seq(from=20, to=40, by=4))
#' 
#' #i think this is the equiv resistance freq plot for comparison
#' runcurtis_f2( exposure=0.5, phi.SS1_A0=0.8, phi.SS2_0B=0.5, h.RS1_A0=1, h.RS2_0B=1)
#' 
#' #try to set effectivenesses equal to see if white space disapears ?
#' #it does but still something maybe wrong ~gen28
#' #I need to think properly about what its doing, maybe will help by outputting allele freqs
#' #exposure 0.5, eff1 0.7, eff2 0.7, dominances 1
#' a <- setExposure( exposure=0.5,  insecticideUsed = 'mixture' )
#' input <- setInputOneScenario( a=a, phi.SS1_A0=0.7, phi.SS2_0B=0.7, h.RS1_A0=1, h.RS2_0B=1)
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen=seq(from=20, to=40, by=4)) 
#' 
#' #try looking at the LD calculations see if that helps
#' 
#' @return dataframe of plotted fitness values and colours
#' @export

plot_ld_gpirm <- function ( genotype = NULL, 
                            gen = NULL,
                            labgen = TRUE){
 
  # so that it will run with default inputs
  if (is.null(genotype))
  {
    input <- setInputOneScenario()
    listOut <- runModel2(input)
    genotype <- listOut$genotype[[1]]
  }
  if (is.null(gen))
  {
    gen <- c(7:12)
  }
    
  num_panels <- length(gen)
  
  #remove blank borders
  par(mar = c(0,0,0,0),oma = c(0, 0, 0, 0))
  
  # first go at setting layout of panels
  # this does constant 2 rows
  # if odd add 1
  if (num_panels %% 2 != 0) num_panels=num_panels+1
  layout( matrix(c(1:num_panels), 2, byrow = TRUE))
  #layout.show(num_panels)
  
  #could use listOut$genotype but it has cis & trans in ...
  #AHA 2nd problem was that genotype includes cis & trans
  #quick fix
  #TODO make a safer version of this !!
  gen2 <- genotype
  gen2[,'RS1RS2_cis'] <- gen2[,'RS1RS2_cis'] + gen2[,'RS1RS2_trans']
  colnames(gen2)[ which(colnames(gen2)=='RS1RS2_cis')] <- 'RS1RS2'
  gen2 <- gen2[,- which(colnames(gen2)=='RS1RS2_trans')]
  
  for( gen_num in gen)
  {
    
    tp <- gen2[gen_num,]
    
    #rectangle  boundaries (see notebook)
    #first assuming complete dominance
    #TODO see yellow leon notebook for way of including dominance 
    p1 <- tp['RR1SS2'] + tp['RS1SS2']
    p2 <- tp['RR1RR2'] + tp['RS1RR2'] +
      tp['RR1RS2'] + tp['RS1RS2']
    p3 <- tp['SS1RR2'] + tp['SS1RS2']
    p4 <- p2
    
    #remember GPIRM diagram only has R & S for each insecticide
    #so my calculation above tries to work out whether R or S for each
    #when dominance is 1 this is straightforward because R = RR + SR
    #testing
    cat("gen",gen_num," AR:",signif(p1,2), "  ARBR:",signif(p2,2), "  BR:",signif(p3,2),"\n")
    
    
    #create blank plot
    #can change extents here to allow in more or less things in borders
    plot(c(-0.2,1.25),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)
    
    #rectangles
    # b S1S2 first so a goes on top
    rect(xleft = p1, xright = 1, ybottom = 0, ytop = 1-p3, col='yellow')
    # a R1R2
    rect(xleft = 0,  xright = p2, ybottom = 1-p4, ytop = 1, col='purple') 
    # c S1R2
    rect(xleft = p2,  xright = 1, ybottom = 1-p3, ytop = 1, col='blue')  
    # d R1S2
    rect(xleft = 0,  xright = p1, ybottom = 0, ytop = 1-p4, col='red')  
    
    #text labels
    #text(x=p1+0.5*(1-p1), y=1.1, "ASBS")
    #text(x=0.9, y=0.1, expression(A^S B^S))
    text(x=0.85, y=0.1, "ASBS")
    text(x=0.15, y=0.9, "ARBR")   
    
    text(x=0.1, y=0, "AR",cex=0.8)
    text(x=0.8, y=0, "AS",cex=0.8) 
  
    #add optional generation num label
    if (labgen)
       mtext(paste0('gen',gen_num), line=-2, cex=0.8)
  }
  

    
  
  # #library(ggrepel)
  # #library(stringr)
  # 
  # #counting S & R, and making a first test colour scheme with red for resist & blue for susceptible 
  # df_fit$num_s <- stringr::str_count(rownames(df_fit),'S')
  # df_fit$num_r <- stringr::str_count(rownames(df_fit),'R')  
  # df_fit$col <- rgb(df_fit$num_r/max(df_fit$num_r),0,df_fit$num_s/max(df_fit$num_s), alpha=0.5)
  # 
  # gg <- ggplot(df_fit, aes_string(x=1, y=column)) +
  #       #ylim(0,1) +
  #       ylim(ylim) +
  #       xlim(0.9,1.5) +
  #       geom_point(size=3, colour=df_fit$col) +
  #       ggrepel::geom_text_repel( aes(label=rownames(df_fit)), nudge_x=0.2, size=2) +
  #       ylab('fitness') +
  #       ggtitle(title) + 
  #       theme_bw() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             axis.line.x = element_blank(),
  #             axis.ticks.x = element_blank(),
  #             #panel.grid.major.x = element_blank(),
  #             panel.grid.major.x = element_blank(),
  #             panel.grid.minor.x = element_blank()
  #             )
  # 
  # if (yblank)
  # {
  #   gg <- gg + theme(axis.text.y = element_blank(),
  #                    axis.title.y = element_blank())
  # }
  # 
  #       # or blank & add back in
  #       # theme_void() +
  #       # theme(axis.text.y = element_text(hjust=0.1), 
  #       #       axis.title.y = element_text(angle=90),
  #       #       axis.line.y = element_line(size=1),
  #       #       axis.ticks.y = element_line(size=1),
  #       #       panel.grid.major.y = element_line(size=1))
  # 
  # gg
  # 
  # # optional facet
  # if ( !is.null(column_facet) )
  # {
  #   gg <- gg + facet_wrap(column_facet)
  # }
  #  
  # print(gg)
  # 
  # return(df_fit)
  
  
}