#' try to plot genotype frequencies to show linkage disequilibrium
#' 
#' as done in GPIRM p122

#' @param genotype genotype frequencies by generation
#' @param gen_num which generation to plot for
#' 
#' @examples 
#' input <- setInputOneScenario()
#' listOut <- runModel2(input)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=5)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=6)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=7)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=8)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=9)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=10)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=11)
#' plot_ld_gpirm(genotype=listOut$genotype[[1]], gen_num=12)
#' 
#' 
#' @return dataframe of plotted fitness values and colours
#' @export

plot_ld_gpirm <- function ( genotype, gen_num = 1  ){
 
  
  #could use listOut$genotype but it has cis & trans in ...
  #AHA 2nd problem was that genotype includes cis & trans
  #quick fix
  #TODO make a safer version of this !!
  gen2 <- genotype
  gen2[,'RS1RS2_cis'] <- gen2[,'RS1RS2_cis'] + gen2[,'RS1RS2_trans']
  colnames(gen2)[ which(colnames(gen2)=='RS1RS2_cis')] <- 'RS1RS2'
  gen2 <- gen2[,- which(colnames(gen2)=='RS1RS2_trans')]
  
  tp <- gen2[gen_num,]
  
  #rectangle  boundaries (see notebook)
  #first assuming complete dominance
  #TODO see notebook for way of including dominance 
  p1 <- tp['RR1SS2'] + tp['RS1SS2']
  p2 <- tp['RR1RR2'] + tp['RS1RR2'] +
        tp['RR1RS2'] + tp['RS1RS2']
  p3 <- tp['SS1RR2'] + tp['SS1RS2']
  p4 <- p2
  
  #remove blank borders
  par(mar = c(0,0,0,0),oma = c(0, 0, 0, 0))
  
  #create blank plot
  #can change extents here to allow in more or less things in borders
  plot(c(-0.2,1.25),c(0,1), type='n', axes=FALSE, xlab='', ylab='', asp=1)
  
  #rectangles
  # b first so a goes on top
  rect(xleft = p1, xright = 1, ybottom = 0, ytop = 1-p3, col='yellow')
  # a
  rect(xleft = 0,  xright = p2, ybottom = 1-p4, ytop = 1, col='purple') 
  # c
  rect(xleft = p2,  xright = 1, ybottom = 1-p3, ytop = 1, col='blue')  
  # d
  rect(xleft = 0,  xright = p1, ybottom = 0, ytop = 1-p4, col='red')  

    
  
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