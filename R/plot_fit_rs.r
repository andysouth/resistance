#' plot fitness by resistant/susceptible genotypes
#' 
#' accepts a dataframe with a column or rownames containing RS identifiers and another

#' @param df_fit dataframe
#' @param column name of column containing fitness values
#' @param column_facet optional column to facet by
#' @param title optional title for the plot
#' 
#' @examples 
#' #singleLocus
#' df_fit1 <- as.data.frame(fitnessSingleLocus())
#' #temp adding an extra column for faceting
#' df_fit1$locus <- paste('locus', c(1,1,1,2,2,2))
#' plot_fit_rs(df_fit1, 'hi', column_facet='locus')
#' 
#'  
#' #niche
#' library(reshape2)
#' df_fit2 <- melt(fitnessNiche()[,,'A','B'])
#' rownames(df_fit2) <- paste(df_fit2$locus1, df_fit2$locus2)
#' plot_fit_rs(df_fit2, 'value')
#' 
#'  
#' #genotype
#' #transpose to get in useable format
#' df_fit3 <- as.data.frame(t(as.data.frame(fitnessGenotype())))
#' plot_fit_rs(df_fit3,'f')
#' 
#' @return 
#' @export

plot_fit_rs <- function ( df_fit, column, column_facet = NULL, title = NULL ){
 
  library(ggrepel)
  
  library(stringr)

  #counting S & R, and making a first test colour scheme with red for resist & blue for susceptible 
  df_fit$num_s <- str_count(rownames(df_fit),'S')
  df_fit$num_r <- str_count(rownames(df_fit),'R')  
  df_fit$col <- rgb(df_fit$num_r/max(df_fit$num_r),0,df_fit$num_s/max(df_fit$num_s), alpha=0.5)
  
  gg <- ggplot(df_fit, aes_string(x=1, y=column)) +
        ylim(0,1) +
        geom_point(size=3, colour=df_fit$col) +
        geom_text_repel( aes(label=rownames(df_fit)), nudge_x=0.1) +
        ylab('fitness') +
        ggtitle(title) + 
        theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_blank())
        # or blank & add back in
        # theme_void() +
        # theme(axis.text.y = element_text(hjust=0.1), 
        #       axis.title.y = element_text(angle=90),
        #       axis.line.y = element_line(size=1),
        #       axis.ticks.y = element_line(size=1),
        #       panel.grid.major.y = element_line(size=1))

  gg
  
  # optional facet
  if ( !is.null(column_facet) )
  {
    gg <- gg + facet_wrap(column_facet)
  }
   
  print(gg)
  
  return(df_fit)
  
  # ggplot(df_fit, aes_(x=1, y=column, label=rownames(df_fit) )) +
  #   ylim(0,1) +
  #   geom_point() +
  #   #geom_text(hjust = 0) #, nudge_x = 0.05)
  #   geom_text_repel(nudge_x=0.1)
  
}