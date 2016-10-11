#' plot exposure to insecticides
#' 
#' accepts an array of exposures created by setExposure()

#' @param a_expos array of exposures
# @param ? whether to plot m&f, or just f
#' @param num_levels number of exposure levels, 2 just shows no & hi, 3 shows lo as well
#' @param title optional title for the plot
#' 
#' @examples 
#' a_expos <- setExposure()
#' plot_exposure(a_expos)
#' 
#' @return dataframe of plotted exposures
#' @export

plot_exposure <- function ( a_expos, num_levels = 2, title = NULL ){
 
  #convert to df & transpose to get in useable form
  df_e <- as.data.frame(t(as.data.frame(a_expos)))

  #get rownames into a column & remove '.'
  df_e$exp_id <- gsub('\\.','',rownames(df_e))
  
  #reformat data
  df_e <- tidyr::gather(df_e, key=sex, value=exposure, m,f)
  # exp_id sex exposure
  # 1      00   m      0.1
  # 2      a0   m      0.0
  # 3      A0   m      0.0
  # 4      0b   m      0.0
  
  #to remove lo (lowercase exposure rows)
  #for when we are only looking at no & hi
  if ( num_levels==2 )
  {
    df_e <- df_e[-grep('[ab]', df_e$exp_id),]
  }

  #todo would be nice to colour bars 00=pale A0=1col AB=2cols combined
  
  gg <- ggplot(df_e, aes(exp_id,exposure)) +
    geom_bar(stat = "identity", width = 0.5) +
    #geom_point() +
    facet_wrap('sex') +
    ggtitle(title) +
    theme_light() +
    theme(axis.title.x = element_blank())
    
  # #counting S & R, and making a first test colour scheme with red for resist & blue for susceptible 
  # df_fit$num_s <- stringr::str_count(rownames(df_fit),'S')
  # df_fit$num_r <- stringr::str_count(rownames(df_fit),'R')  
  # df_fit$col <- rgb(df_fit$num_r/max(df_fit$num_r),0,df_fit$num_s/max(df_fit$num_s), alpha=0.5)
  # 
  # gg <- ggplot(df_fit, aes_string(x=1, y=column)) +
  #       ylim(0,1) +
  #       geom_point(size=3, colour=df_fit$col) +
  #       ggrepel::geom_text_repel( aes(label=rownames(df_fit)), nudge_x=0.1) +
  #       ylab('fitness') +
  #       ggtitle(title) + 
  #       theme_bw() +
  #       theme(axis.text.x = element_blank(),
  #             axis.title.x = element_blank(),
  #             axis.line.x = element_blank(),
  #             axis.ticks.x = element_blank(),
  #             panel.grid.major.x = element_blank())
  #       # or blank & add back in
  #       # theme_void() +
  #       # theme(axis.text.y = element_text(hjust=0.1), 
  #       #       axis.title.y = element_text(angle=90),
  #       #       axis.line.y = element_line(size=1),
  #       #       axis.ticks.y = element_line(size=1),
  #       #       panel.grid.major.y = element_line(size=1))

  gg
  
  # # optional facet
  # if ( !is.null(column_facet) )
  # {
  #   gg <- gg + facet_wrap(column_facet)
  # }
   
  print(gg)
  
  return(df_e)
  
  
}