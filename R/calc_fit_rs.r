#' calc mean fitness by resistant/susceptible genotypes
#' 
#' andy investigative function, might be daft   
#' calculates mean fitness for R&S for each insecticide by counting R&S in the genotypes
#' and multiplying by the fitness of each genotype.
#' see 14/10/16 in resistanceModelNotes
#'   
#' accepts a dataframe with a column or rownames containing RS identifiers and another

#' @param df_fit dataframe
#' @param column name of column containing fitness values
#' @param column_facet optional column to facet by
#' @param round_places decimal places to round output to
#' @param title optional title for the plot
#' 
#' @examples 
#' #singleLocus
#' df_fit1 <- as.data.frame(fitnessSingleLocus())
#' #temp adding an extra column for faceting
#' df_fit1$locus <- paste('locus', c(1,1,1,2,2,2))
#' plot_fit_rs(df_fit1, 'hi', column_facet='locus')
#' 
#' #trying to be able to facet for no & hi 
#' #but the function relies on rownames which get lost this way
#' #might want to convert the whole function, but want to keep it working for niche&genotype examples
#' #df_fit2 <- gather(df_fit1, key=exposure_group, value=fitness, no, lo, hi) 
#' #plot_fit_rs(df_fit2, 'fitness', column_facet='exposure_group')  
#'  
#'  
#' #niche
#' #library(reshape2)
#' df_fit2 <- reshape2::melt(fitnessNiche()[,,'A','B'])
#' rownames(df_fit2) <- paste(df_fit2$locus1, df_fit2$locus2)
#' plot_fit_rs(df_fit2, 'value')
#' 
#'  
#' #genotype
#' #transpose to get in useable format
#' df_fit3 <- as.data.frame(t(as.data.frame(fitnessGenotype())))
#' plot_fit_rs(df_fit3,'f')
#' 
#' @return dataframe of plotted fitness values and colours
#' @export

calc_fit_rs <- function ( df_fit, column, column_facet = NULL, round_places = 3, title = NULL ){
  
  
  #counting S1,S2,R1,R2 
  df_fit$S1 <- stringr::str_count(stringr::str_sub(rownames(df_fit),1,2),'S')
  df_fit$R1 <- stringr::str_count(stringr::str_sub(rownames(df_fit),1,2),'R')
  df_fit$S2 <- stringr::str_count(stringr::str_sub(rownames(df_fit),5,6),'S')
  df_fit$R2 <- stringr::str_count(stringr::str_sub(rownames(df_fit),5,6),'R')
  
  
  alleles <- c('R1','S1','R2','S2')

  df_meanfit <- data.frame(allele=alleles, row.names=alleles)
  
  df_meanfit$meanfit <- 0
  
  for(allele in alleles)
  {
    df_meanfit[allele,'meanfit'] <- mean( (df_fit[,allele] * df_fit[,column]) )    
  }
  
  #df_meanfit['R1','meanfit'] <- mean( (df_fit$num_r1 * df_fit[,column]) )  
  
  #add calc of differences between R & S
  alleles <- c('R1-S1','R2-S2')
  df_meanfit2 <- data.frame(allele=alleles, row.names=alleles)
  
  df_meanfit2['R1-S1','meanfit'] <- df_meanfit['R1','meanfit'] - df_meanfit['S1','meanfit']
  df_meanfit2['R2-S2','meanfit'] <- df_meanfit['R2','meanfit'] - df_meanfit['S2','meanfit']
  
  df_meanfit <- rbind(df_meanfit, df_meanfit2)
  
  df_meanfit$meanfit <- round(df_meanfit$meanfit,round_places)
  
  return(df_meanfit)
  
  
}