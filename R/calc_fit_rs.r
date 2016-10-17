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
#' @param verbose whether to print stuff to console
#' 
#' @examples 
#' #see also effectiveness_exposure_difference.Rmd
#' df_base_ <- as.data.frame(fitnessGenotype(exp1=0.5, exp2=0.5, plot=FALSE))[2,]
#' df_base_ <- data.frame(t(df_base_),row.names=row.names(t(df_base_)))
#' 
#' calc_fit_rs(df_base_,'f', round_places=2)
#' 
#' @return dataframe of mean fitness
#' @export

calc_fit_rs <- function ( df_fit, column, column_facet = NULL, round_places = 3,
                          verbose = FALSE ){
  
  
  #counting S1,S2,R1,R2 
  df_fit$S1 <- stringr::str_count(stringr::str_sub(rownames(df_fit),1,2),'S')
  df_fit$R1 <- stringr::str_count(stringr::str_sub(rownames(df_fit),1,2),'R')
  df_fit$S2 <- stringr::str_count(stringr::str_sub(rownames(df_fit),5,6),'S')
  df_fit$R2 <- stringr::str_count(stringr::str_sub(rownames(df_fit),5,6),'R')

  
  alleles <- c('R1','S1','R2','S2')
  
  #just for testing & verbose output
  #and there must be a better way of doing
  df_fit$S1fit <- df_fit$R1fit <- df_fit$S2fit <- df_fit$R2fit <- 0

  df_meanfit <- data.frame(allele=alleles, row.names=alleles)
  
  df_meanfit$meanfit <- 0
  
  for(allele in alleles)
  {
    df_meanfit[allele,'meanfit'] <- mean( (df_fit[,allele] * df_fit[,column]) ) 
    
    #just to test the calc & for verbose output
    df_fit[paste0(allele,'fit')] <- df_fit[,allele] * df_fit[,column]
  }
  
  # testing
  if (verbose)
    print(round(df_fit, round_places))
  
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