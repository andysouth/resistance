#' to run one simple scenario without an input file, experimental
#' 
#' May restructure later.  
#' 
#' @param calibration one of a limited set of integers effecting how scenarios are run 
#' @param produce.plots whether to produce plots
#' @param savePlots whether to save plots to hardcoded filenames
#' @param save.fitvals whether to save fitness scores to csv 0=no, 1=yes 
#' 
#' @return nothing yet
#' @export
#' 
resistSimple <- function( calibration = 1012,
                          produce.plots = TRUE,
                          savePlots = FALSE,
                          save.fitvals = 0 )
{
  
  
  
  #calling new function to create input matrix
  input <- createInputMatrix( params.csv=FALSE, calibration=calibration, save.fitvals=save.fitvals)
  
  
  # Run Model 
  # creates listOut$results, listOut$genotype & listOut$fitness
  # set produce.plots = FALSE so I can just have one plot later
  listOut <- runModel( input, calibration, produce.plots = FALSE )
  
  # Plot R and S allele frequencies over generations by M&F
  genplot <- plotallele.freq( listOut$results[[1]] ) 

  
}