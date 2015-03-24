#' to run one simple scenario without an input file, experimental
#' 
#' May restructure later.  
#' 
# cut these args out, defaults in setInputOneScenario()  
# @param calibration one of a limited set of integers effecting how scenarios are run 
# @param produce.plots whether to produce plots
# @param savePlots whether to save plots to hardcoded filenames
# @param save.fitvals whether to save fitness scores to csv 0=no, 1=yes 

#' @param ... extra arguments passed to setInputOneScenario()
#' 
#' @return nothing yet
#' @export
#' 
resistSimple <- function(...)
{
 
#cut these args out, defaults in setInputOneScenario()   
#   calibration = 1012,
#   produce.plots = TRUE,
#   savePlots = FALSE,
#   save.fitvals = 0,
  
  #can I use this to output what args were passed ?
  #nearly but from shiny it has the varnames not their values
  #dput(match.call())
  #this fails too
  #cat(as.character(mget(names(formals()),sys.frame(sys.nframe()))))
  
  #calling new function to create input matrix
  #input <- createInputMatrix( params.csv=FALSE, calibration=calibration, save.fitvals=save.fitvals)
  
  #todo next allow all the args to this to be changed
  #by allowing all these args to be set for this function
  #OR can I use the ... operator to allow them to be set without specifying them
  #aha yes seems so !!
  input <- setInputOneScenario(...)
  
  
  # Run Model 
  # creates listOut$results, listOut$genotype & listOut$fitness
  # set produce.plots = FALSE so I can just have one plot later
  listOut <- runModel( input, produce.plots = FALSE )
  
  # Plot R and S allele frequencies over generations by M&F
  genplot <- plotallele.freq.andy( listOut$results[[1]] ) 

  
}