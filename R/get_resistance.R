#' Return allele frequencies
#' 
#' extract just the R allele frequencies at a specified locus, for all scenarios run
#' in a dataframe with the inputs, suitable for ggplot plotting 
#' this outputs mean of M&F I could give option to get M,F separately & maybe both 

#' @param locus locus of interest (i.e. 1 or 2)
#' @param listOut list containing the results matrices generated for each scenario run
#' @examples 
#' #mutliple scenarios
#' listOut <- setInputOneScenario()
#' df_resist <- get_resistance(locus=1, listOut) 
#' #ggplot(df_resist, aes(x=generation, y=resistance)) + geom_point()
#' 
#' @return dataframe
#' @export


get_resistance <- function(locus, listOut){
  
  
  #this allows a single scenario to be passed
  if (class(listOut)=='matrix')
  {
    numScenarios <- 1 
    numGenerations <- nrow(listOut)
  } else
  {
    numScenarios <- length(listOut$results) 
    numGenerations <- nrow(listOut$results[[1]])
  }
  

  dfResistAll <- NULL
  
  #for each scenario (model run based on one set of parameters)
  for(scenarioNum in 1:numScenarios)
  {

    dfResist <- data.frame(generation=c(1:numGenerations))
    
    #getting results matrix out of the list
    #this allows a single results matrix to be passed too
    if (class(listOut)=='matrix') results <- listOut
    else results <- listOut$results[[scenarioNum]]
    
    resistances <- NULL
    if (locus==1)
      #mean of M&F
      resistances <- rowMeans(cbind(results[,'m.R1'],
                                    results[,'f.R1'] ))
    else if (locus==2)
      #mean of M&F
      resistances <- rowMeans(cbind(results[,'m.R2'],
                                    results[,'f.R2'] ))
    
    dfResist$resistance <- resistances
    
    #to add on the inputs 
    #may want to transpose them because I want each input as a named column repeated for all generations
    dfIns <- data.frame( t(listOut$input[,scenarioNum]) )
    
    #bind input columns on
    dfResist <- cbind(dfResist, dfIns)
    
    #bind scenario rows on
    if ( scenarioNum == 1 ) dfResistAll <- dfResist
    else  dfResistAll <- rbind(dfResistAll, dfResist)
    
  }
  

  
  return( dfResistAll )
}