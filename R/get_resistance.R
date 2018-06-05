#' Return allele frequencies
#' 
#' extract just the R allele frequencies at a specified locus, for all scenarios run
#' in a dataframe with the inputs, suitable for ggplot plotting 
#' this outputs mean of M&F I could give option to get M,F separately & maybe both 

#' @param locus locus of interest (i.e. 1 or 2)
#' @param listOut list containing the results matrices generated for each scenario run
#' @param truncate_at_1 whether to truncate generations once resistance reaches 1
#' @examples 
#' #mutliple scenarios
#' i1 <- setInputOneScenario( h.RS1_A0 = 0.5 )
#' i2 <- setInputOneScenario( h.RS1_A0 = 0.8 )
#' input <- cbind(i1,i2)
#' listOut <- runModel2( input )
#' df_resist <- get_resistance(locus=1, listOut) 
#' #ggplot(df_resist, aes(x=generation, y=resistance)) + geom_point()
#' 
#' @return dataframe
#' @export


get_resistance <- function(locus, listOut, truncate_at_1 = TRUE){
  
  
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
    
    #truncate resistances once they reach 1 to make auto-plotting easier
    #had to be careful about float rounding
    #BEWARE this could hide fluctuating resistances, but that's not expected to happen
    if (truncate_at_1)
        resistances <- resistances[ which(resistances < 0.99) ]
    
    dfResist <- data.frame(generation=c(1:length(resistances)))
    
    dfResist$resistance <- resistances
    
    #to add on the inputs 
    #transposed to get each input as a named column repeated for all generations
    dfIns <- data.frame( t(listOut$input[,scenarioNum]) )
    
    #bind input columns on
    dfResist <- cbind(dfResist, dfIns)
    
    #31/5/2018 add option to add a scenario column for windows of selection simulations
    dfResist$scenario <- scenarioNum
    
    #bind scenario rows on
    if ( scenarioNum == 1 ) dfResistAll <- dfResist
    else  dfResistAll <- rbind(dfResistAll, dfResist)
    
  }
  

  
  return( dfResistAll )
}