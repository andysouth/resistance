#' Find number of generations to reach critical points of resistance
#' 
#' and finds the number of generations it takes for the R allele frequency to reach passed criticalPoints
#' Returns NA if freq does not reach 
#' refactored from timetoFifty()

#' @param listOut results & inputs from a set of scenario runs
#' @param locus locus of interest (1,2, 'either' or 'both')
# @param max_gen number of generations [ i don't think needed anymore ] 

#' 
#' @return matrix with a column for each scenario, giving the generation number at which frequency of R is first greater than critical points
#' @export


findResistancePoints <- function( listOut,
                                  locus = 1,
                                  criticalPoints = c(0.1,0.25,0.5)
                                  ){
  
  numScenarios <- ncol(listOut$input)
  
  #could first create matrix to receive the results, name rows including gen_cP for generations to reach criticalPoints
  resistGens <- matrix( nrow=length(criticalPoints), ncol=numScenarios, dimnames=list(paste0('gen_cP',criticalPoints) ) )
    
  #should i go through criticalPoints or scenarios first ?
  #if i go through scenarios first I might be able to do all
  #critical points at once
  
  for(scenarioNum in 1:numScenarios)
  {
    #for each critical point
    for(criticalPointNum in seq_along(criticalPoints) )
    {
      criticalPoint <- criticalPoints[criticalPointNum]
      #cat("criticalPoint ", criticalPoint,"\n")
      
      resistances <- NULL
      if (locus==1)
        #mean of M&F
        resistances <- rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R1'],
                                      listOut$results[[scenarioNum]][,'f.R1'] ))
      else if (locus==2)
        #mean of M&F
        resistances <- rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R2'],
                                      listOut$results[[scenarioNum]][,'f.R2'] ))
      else if (locus=='either')
        #calculate the parallel maximum of the 2 means
        #to find the 1st locus that reaches threshold
        resistances <- pmax(
                        rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R1'],
                                       listOut$results[[scenarioNum]][,'f.R1'] )),
                        rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R2'],
                                       listOut$results[[scenarioNum]][,'f.R2'] )) )
      else if (locus=='both')      
        #calculate the parallel minimum of the 2 means
        #to find the 2nd locus that reaches threshold
        resistances <- pmin(
          rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R1'],
                         listOut$results[[scenarioNum]][,'f.R1'] )),
          rowMeans(cbind(listOut$results[[scenarioNum]][,'m.R2'],
                         listOut$results[[scenarioNum]][,'f.R2'] )) )
      else
        stop("locus is not one of 1,2,'either','both':", locus)
             
      #which generations have resistance greater than critical point
      gens <- which( resistances > criticalPoint)
      
      
      firstGen <- gens[1]
      #store in results
      resistGens[criticalPointNum,scenarioNum] <- firstGen
      
    }    
  }

  
  #listOut$input['sexLinked',]
  
  return(resistGens)
}