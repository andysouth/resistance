#' Find number of generations to reach critical points of resistance
#' 
#' Post processes model output files to find the number of generations it takes 
#' for the R allele frequency to reach passed criticalPoints.
#' Returns NA if freq does not reach 
#' refactored from timetoFifty()

#' @param listOut results from a set of scenario runs (or a single results matrix)
#' @param locus locus of interest (1,2, 'either' or 'both')
#' @param criticalPoints the critical resistance thresholds that yopu want to assess
# @param max_gen number of generations [ i don't think needed anymore ] 

#' @examples 
#' input <- setInputSensiScenarios( nScenarios=3, h.RS1_A0=c(0,1) )
#' listOut <- runModel2(input)
#' # for all scenarios
#' findResistancePoints(listOut, locus='both')
#' # for selected scenarios
#' findResistancePoints(listOut$results[[2]], locus='both')
#' @return matrix with a column for each scenario, giving the generation number at which frequency of R is first greater than critical points
#' @export


findResistancePoints <- function( listOut,
                                  locus,
                                  criticalPoints = c(0.1,0.25,0.5)
                                  ){
  
  if (class(listOut)=='matrix') numScenarios <- 1 
  else numScenarios <- length(listOut$results) 
  
  #could first create matrix to receive the results, name rows including gen_cP for generations to reach criticalPoints
  resistGens <- matrix( nrow=length(criticalPoints), ncol=numScenarios, dimnames=list(paste0('gen_cP',criticalPoints) ) )
    
  #for each scenario (model run based on one set of parameters)
  for(scenarioNum in 1:numScenarios)
  {
    #for each critical point
    for(criticalPointNum in seq_along(criticalPoints) )
    {
      criticalPoint <- criticalPoints[criticalPointNum]
      #cat("criticalPoint ", criticalPoint,"\n")
      
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
      else if (locus=='either')
        #calculate the parallel maximum of the 2 means
        #to find the 1st locus that reaches threshold
        resistances <- pmax(
                        rowMeans(cbind(results[,'m.R1'],
                                       results[,'f.R1'] )),
                        rowMeans(cbind(results[,'m.R2'],
                                       results[,'f.R2'] )) )
      else if (locus=='both')      
        #calculate the parallel minimum of the 2 means
        #to find the 2nd locus that reaches threshold
        resistances <- pmin(
          rowMeans(cbind(results[,'m.R1'],
                         results[,'f.R1'] )),
          rowMeans(cbind(results[,'m.R2'],
                         results[,'f.R2'] )) )
      else
        stop("locus is not one of 1,2,'either','both':", locus)
             
      #which generations have resistance greater than critical point
      gens <- which( resistances > criticalPoint)
      
      
      firstGen <- gens[1]
      #store in results
      resistGens[criticalPointNum,scenarioNum] <- firstGen
      
    }    
  }

  #replace any NAs with 999 to show that resistance not reached
  resistGens[is.na(resistGens)] <- 999
  
  #listOut$input['sexLinked',]
  
  return(resistGens)
}