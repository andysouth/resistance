#' Find number of generations to reach critical points of resistance for rotation strategy
#' 
#' Post processes model output files to find the number of generations it takes 
#' for the R allele frequency to reach passed criticalPoints.
#' Returns NA if freq does not reach 

#' @param rotation_generations is the number of generations an insecticide will be deployed before changing insecticides
#' @param listOut results from a set of scenario runs (or a single results matrix)
#' @param locus locus of interest (1,2 or 'both')
#' @param criticalPoints the critical resistance thresholds that you want to assess


#' @examples 
#' input <- setInputSensiScenarios( nScenarios=3, h.RS1_A0=c(0,1) )
#' listOut <- runModel2(input)
#' findResistancePoints(listOut$results, locus='both')
#' @return matrix with a column for each scenario, giving the generation number at which frequency of R is first greater than critical points
#' @export



findResistancePointsRotation <- function( rotation_generations = 10,
                                          listOutI1,
                                          listOutI2,
                                          locus,
                                          criticalPoints = c(0.1,0.25,0.5)
){
  
  #set the number of scenarios
  numScenarios <- length(listOutI1$results) 
  
  #create matrix to receive the results, name rows including gen_cP for generations to reach criticalPoints.
  resistGens <- matrix( nrow=length(criticalPoints), ncol=numScenarios, dimnames=list(paste0('gen_cP',criticalPoints) ) )
  
  #for each scenario
  for(scenarioNum in 1:numScenarios)
  {
    #for each critical point
    for(criticalPointNum in seq_along(criticalPoints) )
    {
      criticalPoint <- criticalPoints[criticalPointNum]
      #cat("criticalPoint ", criticalPoint,"\n")
      
      #getting results matrix out of the list for the separate insecticides
      resultsI1 <- listOutI1$results[[scenarioNum]]
      resultsI2 <- listOutI2$results[[scenarioNum]]
      
      
      #extracting the resistances for insecticides 1 and 2
      resistancesI1 <-    rowMeans(cbind(resultsI1[,'m.R1'],
                                         resultsI1[,'f.R1'] ))
      resistancesI2 <-    rowMeans(cbind(resultsI2[,'m.R2'],
                                         resultsI2[,'f.R2'] ))
      
      #Isolating generations where resistance hasn't been met for I1 and I2.
      gensI1 <- which( resistancesI1 < criticalPoint)
      gensI2 <- which( resistancesI2 < criticalPoint)
      
      #Connecting first detectable resistance generations.
      gensI1=c(gensI1, max(gensI1)+1)
      gensI2=c(gensI2, max(gensI2)+1)
      
      #Calculate total number of generations until resistance for rotation.
      overallgens = length(gensI1) + length(gensI2)
      
      #The following code calculates the final number of generations before resistance for each insecticide individually.
      
      roundsI1=(length(gensI1)%/%rotation_generations)
      roundsI2=(length(gensI2)%/%rotation_generations)
      
      leftover=(length(gensI2)%%rotation_generations)
      
      
      if (roundsI1 > roundsI2){
        
        
        
        I1gen=overallgens
        
        if (leftover==0){
          
          I2gen=(2*roundsI2)*rotation_generations + leftover
        }
        
        if (leftover!=0){
          
          I2gen=(2*roundsI2 + 1)*rotation_generations + leftover
        }
        
      }
      
      else 
      {
        I2gen=overallgens
        
        I1gen=(2*(roundsI1)*rotation_generations + leftover)
        
      }   
      
      
      
      #Calculate total number of generations until resistance for rotation.
      if (locus == "both")
      {
        if (I1gen>I2gen){
          resistGens[criticalPointNum,scenarioNum] <- I1gen
        }
        
        if (I1gen<I2gen){
          resistGens[criticalPointNum,scenarioNum] <- I2gen
        }
        
      }
      
      #Report number of generations for insecticide 1 and insecticide 2.
      else if (locus ==1)
      {
        resistGens[criticalPointNum,scenarioNum] <- I1gen
        
      }
      
      else if (locus == 2)
      {
        resistGens[criticalPointNum,scenarioNum] <- I2gen
        
      }
      
      
      
    }
  }
  
  #replace any NAs with 999 to show that resistance not reached
  resistGens[is.na(resistGens)] <- 999
  
  
  return(resistGens)
  
}

