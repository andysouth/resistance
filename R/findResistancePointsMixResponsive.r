#' For a responsive mixture strategy, find number of generations to reach critical points of resistance
#' 
#' Post processes model output files to find the number of generations it takes 
#' for the R allele frequency to reach passed criticalPoints.

#' @param listOutMix mixture results 
#' @param listOutI1 insecticide1 results
#' @param listOutI2 insecticide2 results
# @param locus locus of interest (1,2, 'either' or 'both')
#' @param criticalPoints the critical resistance thresholds that you want to assess


#' @examples 
#' input <- setInputSensiScenarios( nScenarios=3, h.RS1_A0=c(0,1) )
#' listOut <- runModel2(input)
#' # for all scenarios
#' findResistancePoints(listOut, locus='both')
#' # for selected scenarios
#' findResistancePoints(listOut$results[[2]], locus='both')
#' @return matrix with a column for each scenario, giving the generation number at which frequency of R is first greater than critical points
#' @export


findResistancePointsMixResponsive <- function( listOutMix,
                                               listOutI1,
                                               listOutI2,
                                               criticalPoints = c(0.1,0.25,0.5)
                                               )
{

  numScenarios <- length(listOutMix$results) 
  
  #could first create matrix to receive the results, name rows including gen_cP for generations to reach criticalPoints
  resistGens <- matrix( nrow=length(criticalPoints), ncol=numScenarios, dimnames=list(paste0('gen_cP',criticalPoints) ) )
  
  
  #time to reach critical points in mixture
#   resistPointsMixI1 <- findResistancePoints(listOutMix, locus=1)
#   resistPointsMixI2 <- findResistancePoints(listOutMix, locus=2)
#   #was it I1 or I2 that was faster ? 
#   resistPointsMixFirst <- ifelse(resistPointsMixI1 < resistPointsMixI2,1,2)
  #tricky to do next step ..
  #might be better to try this way instead ...
  
  #todo BEWARE this code is ugly and done in a rush !!
  
  
  #for each scenario (model run based on one set of parameters)
  for(scenarioNum in 1:numScenarios)
  {
    #for each critical point
    for(criticalPointNum in seq_along(criticalPoints) )
    {
      criticalPoint <- criticalPoints[criticalPointNum]
      #cat("criticalPoint ", criticalPoint,"\n")
      
      #getting results matrix out of the list
      resultsMix <- listOutMix$results[[scenarioNum]]
      resultsI1 <- listOutI1$results[[scenarioNum]]
      resultsI2 <- listOutI2$results[[scenarioNum]]
      
      #calc resistances as mean of M&F in each case
      resistancesMixI1 <- rowMeans(cbind(resultsMix[,'m.R1'],
                                         resultsMix[,'f.R1'] ))
      resistancesMixI2 <- rowMeans(cbind(resultsMix[,'m.R2'],
                                         resultsMix[,'f.R2'] ))
      resistancesI1 <-    rowMeans(cbind(resultsI1[,'m.R1'],
                                         resultsI1[,'f.R1'] ))
      resistancesI2 <-    rowMeans(cbind(resultsI2[,'m.R2'],
                                         resultsI2[,'f.R2'] ))
      
      
      #for mix find which generations have resistance greater than critical point
      #for both I1 and I2
      gensMixI1 <- which( resistancesMixI1 > criticalPoint)
      gensMixI2 <- which( resistancesMixI2 > criticalPoint)
      #first generation that resistance reached for each insecticide
      firstGenMixI1 <- gensMixI1[1]
      firstGenMixI2 <- gensMixI2[1]   
      
      #replace any NAs with 999 to show that resistance not reached
      
      if (is.na(firstGenMixI1)) firstGenMixI1 <- 999
      if (is.na(firstGenMixI2)) firstGenMixI2 <- 999
            
      cat("criticalPoint ", criticalPoint,": firstGenI1,I2",firstGenMixI1,firstGenMixI2,"\n")
      
      #check whether resistance reached first for I1 or I2
      firstR <- 1
      if (firstGenMixI2 < firstGenMixI1) firstR <- 2
      secoundR <- firstR%%2+1 #just to get the other one
      
      #then need to find the difference in generations for other insecticide
      #between the current level of R and the threshold
      firstRGens <- NULL
      if (firstR==1)
      {
        firstRGens <- firstGenMixI1
        #resistance in other insecticide
        otherRStart <- resistancesMixI2[ firstGenMixI1 ]
        #what generation would it get to that resistance when used alone ?
        otherRStartGen <- which(resistancesI2 > otherRStart)[1]
        #what generation would it get to critical when used alone
        otherRStopGen <- which(resistancesI2 > criticalPoint)[1]       
      } else
      {
        firstRGens <- firstGenMixI2
        #resistance in other insecticide
        otherRStart <- resistancesMixI1[ firstGenMixI2 ]
        #what generation would it get to that resistance when used alone ?
        otherRStartGen <- which(resistancesI1 > otherRStart)[1]
        #what generation would it get to critical when used alone
        otherRStopGen <- which(resistancesI1 > criticalPoint)[1]   
      }

      
      #difference between them
      otherRGens <- otherRStopGen - otherRStartGen
      
      #add generations to get first resistance & 2nd
      totGens <- firstRGens + otherRGens
      
      #store in results
      resistGens[criticalPointNum,scenarioNum] <- totGens
      
    }    
  }
  
  #replace any NAs with 999 to show that resistance not reached
  resistGens[is.na(resistGens)] <- 999
  
  #listOut$input['sexLinked',]
  
  return(resistGens)
}