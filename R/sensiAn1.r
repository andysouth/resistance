#' sensitivity analysis first attempt
#' 
#' 
#' @param nScenarios number of scenarios 
#' @param ... parameter ranges to pass to \code{\link{setInputSensiScenarios}} in form c(min,max)
#' 
#' @examples
#' sensiAn1( P_1=c(0,0.5) )
#' inputAndResults <- sensiAn1( h.RS1_A0=c(0.1,1))
#' colnames(inputAndResults)
#' inputAndResults <- sensiAn1( h.RS1_A0=c(0.1,1), h.RS2_0B=c(0.1,1) )
#' inputAndResults <- sensiAn1(500, h.RS1_A0=c(0.1,1), h.RS2_0B=c(0.1,1), s.RR1_A0=c(0.2,1), s.RR2_0B=c(0.2,1))
#' @return todo
#' @export
#' 
sensiAn1 <- function( nScenarios = 10,
                      ... )
{

  input <- setInputSensiScenarios( nScenarios = nScenarios, ... )
  #input <- setInputSensiScenarios( nScenarios = nScenarios)  

  #run the model using input which sets num scenarios
  #suppress plotting to speed
  listOut <- runModel(input, produce.plots = FALSE)
  
  #plot all scenario results on top of each other
  plotallele.freq.andy(listOut)
  
  #extract timetoFifty from listOUt
  #todo replace with my own func
  #note this is currently locus1
  ttf <- timetoFifty(locus=1, max_gen=100, listOut$results, input)
  
  #paste on to input
  inputAndResults <- rbind(input,ttf)
  
  #transpose input, add results column extracted from listOut
  inputAndResults <- t(inputAndResults)
  #convert to a dataframe
  inputAndResults <- data.frame(inputAndResults)
  
  #try a classification tree
  #but may need to simplify outcome to 0/1 ??
  #this from rpart vignette
  #library(rpart)
  #creates a factor for response var to improve labelling
  #outcome <- factor(inputAndResults$Gens.to.0.5, levels = 0:1, labels = c("No", "Prog"))
  #NAs are where 0.5 resistance not reached, convert them to 99 for now
  inputAndResults$Gens.to.0.5[is.na(inputAndResults$Gens.to.0.5)] <- 999
  inputAndResults$res <- cut(inputAndResults$Gens.to.0.5, breaks=c(0,50,9999),labels=c("<=50",">50"))
  #fit the model by adding the columns
  #**me can I specify the predictors by names(match.call())[-1]**
  #cfit <- rpart(res ~ age + eet + g2 + grade + gleason + ploidy, data = inputAndResults, method = 'class')
  
  #cfit <- rpart(res ~ h.RS1_A0 + h.RS2_0B, data = inputAndResults, method = 'class')
  
  tree <- rpart(res ~ h.RS1_A0 + h.RS2_0B + s.RR1_A0 + s.RR2_0B, data = inputAndResults, method = 'class')
  
  #including susanahs code to reduce branches using minsplit
  tree <- rpart(res ~ h.RS1_A0 + h.RS2_0B + s.RR1_A0 + s.RR2_0B, data = inputAndResults, method = 'class', control=rpart.control(minsplit=50))
  
  #pruning tree
  treePruned <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  plot(treePruned) #plots tree
  text(treePruned) #labels tree
    
  
  #can I use match.call to simplify setting the predictors to those passed by the user
  #cfit <- rpart(res ~ names(match.call())[-1], data = inputAndResults, method = 'class')
  
  #can just separate out the data that is used in the model
  #forCT <- inputAndResults[c('h.RS1_A0','h.RS2_0B','res')]
  #cfit <- rpart(res ~ h.RS1_A0 + h.RS2_0B, data = forCT, method = 'class')

  
  plot(tree) #plots tree
  text(tree) #labels tree

  
  #may want to return inputAndResults from this func
  invisible(inputAndResults)
  #and then return tree or treePruned from another func
  #invisible(tree)
  #invisible(treePruned)
  
}