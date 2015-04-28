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

 
  #uh-oh2! need to exclude nScenarios from argString if it has been passed
  
  #using match.call to set the predictors to those passed by the user
  #1 get the arg names (exclude function name at item1)
  argString <- names(match.call())[-1]
  #2 exclude nScenarios if present
  argString <- argString[argString!="nScenarios"]
  #3 collapse="+" adds a + between each arg
  argString <- paste(argString, collapse="+")
  
    
  #"h.RS1_A0+h.RS2_0B+s.RR1_A0+s.RR2_0B"

  #uh-oh1!! seems that my eval bit adds all the args together before putting them in the model.

  
  #using as.formula like this sorts uh-oh1
  tree <- rpart(as.formula(paste("res ~",argString)), data = inputAndResults, method = 'class')  
  
  #2 hardcoded args
  #tree <- rpart(res ~ h.RS1_A0 + h.RS2_0B, data = inputAndResults, method = 'class')
  #4 hardcoded args
  #tree <- rpart(res ~ h.RS1_A0 + h.RS2_0B + s.RR1_A0 + s.RR2_0B, data = inputAndResults, method = 'class')
  
  #todo look at minsplit more
  #including susanahs code to reduce branches using minsplit
  #tree <- rpart(res ~ h.RS1_A0 + h.RS2_0B + s.RR1_A0 + s.RR2_0B, data = inputAndResults, method = 'class', control=rpart.control(minsplit=50))

  #? do these work within function ?
  plot(tree) #plots tree
  text(tree) #labels tree
  
    
  #pruning tree
#   treePruned <- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
#   plot(treePruned) #plots tree
#   text(treePruned) #labels tree
    
  
  #can just separate out the data that is used in the model
  #forCT <- inputAndResults[c('h.RS1_A0','h.RS2_0B','res')]
  #cfit <- rpart(res ~ h.RS1_A0 + h.RS2_0B, data = forCT, method = 'class')

  
  #may want to return inputAndResults from this func
  invisible(inputAndResults)
  #and then return tree or treePruned from another func
  #invisible(tree)
  #invisible(treePruned)
  #BUT then also would need to return arglist somehow
  
}