#' wos_sim run a series of resistance evolution simulations to illustrate 'windows of selection'
#' 
#' @param concs concentration or time
#' @param mort_rr rr mortalities
#' @param mort_sr sr mortalities
#' @param mort_ss ss mortalities
#' @param exposure proportion of popn exposed to insecticide only used in the simulations
#' @param max_gen maximum generations to use in the simulations
#' @param startfreq starting frequency to use in the simulations
#' 
#' 
#' @examples 
#' wos_sim()
#' 
#' @return dataframe of simulation outputs
#' @export


wos_sim <- function( concs = c(1:5),
                     mort_rr = c(0,0,0,0.5,1),
                     mort_sr =c(0,0,0.5,1,1),
                     mort_ss = c(0,0.5,1,1,1),
                     exposure = 0.5, #only used in the simulation
                     max_gen = 1000, #used in simulations
                     startfreq = 0.001
) {
  
dfsim <- data_frame( conc = c(concs),
                     mort_rr = mort_rr,
                     mort_sr = mort_sr,
                     mort_ss = mort_ss,
                     fit_rr = 1-mort_rr,
                     fit_sr = 1-mort_sr,
                     fit_ss = 1-mort_ss,  
                     effectiveness = mort_ss,
                     resist_restor = (fit_rr-fit_ss)/effectiveness,
                     dom_resist = (fit_sr-fit_ss)/(fit_rr-fit_ss) )

#resist_restor & dom_resist are sometimes NaN
#try setting to 0
dfsim$resist_restor=ifelse(dfsim$resist_restor=='NaN',0,dfsim$resist_restor)
dfsim$dom_resist=ifelse(dfsim$dom_resist=='NaN',0,dfsim$dom_resist)


# create a default input matrix one column per scenario
# how to set default input params across all scenarios ?
# may be easiest to use sensiAnPaperPart() and then overwrite variable inputs
# NO sensiAnPaperPart() actually runs model (although  I could add an arg to stop that)
#input <- sensiAnPaperPart( nScenarios=nrow(dfsim), insecticideUsed='insecticide1')
# todo maybe put these bits into a function to make it easier to do multiple runs
#input <- matrix( ncol=nrow(dfsim), nrow=53 )
#input[5,] <- 0.001	        # P_1 locus 1 frequency of resistance allele

#oooo or seems I may be able to add an arg to setInputOneScenario() to make it do multiple scenarios
#but might want to avoid changing setInputOneScenario() because it has a trickiness about num args
#Aha! I already wrote something like this a long time ago
#This will use all defaults from setInputOneScenario()
#beware currently you have to set at least one range or it fails
#arg! can't specify args as variables 
#input <- setInputSensiScenarios( nScenarios = nrow(dfsim), max_gen=c(max_gen,max_gen) )
input <- setInputSensiScenarios( nScenarios = nrow(dfsim), P_2=c(0,0) )

#TODO I will need to change some other inputs from the defaults in setInputOneScenario()
input[2,] <- max_gen           # max_gen
input[5,] <- startfreq           # P_1    
#e.g. is exposure being done correctly ? Seems that default is 0.9
input[8:25,] <- 0
input[c(10,19),] <- exposure #mA0, fA0
input[c(8,17),] <- 1-exposure #m00, f00

#set the rows for the variable inputs from dfsim
#see setInputOneScenario() and createInputMatrix() for reference
#this done a bit weirdly because I want to set all scenarios at once rather than one by one
#effectiveness of insecticide on SS, phi.SS1_A0
input[27,] <- dfsim$effectiveness
# because the model needs selection coefficient
input[39,] <-	dfsim$resist_restor * dfsim$effectiveness
input[34,] <- dfsim$dom_resist	# h.RS1_A0 Dominance coefficient locus1 in A0


## run the model for all of the input scenarios    
listOut <- runModel2(input, produce.plots = FALSE) 

# 
resistPoints <- findResistancePoints(listOut, locus=1, criticalPoints = 0.5)
#transpose
resistPoints <- t(resistPoints)
#replace 999 which is used to indicate resistance not reached with NA
resistPoints <- ifelse(resistPoints==999,NA,resistPoints)
dfsim$time_to_resistance0.5 <- resistPoints


invisible(dfsim)
}

