#' wos_sim run a series of resistance evolution simulations to illustrate 'windows of selection'
#' 
#' requires either a dataframe with columns for genotype, x (e.g. time) and y (e.g.mortality)
#' or vectors of x, and yrr,ysr,yss
#' 
#' @param dfmortbygen dataframe of mortality by genotype, an alternative way of initialising function, needs genotype column
#' @param x column name of x in dfmortbygen
#' @param y column name of y in dfmortbygen 
#' @param concs concentration or time
#' @param mort_rr rr mortalities 0-1
#' @param mort_sr sr mortalities 0-1
#' @param mort_ss ss mortalities 0-1
#' @param exposure proportion of popn exposed to insecticide only used in the simulations
#' @param max_gen maximum generations to use in the simulations
#' @param no_thresh what to return as time-to-resistance if threshold not reached
#' @param startfreq starting frequency to use in the simulations
#' @param dominances only if there are no SR values which dominances to run
#' @param plot whether to plot
#' 
#' @examples 
#' dfsim <- wos_sim()
#' dfsim <- wos_sim(dfmortbygen=data_frame(conc=rep(c(1,2),3),
#'                                genotype=rep(c('RR','SR','SS'),each=2),
#'                                mort=c(0,0.2,0.4,0.6,0.8,1)),
#'         x='conc',y='mort')
#' #with no sr data runs dom0,0.1&1         
#' dfsim <- wos_sim(dfmortbygen=data_frame(conc=rep(c(1,2),2),
#'                                genotype=rep(c('RR','SS'),each=2),
#'                                mort=c(0,0.2,0.8,1)),
#'         x='conc',y='mort')         
#' 
#' @return dataframe of simulation outputs
#' @export


wos_sim <- function( dfmortbygen = NULL,
                     x = 'concentration',
                     y = 'mortality',
                     concs = c(1:5),
                     mort_rr = c(0,0,0,0.5,1),
                     mort_sr =c(0,0,0.5,1,1),
                     mort_ss = c(0,0.5,1,1,1),
                     exposure = 0.5, 
                     max_gen = 1000,
                     no_thresh = max_gen,
                     startfreq = 0.001,
                     dominances = c(0,0.1,1),
                     plot = TRUE

) {
 
# if the input data are provided in a dataframe   
if (!is.null(dfmortbygen))
{
  #removes blank rows and comments
  #dfmortbygen <- dfmortbygen[ !is.na(dfmortbygen$genotype), ]
  #!!(enquo(x))
  #!! unquotes a variable name
  dfmortbygen <- filter(dfmortbygen, !is.na(!!x))
  
  #assumes that concs are the same and repeatd for rr,sr,ss
  concs <- filter(dfmortbygen,genotype=='RR') %>% select(!!x) %>% unlist(use.names=FALSE) 
  mort_rr <- filter(dfmortbygen,genotype=='RR') %>% select(!!y) %>% unlist(use.names=FALSE)
  # if no sr data
  if (nrow(filter(dfmortbygen,genotype=='SR'))==0)
  { 
    mort_sr <- NULL 
  } else
  {
    mort_sr <- filter(dfmortbygen,genotype=='SR') %>% select(!!y) %>% unlist(use.names=FALSE)    
  }
  mort_ss <- filter(dfmortbygen,genotype=='SS') %>% select(!!y) %>% unlist(use.names=FALSE)
  
  #testing this assumes that same numvars for rr,sr,ss (or sr=NULL)
  if(! (length(mort_rr)==length(mort_ss) & 
        (is.null(mort_sr) | length(mort_sr) == length(mort_ss)))) stop('need same num rr,sr,ss')
}

if(any(mort_rr>1) | any(mort_sr>1) | any(mort_rr>1)) 
{
  warning("mortality values should be between 0 & 1, truncating at 1")
  mort_rr <- ifelse(mort_rr>1,1,mort_rr)
  mort_sr <- ifelse(mort_sr>1,1,mort_sr)
  mort_ss <- ifelse(mort_ss>1,1,mort_ss)
}

  
# if there are no heterozygote data, then run simulations for dominance 0&1
# expected result must be between these extremes
# can do that by just replicating dfsim, one version with dom0 the other dom1
no_sr <- FALSE  
if(is.null(mort_sr)){
  no_sr <- TRUE
  mort_sr <- mort_rr #setting mort_sr to mort_rr makes dominance 1
}  
  
dfsim <- data_frame( conc = c(concs),
                     mort_rr = mort_rr,
                     mort_sr = mort_sr,
                     mort_ss = mort_ss,
                     fit_rr = 1-mort_rr,
                     fit_sr = 1-mort_sr,
                     fit_ss = 1-mort_ss,  
                     effectiveness = mort_ss,
                     resist_restor = (fit_rr-fit_ss)/effectiveness,
                     dom_resist = (fit_sr-fit_ss)/(fit_rr-fit_ss),
                     start_frequency = startfreq )

#if no_sr create a copy, set dominance to 0 and bind back on
#will prompt simulation to be run for dominance 1 and 0
if(no_sr){
  #record the dominance1 version
  dfsim1 <- dfsim
  #for(dom in c(0,0.1))
  for(dom in dominances[which(dominances!=1)])    
  {
    #beware this is a bit tricky
    #copy from the ver with just dominance1
    dfsim2 <- dfsim1
    dfsim2$dom_resist <- dom
    #just so other inputs are consistent
    dfsim2$mort_sr <- dfsim2$mort_ss + dom*(dfsim2$mort_rr-dfsim2$mort_ss)
    dfsim2$fit_sr <- 1-dfsim2$mort_sr
    #bind back onto incremental version
    dfsim <- rbind(dfsim, dfsim2)    
  }
  #just running 0&1
  # dfsim2 <- dfsim
  # dfsim2$dom_resist <- 0
  # #just so other inputs are consistent
  # dfsim2$mort_sr <- dfsim2$mort_ss
  # dfsim2$fit_sr <- 1-dfsim2$mort_sr  
  # dfsim <- rbind(dfsim, dfsim2)
} 

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
#resistPoints <- ifelse(resistPoints==999,NA,resistPoints)
# now a passed arg defaulting to max_gen alows other things to be passed
resistPoints <- ifelse(resistPoints==999, no_thresh, resistPoints)

dfsim$time_to_resistance0.5 <- resistPoints

#just uses defaults other options are available by calling func direct
if (plot) wos_plot_sim(dfsim)

invisible(dfsim)
}

