#' wos_advantage calculate selective advantage through 'window of selection'
#' 
#' Similar to wos_sim() which simulates time-to-resistance. 
#' Requires either a dataframe with columns for genotype, x (e.g. time) and y (e.g.mortality)
#' or vectors of x, and yrr,ysr,yss. 
#' 
#' @param dfmortbygen dataframe of mortality by genotype, an alternative way of initialising function, needs genotype column
#' @param x column name of x in dfmortbygen
#' @param y column name of y in dfmortbygen 
#' @param concs concentration or time
#' @param mort_rr rr mortalities
#' @param mort_sr sr mortalities
#' @param mort_ss ss mortalities
#' @param exposure proportion of popn exposed to insecticide 
# @param max_gen maximum generations to use in the simulations
#' @param startfreq starting frequency to use in the simulations
#' 
#' @import dplyr
#' 
#' @examples 
#' wos_advantage()
#' 
#' @return dataframe of simulation outputs
#' @export


wos_advantage <- function( dfmortbygen = NULL,
                           x = 'concentration',
                           y = 'mortality',
                           concs = c(1:5),
                     mort_rr = c(0,0,0,0.5,1),
                     mort_sr =c(0,0,0.5,1,1),
                     mort_ss = c(0,0.5,1,1,1),
                     exposure = 0.5, #only used in the simulation
                     #max_gen = 1000, #used in simulations
                     startfreq = 0.001
) {
  
  #%% to this mark are copied from wos_sim() as temp. measure I must be able to rationalise ...
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
  #%%
  freq <- startfreq
  
  dfsim <- data_frame( conc = c(concs),
                       freq = freq, #just put in here to make dplyr easier below
                       mort_rr = mort_rr,
                       mort_sr = mort_sr,
                       mort_ss = mort_ss,
                       fit_rr = 1-mort_rr,
                       fit_sr = 1-mort_sr,
                       fit_ss = 1-mort_ss )
  
  #if no_sr create a copy, set dominance to 0 and bind back on
  #will prompt simulation to be run for dominance 1 and 0
  if(no_sr){
    #record the dominance1 version
    dfsim1 <- dfsim
    #running 0,0.5 and 1
    for(dom in c(0,0.1,0.5))
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
  
  
  dfsim <- dfsim %>% 
    mutate(r_next_gen = exposure*(freq^2*fit_rr + 2*freq*(1-freq)*0.5*fit_sr) +
                        (1-exposure)*(freq^2+2*freq*(1-freq)*0.5)) %>%
  
    mutate(s_next_gen = exposure*(2*freq*(1-freq)*0.5*fit_sr +
                        (1-freq)^2 * fit_ss) +
                        (1-exposure)*(2*freq*(1-freq)*0.5+(1-freq)*(1-freq))) %>%
    
    # S_next_gen=exposed*(2*R_freq*(1-R_freq)*0.5*fitness_RS+(1-R_freq)*(1-R_freq)*fitness_SS)+
    #   (1-exposed)*(2*R_freq*(1-R_freq)*0.5+(1-R_freq)*(1-R_freq));
    
             
    mutate(freq_next_gen = r_next_gen/(r_next_gen+s_next_gen)) %>%
    
    mutate(relative_fitness=(freq_next_gen/freq)) %>%
             
    mutate(selective_advantage=(freq_next_gen/freq)-1) #so if no increase, advantage=0         

  
  # exposed=0.5# The proportion exposed
  # R_freq=0.001; #frequency of the resistance allele. this will affect results if resistance is recessive beacuse it determines the proportion of R allelels in RR genotypes
  # fitness_RR=1.0
  # fitness_RS=0.9
  # fitness_SS=0.8
  #     
  # R_next_gen=exposed*(R_freq*R_freq*fitness_RR+2*R_freq*(1-R_freq)*0.5*fitness_RS)+
  #   (1-exposed)*(R_freq*R_freq+2*R_freq*(1-R_freq)*0.5);
  # 
  # S_next_gen=exposed*(2*R_freq*(1-R_freq)*0.5*fitness_RS+(1-R_freq)*(1-R_freq)*fitness_SS)+
  #   (1-exposed)*(2*R_freq*(1-R_freq)*0.5+(1-R_freq)*(1-R_freq));
  # 
  # R_freq_next_gen=R_next_gen/(R_next_gen+S_next_gen)
  # 
  # selective_advantage=(R_freq_next_gen/R_freq)-1 #so if no increase, advantage=0
   
  #just aids with labelling in later plots
  dfsim$start_frequency <- dfsim$freq
  
  invisible(dfsim)
}  
  