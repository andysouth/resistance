#' wos_advantage calculate selective advantage through 'window of selection'
#' 
#' from mortalities of rr sr ss
#' 
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


wos_advantage <- function( concs = c(1:5),
                     mort_rr = c(0,0,0,0.5,1),
                     mort_sr =c(0,0,0.5,1,1),
                     mort_ss = c(0,0.5,1,1,1),
                     exposure = 0.5, #only used in the simulation
                     #max_gen = 1000, #used in simulations
                     startfreq = 0.001
) {
  
  freq <- startfreq
  
  dfsim <- data_frame( conc = c(concs),
                       freq = freq, #just put in here to make dplyr easier below
                       mort_rr = mort_rr,
                       mort_sr = mort_sr,
                       mort_ss = mort_ss,
                       fit_rr = 1-mort_rr,
                       fit_sr = 1-mort_sr,
                       fit_ss = 1-mort_ss )
 
  dfsim <- dfsim %>% 
    mutate(r_next_gen = exposure*(freq^2*fit_rr + 2*freq*(1-freq)*0.5*fit_sr) +
                        (1-exposure)*(freq^2+2*freq*(1-freq)*0.5)) %>%
  
    mutate(s_next_gen = exposure*(2*freq*(1-freq)*0.5*fit_sr +
                        (1-freq)^2 * fit_ss) +
                        (1-exposure)*(2*freq*(1-freq)*0.5+(1-freq)*(1-freq))) %>%
    
    # S_next_gen=exposed*(2*R_freq*(1-R_freq)*0.5*fitness_RS+(1-R_freq)*(1-R_freq)*fitness_SS)+
    #   (1-exposed)*(2*R_freq*(1-R_freq)*0.5+(1-R_freq)*(1-R_freq));
    
             
    mutate(freq_next_gen = r_next_gen/(r_next_gen+s_next_gen)) %>%
             
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
   
  invisible(dfsim)
}  
  