exposed=0.5# The proportion exposed
R_freq=0.01; #frequency of the resistance allele. this will affect results if resistance is recessive beacuse it determines the proportion of R allelels in RR genotypes
fitness_RR=0.5
fitness_RS=0
fitness_SS=0 

R_next_gen=exposed*(R_freq*R_freq*fitness_RR+2*R_freq*(1-R_freq)*0.5*fitness_RS)+
  (1-exposed)*(R_freq*R_freq+2*R_freq*(1-R_freq)*0.5);

S_next_gen=exposed*(2*R_freq*(1-R_freq)*0.5*fitness_RS+(1-R_freq)*(1-R_freq)*fitness_SS)+
  (1-exposed)*(2*R_freq*(1-R_freq)*0.5+(1-R_freq)*(1-R_freq));

R_freq_next_gen=R_next_gen/(R_next_gen+S_next_gen)

selective_advantage=(R_freq_next_gen/R_freq)-1 #so if no increase, advantage=0