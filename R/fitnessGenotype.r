#' calculate individual fitness based on niche fitness and exposure
#' 
#' allows calling from runModel2() and independently 

#' @param a_fitnic array of niche fitnesses
#' @param a_expos array of exposure in each niche
#' @param a_fitgen array of individual fitnesses to fill
#' @param eff1 effectiveness1
#' @param eff2 effectiveness2
#' @param dom1 dominance1
#' @param dom2 dominance2
#' @param rr_1 resistance restoration 1 selection coefficient = resistance restoration * effectiveness
#' @param rr_2 resistance restoration 2
#' @param cost1 fitness cost of R in no insecticide
#' @param cost2 fitness cost of R in no insecticide
#' @param fitSS1 fitness of SS1 if no insecticide
#' @param fitSS2 fitness of SS2 if no insecticide
#' @param exposure exposure if set the same for both 
#' @param exp1 option to set exposure differently for the diff insecticides
#' @param exp2 option to set exposure differently for the diff insecticides 
#' @param plot whether to plot fitness results
#' 
#' @examples 
#' fitnessGenotype()
#' 
#' a_fitloc <- fitnessSingleLocus( eff1 = 0.8, eff2 = 0.8 )
#' a_fitnic <- fitnessNiche( a_fitloc = a_fitloc )
#' a_expos <- setExposure( exposure=0.5, insecticideUsed = 'mixture' )
#' a_fitgen <- fitnessGenotype( a_fitnic = a_fitnic, a_expos = a_expos )

#' @return fitness values in an array
#' @export

fitnessGenotype <- function ( a_fitnic = NULL,
                              a_expos = NULL,
                              a_fitgen = NULL,
                              eff1 = 0.5,
                              eff2 = 0.5,
                              dom1 = 0.5,
                              dom2 = 0.5,
                              rr_1 = 0.5,
                              rr_2 = 0.5,
                              cost1 = 0,
                              cost2 = 0,
                              fitSS1 = 1,
                              fitSS2 = 1,
                              exposure = 0.5,
                              exp1 = NULL,
                              exp2 = NULL,
                              plot = FALSE )
{
  
  # to allow this function to be called with no args
  if ( is.null(a_fitnic) )
  {
    a_fitloc   <- fitnessSingleLocus(eff1 = eff1,
                                     eff2 = eff2,
                                     dom1 = dom1,
                                     dom2 = dom2,
                                     rr_1 = rr_1,
                                     rr_2 = rr_2,
                                     cost1 = cost1,
                                     cost2 = cost2,
                                     fitSS1 = fitSS1,
                                     fitSS2 = fitSS2)
    
    a_fitnic   <- fitnessNiche( a_fitloc = a_fitloc)
  }  

  if ( is.null(a_expos) )
  {
    a_expos <- setExposure(exposure=exposure, exp1=exp1, exp2=exp2)
    #a_expos <- setExposure( exposure=0.9, insecticideUsed = 'mixture' )
  }
  
  if ( is.null(a_fitgen) )
  {
    # empty array to fill
    a_fitgen  <- array_named( sex=c('m','f'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
  }

  #testing
  # cat("in fitnessGenotype\n")
  # df_niche <- as.data.frame( aperm( a_fitnic[,,c('A'),c('B','0')], c('niche2','locus1','locus2')) )
  # rownames(df_niche)[1] <- 'niche'
  # print(df_niche[1,])
  # print(as.data.frame(a_expos)[1,]) #exposure
    
  
  for( sex in dimnames(a_fitgen)$sex)
  {
    for( locus1 in dimnames(a_fitgen)$locus1)
    {
      for( locus2 in dimnames(a_fitgen)$locus2)
      {
        # multiplies exposure by fitness for all niches & then sums
        # creates a weighted average of exposure in each niche
        a_fitgen[sex,locus1,locus2] <- sum( a_expos[sex,,] * a_fitnic[locus1,locus2,,])
      }
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any(a_fitgen > 1 ) ) 
    warning( sum(a_fitgen > 1  ), " individual fitness values (a_fitloc) are >1")
  if ( any( a_fitgen < 0 ) ) 
    warning( sum( a_fitgen < 0 ), " individual fitness values (a_fitloc) are <0")
  
  #testing
  #cat("in fitnessGenotype\n")
  #df_indiv <- as.data.frame(a_fitgen)
  #print(df_indiv[1,]) #just m
  
  if (plot)
  {
    #transpose to get in useable format
    df_fit3 <- as.data.frame(t(as.data.frame(a_fitgen)))
    # I could melt & then facet m&f
    # melt(df_fit3)
    plot_fit_rs(df_fit3,'f',title='genotype dependent on exposure')
  }
  
  return(a_fitgen)
}