#' to print out fitness values based on inputs
#' 
#' for model testing & understanding

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
#' @param a_dom dominance array
#' @param a_sel selection coefficient array
#' @param a_effect effectiveness array
#' @param a_cost cost array
#' @param a_fitloc array of single locus fitnesses to fill
#' @param exposure exposure
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' 
#' @examples 
#' fitnessPrint()
#' fitnessPrint(eff1=0.8, eff2=0.8, exposure=0.5, insecticideUsed='insecticide1')
#' fitnessPrint(eff1=0.8, eff2=0.8, exposure=0.5, insecticideUsed='insecticide2')
#' fitnessPrint(eff1=0.8, eff2=0.8, exposure=0.5, insecticideUsed='mixture')

#' @return fitness values
#' @export

fitnessPrint <- function ( eff1 = 0.5,
                           eff2 = 0.5,
                           dom1 = 0.5,
                           dom2 = 0.5,
                           rr_1 = 0.5,
                           rr_2 = 0.5,
                           cost1 = 0,
                           cost2 = 0,
                           fitSS1 = 1,
                           fitSS2 = 1,
                           a_dom = NULL,
                           a_sel = NULL,
                           a_effect = NULL,
                           a_cost = NULL,
                           a_fitloc = NULL,
                           exposure = 0.5,
                           insecticideUsed = 'mixture')
{
  
  # do the fitness calculations
  a_fitloc <- fitnessSingleLocus( eff1 = eff1,
                               eff2 = eff2,
                               dom1 = dom1,
                               dom2 = dom2,                               
                               rr_1 = rr_1,
                               rr_2 = rr_2,
                               cost1 = cost1,
                               cost2 = cost2,
                               fitSS1 = fitSS1,
                               fitSS2 = fitSS2,
                               a_dom = a_dom,
                               a_sel = a_sel,
                               a_effect = a_effect,
                               a_cost = a_cost,
                               a_fitloc = a_fitloc )
  
  a_fitnic <- fitnessNiche( a_fitloc = a_fitloc )
  
  a_expos <- setExposure( exposure = exposure, insecticideUsed = insecticideUsed )
  
  a_fitgen <- fitnessGenotype( a_fitnic = a_fitnic, a_expos = a_expos )
  
  # now trying to print the results in a useful way
  
  #as.data.frame does what I want on a_fitgen because it has 3 dimensions, but not on a_fitnic[,,'A','B'] because it just has 2  
  
  #this is a hack but does what I want
  #i add the 0 dimension that I don't want, then have to aperm to get output in same format as a_fitgen
  df_niche <- as.data.frame( aperm( a_fitnic[,,c('A'),c('B','0')], c('niche2','locus1','locus2')) )
  #SS2.SS1 RS2.SS1 RR2.SS1 SS2.RS1 RS2.RS1 RR2.RS1 SS2.RR1 RS2.RR1 RR2.RR1
  #B     0.5     0.5     0.5    0.75    0.75    0.75       1       1       1
  #0     0.0     0.0     0.0    0.00    0.00    0.00       0       0       0 
  
  rownames(df_niche)[1] <- 'niche'
  
  #single locus fitnesses
  #print(a_fitloc)
  
  print(df_niche[1,])
  
  # i could make it option to print exposure here
  # [1,] just prints males
  print(as.data.frame(a_expos)[1,]) #exposure
  
  
  df_indiv <- as.data.frame(a_fitgen)
  
  #SS1.SS2 RS1.SS2 RR1.SS2 SS1.RS2 RS1.RS2 RR1.RS2 SS1.RR2 RS1.RR2 RR1.RR2
  #m     0.6   0.725    0.85     0.6   0.725    0.85     0.6   0.725    0.85
  #f     0.6   0.725    0.85     0.6   0.725    0.85     0.6   0.725    0.85  
  
  rownames(df_indiv) <- paste0('ind_', rownames(df_indiv))
  
  print(df_indiv[1,]) #just m
  #print(df_indiv) #m&f
  

  
}