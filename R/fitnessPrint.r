#' to print out fitness values based on inputs
#' 
#' for model testing & understanding

#' @param effectiveness1 effectiveness
#' @param effectiveness2 effectiveness
#' @param dominance dominance
#' @param selection_co selection_coefficient
#' @param cost fitness cost of R in no insecticide
#' @param SS fitness of SS if no insecticide
#' @param h dominance array
#' @param s selection coefficient array
#' @param phi effectiveness array
#' @param z cost array
#' @param Wloci array of single locus fitnesses to fill
#' @param insecticideUsed one of 'insecticide1','insecticide2','mixture'
#' 
#' @examples 
#' fitnessPrint()
#' fitnessPrint(effectiveness1=0.8, effectiveness2=0.8, exposure=0.5, insecticideUsed='insecticide1')
#' fitnessPrint(effectiveness1=0.8, effectiveness2=0.8, exposure=0.5, insecticideUsed='insecticide2')
#' fitnessPrint(effectiveness1=0.8, effectiveness2=0.8, exposure=0.5, insecticideUsed='mixture')

#' @return fitness values
#' @export

fitnessPrint <- function ( effectiveness1 = 0.5,
                           effectiveness2 = 0.5,
                                 dominance = 0.5,
                                 selection_co = 0.5,
                                 cost = 0,
                                 SS = 1,
                                 h = NULL,
                                 s = NULL,
                                 phi = NULL,
                                 z = NULL,
                                 Wloci = NULL,
                                 exposure = 0.5,
                                 insecticideUsed = 'mixture')
{
  
  # do the fitness calculations
  Wloci <- fitnessSingleLocus( effectiveness1 = effectiveness1,
                               effectiveness2 = effectiveness2,
                               dominance = dominance,
                               selection_co = selection_co,
                               cost = cost,
                               SS = SS,
                               h = h,
                               s = s,
                               phi = phi,
                               z = z,
                               Wloci = Wloci )
  
  Wniche <- fitnessNiche( Wloci = Wloci )
  
  a <- setExposure( exposure = exposure, insecticideUsed = insecticideUsed )
  
  Windiv <- fitnessIndiv( Wniche = Wniche, a = a )
  
  # now trying to print the results in a useful way
  
  #as.data.frame does what I want on Windiv because it has 3 dimensions, but not on Wniche[,,'A','B'] because it just has 2  
  
  #this is a hack but does what I want
  #i add the 0 dimension that I don't want, then have to aperm to get output in same format as Windiv
  df_niche <- as.data.frame( aperm( Wniche[,,c('A'),c('B','0')], c('niche2','locus1','locus2')) )
  #SS2.SS1 RS2.SS1 RR2.SS1 SS2.RS1 RS2.RS1 RR2.RS1 SS2.RR1 RS2.RR1 RR2.RR1
  #B     0.5     0.5     0.5    0.75    0.75    0.75       1       1       1
  #0     0.0     0.0     0.0    0.00    0.00    0.00       0       0       0 
  
  rownames(df_niche)[1] <- 'niche'
  
  #single locus fitnesses
  #print(Wloci)
  
  print(df_niche[1,])
  
  # i could make it option to print exposure here
  # [1,] just prints males
  print(as.data.frame(a)[1,]) #exposure
  
  
  df_indiv <- as.data.frame(Windiv)
  
  #SS1.SS2 RS1.SS2 RR1.SS2 SS1.RS2 RS1.RS2 RR1.RS2 SS1.RR2 RS1.RR2 RR1.RR2
  #m     0.6   0.725    0.85     0.6   0.725    0.85     0.6   0.725    0.85
  #f     0.6   0.725    0.85     0.6   0.725    0.85     0.6   0.725    0.85  
  
  rownames(df_indiv) <- paste0('ind_', rownames(df_indiv))
  
  print(df_indiv[1,]) #just m
  #print(df_indiv) #m&f
  

  
}