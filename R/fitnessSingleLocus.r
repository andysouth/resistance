#' calculate single locus fitness for 2 loci from insecticide exposure
#' 
#' can be used in 2 ways
#' 1) by passing arrays a_dom etc. as done from runModel2()
#' 2) by passing separate inputs (e.g. eff1) for a single niche for testing & vis

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
#' @param plot whether to plot fitness
#' 
#' @examples 
#' fitnessSingleLocus()
#' fitnessSingleLocus(eff1 = 0.8)

#' @return fitness values
#' @export

fitnessSingleLocus <- function ( eff1 = 0.5,
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
                                 plot = FALSE)
{
  
  if ( is.null(a_fitloc) )
  {
    a_fitloc   <- array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    #set from input file in runModel2()
    a_fitloc['SS1','no'] <- fitSS1 #input[30,i]
    a_fitloc['SS2','no'] <- fitSS2 #input[31,i] 
  }

  if ( is.null(a_dom) )  
  {
    # dominance coefficient
    a_dom       <- array_named(locusNum=c(1,2), exposure=c('no','lo','hi')) 
    a_dom[1, 'hi'] <- dom1
    a_dom[2, 'hi'] <- dom2
  }
  if ( is.null(a_sel) )  
  {
    # selection coefficient
    a_sel       <- array_named(locusNum=c(1,2), exposure=c('no','lo','hi') ) #or just lo hi
    a_sel[1, 'hi'] <- rr_1 * eff1
    a_sel[2, 'hi'] <- rr_2 * eff2    
  }  
  if ( is.null(a_effect) )  
  {
    # fitness of one locus (baseline), effectiveness
    a_effect     <- array_named(locusNum=c(1,2), exposure=c('no','lo','hi'))
    a_effect[1, 'hi'] <- eff1
    a_effect[2, 'hi'] <- eff2    
  }  
  if ( is.null(a_cost) )  
  {
    # fitness cost of resistance allele in no insecticide
    a_cost       <- array_named(locusNum=c(1,2))
    a_cost[1] <- cost1
    a_cost[2] <- cost2
  } 
 
  #testing
  #cat('effectiveness\n')
  #print(a_effect)
  
  
  for( locusNum in 1:2 ) #todo improve 1:2 get it from somewhere
  {
    #exposure 0 'no'
    a_fitloc[ paste0('RS',locusNum), 'no'] <- 1 - (a_dom[locusNum, 'no'] * a_cost[locusNum])
    a_fitloc[ paste0('RR',locusNum), 'no'] <- 1 - a_cost[locusNum]
    
    for( exposID in c('lo','hi') )
    {
      a_fitloc[ paste0('SS',locusNum), exposID] <-  1 - a_effect[locusNum, exposID] 
      
      a_fitloc[ paste0('RS',locusNum), exposID] <- (1 - a_effect[locusNum, exposID]) + 
        (a_dom[locusNum, exposID] * a_sel[locusNum, exposID])
      
      a_fitloc[ paste0('RR',locusNum), exposID] <- (1 - a_effect[locusNum, exposID]) + 
        (a_sel[locusNum, exposID])
    }
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any( a_fitloc > 1  ) ) 
    warning( sum(a_fitloc > 1 ), " locus fitness values (a_fitloc) are >1 : ", a_fitloc[a_fitloc>1])
  if ( any( a_fitloc < 0 ) ) 
    warning( sum( a_fitloc < 0 ), " locus fitness values (a_fitloc) are <0")     
  
  if (plot)
  {
    df_fit1 <- as.data.frame(a_fitloc)
    #temp adding an extra column for faceting
    df_fit1$locus <- paste('locus', c(1,1,1,2,2,2))

    plot_fit_rs(df_fit1, 'hi', column_facet = 'locus')
  }
  
  return(a_fitloc)
}