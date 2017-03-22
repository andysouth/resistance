#' selection of genotypes based upon their fitnesses
#' 
#'   
#' 
#' @param a_gtypes array with frequencies of genotypes in the popn.
#' @param a_fitgen array of individual fitnesses by genotype 
#' @param calibration flag if set to 103 turns off selection
#' 
#' @examples 
#' #selection() 
#' @return array of post-selection genotype frequencies
#' @export



selection <- function( a_gtypes, a_fitgen, calibration)
{

  # copy after selection frequencies from before
  # to initialise array
  a_gtypes_s <- a_gtypes
  
  if(calibration!=103){		## no selection calibration
    
    #todo : address comment from Ian on ms 12/2015
    #W.bar may not be necessary
    #I had originally normalised these finesses by dividing by  Wbar. 
    #In retrospect this was not necessary
    #Ian said thi is necessary at this stage to ensure that the gamete frequencies in each sex sum to 1
    
    # W bar - Sum of numerators
    W.bar <- array_named(sex=c('m','f'))
    
    for( sex in dimnames(a_fitgen)$sex)
    {
      for( locus1 in dimnames(a_fitgen)$locus1)
      {
        for( locus2 in dimnames(a_fitgen)$locus2)
        {
          #have to do cis/trans specially
          if ( locus1=='RS1' & locus2=='RS2' )
          {
            W.bar[sex] = W.bar[sex] + (a_gtypes[sex,'RS1RS2_cis'] * a_fitgen[sex,locus1,locus2])
            W.bar[sex] = W.bar[sex] + (a_gtypes[sex,'RS1RS2_trans'] * a_fitgen[sex,locus1,locus2])
          }else
          {
            W.bar[sex] = W.bar[sex] + (a_gtypes[sex,paste0(locus1,locus2)] * a_fitgen[sex,locus1,locus2])                
          }
        }
      }
    }
    
    # doing calculation using W.bar from above
    
    for( sex in dimnames(a_fitgen)$sex)
    {
      for( locus1 in dimnames(a_fitgen)$locus1)
      {
        for( locus2 in dimnames(a_fitgen)$locus2)
        {
          #have to do cis/trans specially
          if ( locus1=='RS1' & locus2=='RS2' )
          {
            a_gtypes_s[sex,'RS1RS2_cis']   <- (a_gtypes[sex,'RS1RS2_cis'] * a_fitgen[sex,locus1,locus2]) / W.bar[sex]
            a_gtypes_s[sex,'RS1RS2_trans'] <- (a_gtypes[sex,'RS1RS2_trans'] * a_fitgen[sex,locus1,locus2]) / W.bar[sex]
          }else
          {
            a_gtypes_s[sex,paste0(locus1,locus2)] <- (a_gtypes[sex,paste0(locus1,locus2)] * a_fitgen[sex,locus1,locus2]) / W.bar[sex]                
          }
        }
      }
    }
  }
  
  # check that genotype frequencies total 1.
  for(sex in c('m','f'))
  {
    # allow for rounding differences
    if ( !isTRUE( all.equal(1, sum(a_gtypes_s[sex,])  )))
      warning(sex," genotype frequencies after selection total != 1 ", sum(a_gtypes_s[sex,]) )         
  }
  
  return(a_gtypes_s)
  
}