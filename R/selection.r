#' selection of genotypes based upon their fitnesses
#' 
#'   
#' 
#' @param freq array with frequencies of genotypes in the popn.
#' @param Windiv array of individual fitnesses by genotype 
#' @param calibration flag if set to 103 turns off selection
#' 
#' @examples 
#' #selection() 
#' @return array of post-selection genotype frequencies
#' @export



selection <- function( freq, Windiv, calibration)
{

  # copy after selection frequencies from before
  # to initialise array
  fs <- freq
  
  if(calibration!=103){		## no selection calibration
    
    #todo : address comment from Ian on ms 12/2015
    #W.bar may not be necessary
    #I had originally normalised these finesses by dividing by  Wbar. 
    #In retrospect this was not necessary
    #Ian said thi is necessary at this stage to ensure that the gamete frequencies in each sex sum to 1
    
    # W bar - Sum of numerators
    W.bar <- createArray2(sex=c('m','f'))
    
    for( sex in dimnames(Windiv)$sex)
    {
      for( locus1 in dimnames(Windiv)$locus1)
      {
        for( locus2 in dimnames(Windiv)$locus2)
        {
          #have to do cis/trans specially
          if ( locus1=='RS1' & locus2=='RS2' )
          {
            W.bar[sex] = W.bar[sex] + (freq[sex,'RS1RS2_cis'] * Windiv[sex,locus1,locus2])
            W.bar[sex] = W.bar[sex] + (freq[sex,'RS1RS2_trans'] * Windiv[sex,locus1,locus2])
          }else
          {
            W.bar[sex] = W.bar[sex] + (freq[sex,paste0(locus1,locus2)] * Windiv[sex,locus1,locus2])                
          }
        }
      }
    }
    
    # doing calculation using W.bar from above
    
    for( sex in dimnames(Windiv)$sex)
    {
      for( locus1 in dimnames(Windiv)$locus1)
      {
        for( locus2 in dimnames(Windiv)$locus2)
        {
          #have to do cis/trans specially
          if ( locus1=='RS1' & locus2=='RS2' )
          {
            fs[sex,'RS1RS2_cis']   <- (freq[sex,'RS1RS2_cis'] * Windiv[sex,locus1,locus2]) / W.bar[sex]
            fs[sex,'RS1RS2_trans'] <- (freq[sex,'RS1RS2_trans'] * Windiv[sex,locus1,locus2]) / W.bar[sex]
          }else
          {
            fs[sex,paste0(locus1,locus2)] <- (freq[sex,paste0(locus1,locus2)] * Windiv[sex,locus1,locus2]) / W.bar[sex]                
          }
        }
      }
    }
  }
  
  # check that genotype frequencies total 1.
  for(sex in c('m','f'))
  {
    # allow for rounding differences
    if ( !isTRUE( all.equal(1, sum(fs[sex,])  )))
      warning(sex," genotype frequencies after selection total != 1 ", sum(fs[sex,]) )         
  }
  
  return(fs)
  
}