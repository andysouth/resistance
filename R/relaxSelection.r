#' relax selection by modifying indiv fitnesses
#' 
#' 
#' @param Windiv array of individual fitnesses
#' @param k generation number
#' 
#' @return array of individual fitnesses modified

#don't @export

relaxSelection <- function(Windiv, k){
  

  if( (2<k) & (k<12)){
    
    # relaxed selection fitnesses
    Windiv[] <- 0.1
    
  } else if( (k>11) & (k<15)){
    
    # non relaxed fitnesses
    Windiv[] <- 0.1
    #setting m,f at same time
    Windiv[,'RS1','RS2'] <- 1
    Windiv[,'RS1','RR2'] <- 1
    Windiv[,'RR1','RS2'] <- 1
    Windiv[,'RR1','RR2'] <- 1
    
  } else if( k>14 ){
    
    # relaxed selection fitnesses
    Windiv[] <- 0.1
  }
  
return(Windiv)
}