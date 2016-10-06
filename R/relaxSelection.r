#' relax selection by modifying indiv fitnesses
#' 
#' 
#' @param a_fitgen array of individual fitnesses
#' @param k generation number
#' 
#' @return array of individual fitnesses modified

#don't @export

relaxSelection <- function(a_fitgen, k){
  

  if( (2<k) & (k<12)){
    
    # relaxed selection fitnesses
    a_fitgen[] <- 0.1
    
  } else if( (k>11) & (k<15)){
    
    # non relaxed fitnesses
    a_fitgen[] <- 0.1
    #setting m,f at same time
    a_fitgen[,'RS1','RS2'] <- 1
    a_fitgen[,'RS1','RR2'] <- 1
    a_fitgen[,'RR1','RS2'] <- 1
    a_fitgen[,'RR1','RR2'] <- 1
    
  } else if( k>14 ){
    
    # relaxed selection fitnesses
    a_fitgen[] <- 0.1
  }
  
return(a_fitgen)
}