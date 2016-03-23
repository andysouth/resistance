#' create array for representing genotypes and niches
#' 
#' creates array with the passed dimensions
#' 
#' @param ... named dimensions to create in array e.g. dimname=c('name1','name2') 
#' 
#' @examples 
#' createArray( sex = c('f','m') ) 
#' 
#' createArray( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), 
#'              niche1=c('0','a','A'), niche2=c('0','b','B') )
#' createArray( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), 
#'              locus2 = c('SS2','RS2','RR2'), 
#'              niche1 = c('0','a','A'), niche2 = c('0','a','A') )#' 
#' @return array
#' @export



createArray2 <- function( ...)
  {
  
  
  #previously didn't work if a named variable rather than the actual range is passed
  #now seems it does
  #e.g.
  #works
  #createArray2( sex = c('f','m') )
  #used to fail
  #sex = c('f','m')
  #createArray2( sex = sex )
  
  
  listArgs <- as.list(match.call()[-1])
  
  #works only if args are specified by actual ranges not by a varname
  #dimnames1 <- lapply(listArgs,eval)
  
  #fails
  #dimnames1 <- lapply(listArgs,function(x){eval.parent(x,n=1)})
  #works
  #eval.parent(listArgs$sex)
  #[1] "f" "m"
  #fails
  #dimnames1 <- lapply(listArgs,eval.parent)  
  #works!! n=3 but I'm not sure why !!!
  dimnames1 <- lapply(listArgs,function(x){eval.parent(x,n=3)})
  
  
  #setting dimensions of array from dimnames1
  dim1 <- sapply(dimnames1, function(x) length(x))
  
  #creating array and filling with fill value
  a <- array(0, dim=dim1, dimnames=dimnames1)
  
  return(a)
  
}