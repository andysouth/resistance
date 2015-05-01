#' create array for representing genotypes and niches
#' 
#' creates array with the passed dimensions
#' 
#' @param ... named dimensions to create in array e.g. dimname=c('name1','name2') 
#' 
#' @examples 
#' createArray2( sex = c('f','m') ) 
#' #Wloci
#' createArray2( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )
#' createArray2( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
#' createArray2( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1 = c('0','a','A') )
#' createArray2( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1 = c('0','a','A'), niche2 = c('0','a','A') )
#' 
#' @return array
#' @export



createArray2 <- function( ...)
  {
  
  
  listArgs <- as.list(match.call()[-1])
  
  dimnames1 <- lapply(listArgs,eval)
  
  
  
  #1 get the arg names (exclude function name at item1)
  #argString <- names(match.call())[-1]
  
  
#   loci <- c('tits','wank')
#   
#   if(!is.null(sex)) dimnames1 <- list( sex=sex )
#   
#   if(!is.null(loci)) {
#     if(exists("dimnames1", environment(), inherits=FALSE)) {
#       dimnames1 <- list( dimnames1, list(loci=loci) )
#       dimnames1 <- unlist(dimnames1, recursive=FALSE)
#     }
#     else dimnames1 <- list( loci=loci )
#   }
  
  
  #setting dimensions of array from dimnames1
  dim1 <- sapply(dimnames1, function(x) length(x))
  
  #creating array and filling with fill value
  a <- array(0, dim=dim1, dimnames=dimnames1)
  
  return(a)
  
}