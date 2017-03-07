#' create an array with named dimensions
#' 
#' an array specified by an number of dimensions with e.g. (name1=c('a','b','c), name2=c('x','y')) 
#' 
#' @param ... named dimensions to create in array e.g. (name1=c('a','b','c), name2=c('x','y')) 
#' 
#' @examples 
#' array_named( sex = c('f','m') ) 
#' 
#' sexvar <- c('f','m')
#' array_named( sex = sexvar ) 
#' 
#' array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), 
#'              niche1=c('0','a','A'), niche2=c('0','b','B') )
#' array_named( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), 
#'              locus2 = c('SS2','RS2','RR2'), 
#'              niche1 = c('0','a','A'), niche2 = c('0','a','A') )#' 
#' @return array
#' @export

array_named <- function(...)
  {
   array(0, dim = lengths(list(...)), dimnames = list(...))
  }


