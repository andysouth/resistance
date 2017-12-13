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
#' # to create arrays with flexible dimension names and numbers
#' # e.g. here with flexible number of dimensions named niche*
#' n_insecticides <- 3
#' # list(c()) is important below to create one list for each dim
#' l_dimnames <- rep(list(c('no','lo','hi')), n_insecticides)
#' names(l_dimnames) <- paste0("niche", 1:n_insecticides)
#' do.call(array_named,l_dimnames)
#'                
#' @return array
#' @export

array_named <- function(...)
  {
   array(0, dim = lengths(list(...)), dimnames = list(...))
  }

# 4/10/17 this was a potential way of specifying the names of dimnames separately
# to try to make the number and names of dimensions flexible
# but instead solved by do.call as in the examples above
# array_named_flex <- function(namesdimnames, ...)
# {
#   a_ <- array(0, dim = lengths(list(...)), dimnames = list(...))
#   names(dimnames(a_)) <- namesdimnames
#   return(a_)
# }
