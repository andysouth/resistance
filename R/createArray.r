#' deprecated, replaced with createArray2() create array for representing genotypes and niches
#' 
#' creates array with the passed dimensions
#' 
#' @param sex vector of dimension names e.g. c('f','m')
#' @param loci vector of dimension names e.g. c('SS1','RS1','RR1','SS2','RS2','RR2')
#' @param locus1 vector of dimension names e.g. c('SS1','RS1','RR1')
#' @param locus2 vector of dimension names e.g. c('SS2','RS2','RR2')
#' @param niche1 vector of dimension names e.g. c('0','a','A')
#' @param niche2 vector of dimension names e.g. c('0','a','A')
#' @param exposure vector of dimension names e.g. c('no','lo','hi')
#' @param fillValue optional what to fill array with, defaults to 0  
#' 
#' @examples 
#' createArray( sex = c('f','m') ) 
#' #Wloci
#' createArray( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), niche1=c('0','a','A'), niche2=c('0','b','B') )
#' createArray( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2') )
#' createArray( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1 = c('0','a','A') )
#' createArray( sex = c('f','m'), locus1 = c('SS1','RS1','RR1'), locus2 = c('SS2','RS2','RR2'), niche1 = c('0','a','A'), niche2 = c('0','a','A') )
#' 
#' @return matrix
#' @export

# createArray <- function(sex = c('f','m'),
#                         loci = c('SS1','RS1','RR1','SS2','RS2','RR2'),
#                         locus1 = c('SS1','RS1','RR1'),
#                         locus2 = c('SS2','RS2','RR2'),
#                         niche1 = c('0','a','A'),
#                         niche2 = c('0','a','A'),
#                         fillValue = 0
# ){

createArray <- function(sex = NULL,
                        loci = NULL,
                        locus1 = NULL,
                        locus2 = NULL,
                        niche1 = NULL,
                        niche2 = NULL,
                        exposure = NULL,
                        fillValue = 0
                        ){

  #dimnames1 <- list( sex=sex, locus1=locus1, locus2=locus2 )
  #dimnames1 <- NULL
  
  # building up dimension names according to passed args
  
  #BEWARE exists() also searches in GlobalEnv which can mess up this function
  #exists("dimnames1", environment(), inherits=FALSE) #fixes it
  
  if(!is.null(sex)) dimnames1 <- list( sex=sex )

  if(!is.null(loci)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(loci=loci) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( loci=loci )
  }
  
  if(!is.null(locus1)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(locus1=locus1) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( locus1=locus1 )
  }
  
  if(!is.null(locus2)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(locus2=locus2) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( locus2=locus2 )
  }  

  if(!is.null(niche1)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(niche1=niche1) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( niche1=niche1 )
  }  
    
  if(!is.null(niche2)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(niche2=niche2) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( niche2=niche2 )
  }   
  
  if(!is.null(exposure)) {
    if(exists("dimnames1", environment(), inherits=FALSE)) {
      dimnames1 <- list( dimnames1, list(exposure=exposure) )
      dimnames1 <- unlist(dimnames1, recursive=FALSE)
    }
    else dimnames1 <- list( exposure=exposure )
  }  
  
#   if(!is.null(exposure2)) {
#     if(exists("dimnames1", environment(), inherits=FALSE)) {
#       dimnames1 <- list( dimnames1, list(exposure2=exposure2) )
#       dimnames1 <- unlist(dimnames1, recursive=FALSE)
#     }
#     else dimnames1 <- list( exposure2=exposure2 )
#   }   
  
  #setting dimensions of array from dimnames1
  dim1 <- sapply(dimnames1, function(x) length(x))
  
  #creating array and filling with fill value
  a <- array(fillValue, dim=dim1, dimnames=dimnames1)
  
  return(a)
   
}