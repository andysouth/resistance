#' Finds proportion of genotypes pp, pq and qq, given P (i.e. sqrt(pp))
#' 
#' Takes P and matrix to print results to, returning matrix

#' @param P todo
#' @param mat todo
#' 
#' @return matrix
#' @export

HW <- function ( P, mat ){
  R <- P
  S <- 1 - R
  r <- R*R
  s <- S*S
  rs <- 2*(R*S)
  mat[1,] <- s
  mat[2,] <- rs
  mat[3,] <- r
  return( mat )
}