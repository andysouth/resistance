#' Makes data.frame into a matrix with parameters as rownames
#' 
#' takes data frame as dat, and column in data frame containing parameters as rnames.
#' this produces NA generation errors due to coercing location column, this column of NA's is however removed.

#' @param mat
#' @param rnames
#' 
#' @return ? dat.m
#' @export

make.matrix <- function(mat, rnames){
  dat.m <- data.matrix(mat)				#converts to data.matrix
  dat.m <- dat.m[ ,2:ncol(dat.m)]			#removes column of parameter names (will be NA's)
  rownames(dat.m) <- rnames				#uses data in column of original data frame given by datlocs to rename matrix rows
  return(dat.m)
}