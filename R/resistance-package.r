#' insecticide resistance modelling
#' 
#' submitted model of the development of insecticide resistance.
#' 
#' @name resistance-package
#' @aliases resistance-package resistance
#' @docType package
#' @author Andy South
#' 
#' @keywords package
#' @import grDevices graphics
# tidyr & ggplot2 imported to some functions
# tidyverse
#' @importFrom stats as.formula runif
#' @importFrom utils read.csv write.csv

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# copied from : https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))