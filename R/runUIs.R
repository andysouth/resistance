#' run a shiny User Interface to display 2 scenarios allow user to modify both
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
# check said these not needed
# @import shiny markdown
#' @export

runUImix <- function() {
  
  shiny::runApp(system.file('shiny/resistmixseq', package='resistance'))
}

#' run a shiny User Interface to display 2 scenarios allow user to modify both
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUImob <- function() {
  
  shiny::runApp(system.file('shiny/resistmob2', package='resistance'))
}


#' run a shiny User Interface to a simple version of the resistance model used by curtis
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI1 <- function() {
  
  shiny::runApp(system.file('shiny/shinyCurtis1', package='resistance'))
}

#' run a shiny User Interface to look at the effect of sex-linkage
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI2 <- function() {
  
  shiny::runApp(system.file('shiny/shinyCurtis2', package='resistance'))
}

#' run a shiny User Interface to display Curtis Fig2 and allow user to modify
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI3 <- function() {
  
  shiny::runApp(system.file('shiny/shinyFig2Curtis', package='resistance'))
}


#' run a shiny User Interface to display 2 scenarios allow user to modify both
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI5 <- function() {
  
  shiny::runApp(system.file('shiny/resistmob_mosaic', package='resistance'))
}

#' run a shiny User Interface to display fitness
#' 
#' @return nothing
#' @export
runUIfit <- function() {
  
  shiny::runApp(system.file('shiny/fitvis', package='resistance'))
}

#' run a shiny User Interface to display exposure
#' 
#' @return nothing
#' @export
runUIexpo <- function() {
  
  shiny::runApp(system.file('shiny/expovis', package='resistance'))
}

