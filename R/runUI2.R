#' run a shiny User Interface to look at the effect of sex-linkage
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI2 <- function() {
  
  shiny::runApp(system.file('shiny/shinyCurtis2', package='resistance'))
  
}