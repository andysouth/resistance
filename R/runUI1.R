#' run a shiny User Interface to a simple version of the resistance model used by curtis
#' 
#' displays a user interface in the browser, allowing users to modify inputs and see how the frequency of resistance alleles responds

#' @return nothing
#' @export

runUI1 <- function() {
  
  shiny::runApp(system.file('shiny/shinyCurtis1', package='resistance'))

}
