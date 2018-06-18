####################################################################################################
## Script to run the AFAM App
## This is a simple script to run the AFAM App on your computer
## Please contact Gavin McDonald with any questions - gmcdonald@bren.ucsb.edu
## Originally written at University of California, Santa barbara on March 24, 2017
####################################################################################################

#' Title
#'
#' @return
#' @export
#'
#' @examples
runAFAM <- function() {
  appDir <- system.file("shiny-examples", "afamApp", package = "afamAppPackage")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `afamAppPackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
  
}
