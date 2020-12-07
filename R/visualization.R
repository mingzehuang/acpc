#' Interface for Area classification based on census data.
#' 
#' You're supposed to save your data input as an object call "data_for_acpc" in your global environment, then do visualization() to see the App window.
#' 
#' @return Run shiny app for output, two principle components, all parameters are set by default value.
#' @export
visualization <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "acpc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `acpc`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}