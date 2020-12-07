#' Interface for Area classification based on census data.
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