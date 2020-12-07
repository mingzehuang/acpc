#' Interface for Area classification based on survey data.
#'
#' @param censusdata Input data set for visualization.
#'
#' @return Run shiny app for output.
#' @export
visualization <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "acpc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `acpc`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}