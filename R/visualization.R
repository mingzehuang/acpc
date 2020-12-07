#' Interface for Area classification based on survey data.
#'
#' @param data_input Dataset for visualization.
#'
#' @return Run shiny app for output.
#' @export
visualization <- function(data_input = NULL) {
  if (missing(data_input) | is.null(data_input)) {
    data_input = readRDS(url("https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds", "rb"))
  }
  assign("data_for_acpc", data_input, envir = .GlobalEnv)
  appDir <- system.file("shiny-examples", "myapp", package = "acpc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `acpc`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}