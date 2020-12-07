#' Interface for Area classification based on survey data.
#'
#' @return Run shiny app for output.
#' @export
visualization <- function(data_for_acpc = NULL) {
  if (missing(data_for_acpc) | is.null(data_for_acpc)) {
    data_for_acpc = readRDS(url("https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds", "rb"))
  }
  assign(data_for_acpc, data_for_acpc, evir = ..GlobalEnv)
  appDir <- system.file("shiny-examples", "myapp", package = "acpc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `acpc`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}