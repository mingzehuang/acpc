#' Interface for Area classification based on survey data.
#' @return Run shiny app for output.
#' @export
visualization <- function(censusdata) {
  if (is.missing(cencusdata) | is.null(censusdata))
  censusdata = readRDS(url("https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds", "rb"))
  appDir <- system.file("shiny-examples", "myapp", package = "acpc")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `acpc`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}