#' Run Shiny Applications
#'
#' @description The ctru package includes Shiny applications run them with this function.
#'
#' @details
#'
#' The ctru package has been supplemented with Shiny applications to perform sample size
#' calculations for different study designs.  This function allows you to start these without
#' recourse to \code{setwd()} and \code{runApp()}.
#'
#' Currently there is only one Shiny application (the default \code{sample_size}), but this
#' function has been written to allow additional applications to be included and run.
#'
#' @param example Name of the example you wish to run.
#'
#' @export
shiny_examples<- function(example = "sample_size") {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "ctru"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `shiny_examples()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "ctru")
  shiny::runApp(appDir, display.mode = "normal")
}
