#' Runs Shiny examples from this package
#'
#' @param example The Shiny example you wish to run (omit for a list of possible options).
#' @param display.mode Shiny display mode to use \code{normal} (default) \code{showcase}.
#' @export
ctru_shiny<- function(example = "sample_size",
                      display.mode = "normal") {
    # locate all the shiny app examples that exist
    valid_examples <- list.files(system.file("shiny", package = "nmisc"))
    valid_examples_msg <-
        paste0("Valid examples are: '",
               paste(valid_examples, collapse = "', '"),
               "'")

    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||
        !example %in% valid_examples){
        stop('Please run `ctru_shiny()` with a valid example app as an argument.\n',
            valid_examples_msg,
            call. = FALSE)
    }
    # find and launch the app
    app_dir <- system.file("shiny", example, package = "ctru")
    shiny::runApp(app_dir, display.mode = display.mode)
}
