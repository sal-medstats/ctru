#' Search a data frames variable names for a pattern
#'
#' @description Takes a data frame and search for a regular expression in the variables names.
#'
#' @details
#'
#'
#'
#'
#' @param df Data frame.
#' @param pattern Regular expression for which variable names are to be searched.
#'
#' @export
table_summary <- function(df      = mtcars,
                          pattern = 'mpg',
                          ...){
    df[,grepl(pattern, names(df))] %>% names()
}
