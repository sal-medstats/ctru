#' Produce a dataframe with numbers and percentages
#'
#' @description Takes a data frame and tabulates two variables producing
#'              the number and percentage either by row, column or total.
#'
#' @details
#'
#' People like to see the raw numbers but often think about them in
#' percentages, this wrapper function facilitates production of such tables.
#'
#'
#' @param df Data frame.
#' @param digits Number of decimal places to be used in proportion/percentages.
#' @param reshape Logical indicator to indicate whether data needs reshaping to long.
#'
#' @export
table_summary_ <- function(df              = .data,
                           digits          = 2,
                           reshape         = TRUE,
                           group_by        = NULL,
                           ...){
    results <- list()
    ## Make data long
    if(reshape == TRUE){
        df <- df %>%
              dplyr::select(group_by, ...) %>%
              gather(key = to.sum, ...)
    }
    ## Optionally summarise by specified group_by
    if(!is.null(group_by)){
        df <- df %>%
              group_by(group_by)
    }
    ## Summarise
    results$table <- df %>%
                     summarise(N      = n(),
                               n      = !is.na(value),
                               missing = is.na(value),
                               mean   = mean(value, na.rm = TRUE),
                               sd     = sd(value, na.rm = TRUE),
                               p25    = quantile(value, probs = 0.25, na.rm = TRUE),
                               p50    = quantile(value, probs = 0.50, na.rm = TRUE),
                               p75    = quantile(value, probs = 0.75, na.rm = TRUE))
    return(results$table)
}
