#' Produce a dataframe with numbers and percentages
#'
#' @description Takes a data frame and tabulates two variables producing
#'              the number and percentage either by row, column or total.
#'
#' @details
#'
#' People like to see the raw numbers but often think about them the summary
#' statistics that describe them.  This wrapper function facilitates production
#' of such tables.
#'
#'
#' @param df Data frame.
#' @param lookup Data frame with descriptions of variables.  If working with data from
#'               Prospect this should be the imported \code{Fields} worksheet from the
#'               database specification spreadsheet(/GoogleSheet).
#' @param id Unique identifier for individuals.
#' @param select Variables to be summarised.
#' @param group Variables by which to summarise the data by.
#' @param digits Number of decimal places to be used in proportion/percentages.
#'
#' @export
table_summary <- function(df     = .,
                          lookup = master$lookup_fields,
                          id     = individual_id,
                          select = c(),
                          group  = c(),
                          ## digits = 3,
                          ...){
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    quo_group  <- quos(group)
    ## quo_group  <- quos(...)
    ## Subset the data
    df <- df %>%
          dplyr::select(!!quo_id, !!quo_select, !!!quo_group) %>%
          unique()
    ## gather() data, just in case there is > 1 variable selected to be summarised
    df <- df %>%
          gather(key = variable, value = value, !!quo_select)
    ## Summarise selected variables by specified groups
    results <- df %>%
               group_by(!!!quo_group, variable) %>%
               summarise(n       = n(),
                         missing = sum(is.na(value)),
                         mean    = mean(value, na.rm = TRUE),
                         sd      = sd(value, na.rm = TRUE),
                         p01     = quantile(value, probs = 0.01, na.rm = TRUE),
                         p05     = quantile(value, probs = 0.05, na.rm = TRUE),
                         p25     = quantile(value, probs = 0.25, na.rm = TRUE),
                         p50     = quantile(value, probs = 0.50, na.rm = TRUE),
                         p75     = quantile(value, probs = 0.75, na.rm = TRUE),
                         p95     = quantile(value, probs = 0.95, na.rm = TRUE),
                         p99     = quantile(value, probs = 0.99, na.rm = TRUE),
                         min     = min(value, na.rm = TRUE),
                         max     = max(value, na.rm = TRUE))
    ## Get meaningful labels for variables that are being summarised
    results <- left_join(results,
                         lookup,
                         by = c('variable' = 'identifier')) %>%
               dplyr::select(!!!quo_group, label, n, missing, mean, sd, p01, p05, p25, p50, p75, p95, p99, min, max)
    return(results)
}
