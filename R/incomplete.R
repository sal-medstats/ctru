#' Summarise by observation the completeness of data
#'
#' @description Summarise by observation the completeness of subsets of data
#'
#' @details
#'
#' It is often desirable to know how much missing data there is for subsets of variables
#' for each individual.  This function subsets the data for the specified variables
#' (\code{...}) and returns a data frame of these along with the number of them missing
#' in each row (observation)
#'
#' This function takes advantage of lazy evaluation.
#'
#'
#' @param df Data frame.
#' @param id Variable(s) that uniquely identify observations.
#' @param group Variable to group by.
#' @param to.check to be assessed for missing.
#'
#' @export
incomplete <- function(df              = .data,
                       id              = screening,
                       group           = group,
                       ...){
    ## Subset the data and calculate the number of missing obs for each individual
    results <- dplyr::select_(df, .dots = lazyeval::lazy_dots(group, ...)) %>%
               mutate(n_missing = rowSums(is.na(.)))
    return(results)
}
