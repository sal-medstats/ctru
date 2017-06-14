#' Produce a plots of multiple variables
#'
#' @description Takes a data frame and plots the distribution of continuous or
#'              categorical variables
#'
#' @details
#'
#' People like to see their raw numbers plotted and get a feel for the data and
#' its distribution.  This wrapper function facilitates production
#' of such such graphs.
#'
#'
#' @param df Data frame.
#' @param id Unique identifier for individuals.
#' @param select Variables to be summarised.
#' @param lookup Data frame with descriptions of variables.  If working with data from
#'               Prospect this should be the imported \code{Fields} worksheet from the
#'               database specification spreadsheet(/GoogleSheet).
#' @param group Variables by which to summarise the data by.
#' @param digits Number of decimal places to be used in proportion/percentages.
#'
#' @export
plot_summary <- function(df     = .,
                         id     = individual_id,
                         select = c(),
                         lookup = master$lookups_fields,
                         ## group  = c(),
                         ## digits = 3,
                         theme  = theme_bw(),
                         ...){
    ## Results to return
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    ## quo_group  <- quos(group)
    quo_group  <- quos(...)
    ## Subset the data and de-duplicate
    df <- df %>%
          dplyr::select(!!quo_id, !!quo_select, !!!quo_group) %>%
          unique()
    ## Subset the continuous variables gather() and bind with the lookup descriptions
    ## so that when plotted the graphs have meaningful titles
    numeric_vars <- which(sapply(df, class) == 'numeric') %>% names()
    results$df_numeric <- df %>%
                          dplyr::select(which(sapply(., class) == 'numeric'), !!quo_id, !!!quo_group) %>%
                          gather(key = variable, value = value, numeric_vars) %>%
                          left_join(.,
                                    lookup,
                                    by = c('variable' = 'identifier'))
    quo_group[[1]] %>% print()
    results$continuous <- results$df_numeric %>%
                          dplyr::filter(!is.na(!!!quo_group)) %>%
                          ## ToDo : get colour() working
                          ## ggplot(aes(x = value)) +
                          ggplot(aes_(~value, colour = quo_group[[1]])) %>%
                          ## ## ggplot(aes(value, colour = !!!quo_group)) %>%
                          geom_histogram(stat = 'count') +
                          facet_wrap(~label,
                                     scales = 'free',
                                     strip.position = 'bottom') +
                          xlab('') + ylab('N') +
                          theme +
                          theme(strip.background = element_blank(),
                                strip.placement  = 'outside')

    ## Subset factor variables, gather() and plot these
    return(results)
}
