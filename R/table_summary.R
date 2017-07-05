#' Produce a dataframe with numbers and percentages
#'
#' @description Takes a data frame and tabulates two variables producing
#'              the number and percentage either by row, column or total.
#'
#' @details
#'
#' People like to see the raw numbers but often think about them the summary
#' statistics that describe them.  This wrapper function facilitates production
#' of such tables which can then be formatted easily using the \code{pixiedust}
#' package which is highly recommended.
#'
#'
#' @param df Data frame.
#' @param lookup_fields Data frame with descriptions of variables.  If working with data from Prospect this should be the imported \code{Fields} worksheet from the database specification spreadsheet(/GoogleSheet).
#' @param id Unique identifier for individuals.
#' @param select Variables to be summarised.
#' @param group Variables by which to summarise the data by.
#' @param digits Number of decimal places to be used in proportion/percentages.
#' @param nomissing Logical of whether to remove from summary of continuous variables instances where all observations are missing.
#'
#' @export
table_summary <- function(df            = .,
                          lookup_fields = master$lookups_fields,
                          id            = individual_id,
                          select        = c(),
                          nomissing     = TRUE,
                          ## group      = c(),
                          ## digits     = 3,
                          ...){
    ## Results list
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    ## quo_group  <- quos(group)
    quo_group  <- quos(...)
    ## paste0('Quoted ID     : ', quo_id) %>% print()
    ## paste0('Quoted Select : ', quo_select) %>% print()
    ## paste0('Quoted Group  : ', quo_group) %>% print()
    ## Subset the data
    df <- df %>%
          dplyr::select(!!quo_id, !!quo_select, !!!quo_group) %>%
          unique()
    ##################################################################################
    ## Summarise continuous variables                                               ##
    ##################################################################################
    ## Of the provided list of variables to select (quo_select) need to remove those
    ## that are factors.
    ## Have already removed all but id, select and group vars so
    numeric_vars <- df %>%
                    dplyr::select(which(sapply(., class) == 'numeric'),
                                  which(sapply(., class) == 'integer')) %>%
                    names()
    ## Remove event_name and site from the numeric_vars list (they are actually factors)
    ## ToDo :  Generalise this rather than having it hard coded
    numeric_vars <- numeric_vars[numeric_vars != 'individual_id']
    if(length(numeric_vars) >= 1){
        ## gather() data, just in case there is > 1 variable selected to be summarised
        results$df_numeric <- df %>%
                              dplyr::select(which(sapply(., class) == 'numeric'),
                                            !!quo_id, !!!quo_group)
        results$df_numeric <- df %>%
                              dplyr::select(which(sapply(., class) == 'numeric'),
                                            !!quo_id, !!!quo_group) %>%
                              gather(key = variable, value = value, numeric_vars)
        ## Summarise selected variables by specified groups
        results$continuous <- results$df_numeric %>%
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
                                        max     = max(value, na.rm = TRUE)) %>%
                              ungroup() %>%
                              left_join(.,
                                        lookup_fields,
                                        by = c('variable' = 'identifier')) %>%
                              dplyr::select(!!!quo_group, label,
                                            n, missing,
                                            mean, sd,
                                            p01, p05, p25, p50, p75, p95, p99,
                                            min, max)
        if(nomissing == TRUE){
            results$continuous <- results$continuous %>%
                                  dplyr::filter(!is.na(mean) & !is.na(sd) & !is.na(min) & !is.na(max))
        }
    }
    ##################################################################################
    ## Summarise continuous variables                                               ##
    ##################################################################################
    factor_vars <- df %>%
                   dplyr::select(which(sapply(., class) == 'factor'),
                                 !!!quo_group) %>%
                    names()
    ## Remove 'event_name' which should be a factor to assist plotting but on a different axis
    ## ToDo : This is currently hard coded, need to make it flexible
    factor_vars <- factor_vars[factor_vars != 'event_name']
    factor_vars <- factor_vars[factor_vars != 'site']
    if(length(factor_vars) >= 1){
        results$df_factor <- df %>%
                             dplyr::select(which(sapply(., class) == 'factor'),
                                           !!!quo_group, !!quo_id)
        results$df_factor <- df %>%
                             dplyr::select(which(sapply(., class) == 'factor'),
                                           !!!quo_group, !!quo_id) %>%
                             gather(key = variable, value = value, factor_vars)
        results$factor <- results$df_factor %>%
                          group_by(!!!quo_group, variable, value) %>%
                          summarise(n = n()) %>%
                          ungroup() %>%
                          group_by(!!!quo_group, variable) %>%
                          mutate(N = sum(n),
                                 prop = (n * 100) / N) %>%
                          left_join(.,
                                    lookup_fields,
                                    by = c('variable' = 'identifier')) %>%
                          dplyr::select(!!!quo_group, label, value, n, prop)
    }
    return(results)
}
