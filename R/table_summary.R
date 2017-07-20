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
#' @param time Variable that defines the events over time, default is that used in Prospect \code{event_name}.
#' @param group Variable that defines the grouping of participants, default is that used in Prospect \code{group}.
#' @param nomissing Logical of whether to remove from summary of continuous variables instances where all observations are missing.
#' @param digits Number of decimal places to be used in tidied and formatted output.  By default this is \code{NULL} and a pain data frame is returned without combining columns.  For continuous variables the following statistics are combined \code{mean (sd)}, \code{median (IQR)}, \code{min-max}, whilst for factor variables it is simply \code{n (prop)}.
#' @param reshape A formula of how to reshape the data to wide if (and only if) formatted results are being output (i.e. requires \code{!is.null(digits)} option).
#'
#' @seealso
#'
#' \url{https://ropensci.org/blog/blog/2017/07/11/skimr}
#' \url{https://github.com/MaximeWack/desctable}
#'
#' @export
table_summary <- function(df            = .,
                          lookup_fields = master$lookups_fields,
                          id            = individual_id,
                          select        = c(),
                          time          = event_name,
                          group         = group,
                          nomissing     = TRUE,
                          digits        = NULL,
                          reshape       = NULL,
                          ...){
    ## Results list
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    quo_group  <- enquo(group)
    quo_time   <- enquo(time)
    ## paste0('Quoted ID     : ', quo_id) %>% print()
    ## paste0('Quoted Select : ', quo_select) %>% print()
    ## paste0('Quoted Group  : ', quo_group) %>% print()
    ## paste0('Quoted Time   : ', quo_time) %>% print()
    ## Subset the data
    df <- df %>%
          dplyr::select(!!quo_id, !!quo_select, !!quo_group, !!quo_time) %>%
          unique()
    ## Subset the select variables and assess which are numeric and which are factors
    t <- df %>%
         dplyr::select(!!quo_select)
    numeric_vars <- t %>%
                    dplyr::select(which(sapply(., class) == 'numeric'),
                                  which(sapply(., class) == 'integer')) %>%
                    names()
    factor_vars <- t %>%
                    dplyr::select(which(sapply(., class) == 'factor')) %>%
                    names()
    ##################################################################################
    ## Summarise continuous variables                                               ##
    ##################################################################################
    if(length(numeric_vars) >= 1){
        ## gather() data, just in case there is > 1 variable selected to be summarised
        results$df_numeric <- df %>%
                              dplyr::select(which(sapply(., class) == 'numeric'),
                                            !!quo_id, !!quo_group, !!quo_time) %>%
                              gather(key = variable, value = value, numeric_vars)
        ## Summarise selected variables by specified groups
        results$continuous <- results$df_numeric %>%
                              group_by(variable, !!quo_group, !!quo_time) %>%
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
                              dplyr::select(!!quo_group, !!quo_time, label,
                                            n, missing,
                                            mean, sd,
                                            p01, p05, p25, p50, p75, p95, p99,
                                            min, max) %>%
                              arrange(!!quo_time, !!quo_group, label)
        if(nomissing == TRUE){
            results$continuous <- results$continuous %>%
                                  dplyr::filter(!is.na(mean) & !is.na(sd) & !is.na(min) & !is.na(max))
        }
        if(!is.null(digits)){
            results$continuous <- results$continuous %>%
                                  mutate(mean_sd    = paste0(formatC(mean,
                                                                     digits = digits,
                                                                     format = 'f'),
                                                             ' (',
                                                             formatC(sd,
                                                                     digits = digits,
                                                                     format = 'f'),
                                                             ')'),
                                         median_iqr = paste0(formatC(p50,
                                                                     digits = digits,
                                                                     format = 'f'),
                                                             ' (',
                                                             formatC(p25,
                                                                     digits = digits,
                                                   format = 'f'),
                                                   ' - ',
                                                   formatC(p75,
                                                           digits = digits,
                                                           format = 'f'),
                                                   ')'),
                                         range      = paste0(formatC(min,
                                                                     digits = digits,
                                                                     format = 'f'),
                                                             ' - ',
                                                             formatC(max,
                                                                     digits = digits,
                                                                     format = 'f'))) %>%
                dplyr::select(-mean, -sd, -p01, -p05, -p25, -p50, -p75, -p95, -p99, -min, -max)
            if(!is.null(reshape)){
                results$continuous <- results$continuous %>%
                                      melt(id.vars      = c(quo_group, 'label', 'value'),
                                           measure.vars = c('n_prop'),
                                           value.name   = 'val') %>%
                    ## ToDo - How to split quo_group, most likely have to change
                    ##        the way grouping variables are specified
                                      dcast(reshape, value.var = 'val')
            }
        }
    }
    ##################################################################################
    ## Summarise Factor variables                                                   ##
    ##################################################################################
    if(length(factor_vars) >= 1){
        results$df_factor <- df %>%
                             dplyr::select(which(sapply(., class) == 'factor'),
                                           !!quo_group, !!quo_time, !!quo_id) %>%
                             gather(key = variable, value = value, factor_vars)

        ## print('Selecting and gathering works.')
        ## Filter out data with missing grouping variables then group and summarise
        ## in terms of N and propotion
        if(nomissing == TRUE){
            results$df_factor <- results$df_factor %>%
                                 dplyr::filter(!is.na(!!quo_group), !is.na(!!quo_time), !is.na(variable))
        }
        results$factor <- results$df_factor %>%
                          group_by(!!quo_group, !!quo_time,  variable, value) %>%
                          summarise(n = n()) %>%
                          ungroup() %>%
                          group_by(!!quo_group, !!quo_time, variable) %>%
                          mutate(N = sum(n),
                                 prop = (n * 100) / N) %>%
                          left_join(.,
                                    lookup_fields,
                                    by = c('variable' = 'identifier')) %>%
                          ungroup() %>%
                          dplyr::select(!!quo_group, !!quo_time, label, value, n, prop)
        ## Optionally combine N and Prop
        if(!is.null(digits)){
            results$factor <- results$factor %>%
                              mutate(n_prop = paste0(n,
                                                     ' (',
                                                     formatC(prop, digits = digits, format = 'f'),
                                                     ')')) %>%
                              dplyr::select(-n, -prop)
            if(!is.null(reshape)){
                results$factor %>%
                    melt(id.vars      = c(!!quo_group, !!quo_time, 'label', 'value'),
                         measure.vars = c('n_prop'),
                         value.name   = 'val') %>% names() %>% print()
                results$factor <- results$factor %>%
                                  melt(id.vars      = c(!!quo_group, !!quo_time, 'label', 'value'),
                                       measure.vars = c('n_prop'),
                                       value.name   = 'val') %>%
                    ## ToDo - How to split quo_group, most likely have to change
                    ##        the way grouping variables are specified
                                  dcast(reshape, value.var = 'val')
            }
        }
    }
    return(results)
}
