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
#' @param group Variable by which to summarise the data by.
#' @param events Variable defining repeated events.
#' @param position Position adjustment for ggplot2, default \code{'dodge'} avoids overlapping histograms.
#' @param individual Logical of whether to plot outcomes indvidually.
#' @param plotly Logical of whether to make \code(ggplotly()) figures.  This is useful if outputing HTML since the embedded figures are zoomable.
#'
#' @export
plot_summary <- function(df     = .,
                         id     = individual_id,
                         select = c(),
                         lookup = master$lookups_fields,
                         group  = group,
                         events = event_name,
                         theme  = theme_bw(),
                         position   = 'dodge',
                         individual = FALSE,
                         plotly     = FALSE,
                         ...){
    ## Results to return
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    quo_group  <- enquo(group)
    quo_events <- enquo(events)
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
                                    by = c('variable' = 'identifier')) %>%
                          ## Ensure value is numberic otherwise nothing to plot
                          mutate(value = as.numeric(value))
    ## Facetted plot
    results$continuous <- results$df_numeric %>%
                          dplyr::filter(!is.na(!!!quo_group)) %>%
                          ggplot(aes_(~value, fill = quo_group)) +
                          geom_histogram(position = position) +
                          facet_wrap(~label,
                                     scales = 'free',
                                     strip.position = 'bottom') +
                          xlab('') + ylab('N') +
                          theme +
                          theme(strip.background = element_blank(),
                                strip.placement  = 'outside')
    if(plotly == TRUE){
        results$continuous <- results$continuous %>%
                              ggplotly()
    }
    ## Plot individual figures if requested
    if(individual == TRUE){
        for(x in numeric_vars){
            ## Extract the label
            xlabel <- results$df_numeric %>%
                      dplyr::filter(variable == x) %>%
                      dplyr::select(label) %>%
                      unique() %>%
                      as.data.frame()
            ## Plot current variable
            results[[x]] <- results$df_numeric %>%
                            dplyr::filter(!is.na(!!!quo_group) & variable == x) %>%
                            ggplot(aes_(~value, fill = quo_group)) +
                            geom_histogram(position = position) +
                            xlab(xlabel[[1]]) +
                            ylab('N') +
                            theme
            if(plotly == TRUE){
                results[[x]] <- results[[x]] %>%
                                ggplotly()
            }
        }
    }
    ## Subset factor variables, gather() and plot these
    factor_vars <- which(sapply(df, class) == 'factor') %>% names()
    results$df_factor <- df %>%
                          dplyr::select(which(sapply(., class) == 'factor'), !!quo_id, !!!quo_group) %>%
                          gather(key = variable, value = value, factor_vars) %>%
                          left_join(.,
                                    lookup,
                                    by = c('variable' = 'identifier'))

    return(results)
}
