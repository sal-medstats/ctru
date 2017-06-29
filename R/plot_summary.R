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
#' @param plotly Logical of whether to make \code{ggplotly()} figures.  This is useful if outputing HTML since the embedded figures are zoomable.
#' @param remove.na Logical to remove NA from plotting (only affects factor variables since NA is excluded from continuous plots by default anyway).
#' @param title.continuous Title for faceted histogram plots of continuous variables.
#' @param title.factor TItle for faceted likert plots of factor variables.
#'
#' @export
plot_summary <- function(df               = .,
                         id               = individual_id,
                         select           = c(),
                         lookup           = master$lookups_fields,
                         group            = group,
                         events           = NULL,
                         theme            = theme_bw(),
                         position         = 'dodge',
                         individual       = FALSE,
                         plotly           = FALSE,
                         remove.na        = TRUE,
                         title.continuous = 'Continuous outcomes by treatment group.',
                         title.factor     = 'Factor outcomes by treatment group',
                         ...){
    ## Results to return
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    quo_group  <- enquo(group)
    quo_events <- enquo(events)
    ## Since quo_events is used to control subsequent steps if its NULL select()
    ## fails, therefore conditionally build the variables that are select on
    to_select <- c(quo_id, quo_group)
    ## if(!is.null(events)){
    ##     to_select <- c(to_select, quo_events)
    ## }
    ## Subset the data and de-duplicate
    df <- df %>%
          ## dplyr::select(!!quo_id, !!quo_select, !!quo_events, !!quo_group) %>%
          dplyr::select(!!!to_select, !!quo_select) %>%
          unique()
    ##########################################################################
    ## Continuous Variables                                                 ##
    ##########################################################################
    ## Subset the continuous variables gather() and bind with the lookup descriptions
    ## so that when plotted the graphs have meaningful titles
    numeric_vars <- which(sapply(df, class) == 'numeric') %>% names()
    ## Logical check required to determine if numeric variables are to be plotted.
    ## If none are specified then left_join() fails...
    results$df_numeric <- df %>%
                          dplyr::select(which(sapply(., class) == 'numeric'),
                                        !!!to_select) %>%
                          gather(key = variable, value = value, numeric_vars)
    if(names(results$df_numeric) %in% c('variable')){
        results$df_numeric <- results$df_numeric %>%
                              left_join(.,
                                        lookup,
                                        by = c('variable' = 'identifier')) %>%
                              ## Ensure value is numberic otherwise nothing to plot
                              mutate(value = as.numeric(value))
        print('Are we here?')
        ## Generate plot
        results$continuous <- results$df_numeric %>%
                              dplyr::filter(!is.na(!!quo_group)) %>%
                              ggplot(aes_(~value, fill = quo_group)) +
                              geom_histogram(position = position) +
                              xlab('') + ylab('N') +
                              ggtitle(title.continuous) +
                              theme +
                              theme(strip.background = element_blank(),
                                    strip.placement  = 'outside')
        ## Facetted plot when no events specified
        if(is.null(events)){
            results$continuous <- results$continuous +
                                  facet_wrap(~label,
                                             scales = 'free',
                                             strip.position = 'bottom')
        }
        else{
            results$continuous <- results$continuous +
                                  facet_grid(label~quo_events,
                                             scales = 'free')
        }
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
                                dplyr::filter(!is.na(!!quo_group) & variable == x) %>%
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
    }
    ##########################################################################
    ## Factor Variables                                                     ##
    ##########################################################################
    ## Consider plotting using methods described at...                      ##
    ## http://rnotr.com/likert/ggplot/barometer/likert-plots/               ##
    ## http://rnotr.com/likert/ggplot/barometer/likert-plotly/              ##
    ##########################################################################
    ## Subset factor variables, gather() and plot these
    factor_vars <- which(sapply(df, class) == 'factor') %>% names()
    ## Logical check required to determine if numeric variables are to be plotted.
    ## If none are specified then left_join() fails...
    results$df_factor <- df %>%
                         dplyr::select(which(sapply(., class) == 'factor'),
                                       !!!to_select) %>%
                         gather(key = variable, value = value, factor_vars)
    if(names(results$df_numeric) %in% c('variable')){
        results$df_factor <- results$df_factor %>%
                             left_join(.,
                                       lookup,
                                       by = c('variable' = 'identifier'))
        ## Remove NAs
        if(remove.na == TRUE){
            results$df_factor <- results$df_factor %>%
                                 dplyr::filter(!is.na(value))
        }
        ## Plot
        ## ToDo : group factor variables based on the form, plot each as 1 x group
        ##        then use gridExtra() to arrange.
        factor_sets <- results$df_factor %>%
                       dplyr::select(form) %>%
                       table() %>%
                       names()
        ## Plot groups of factors based on the Form they are collected on
        results_length_pre <- length(results)
        for(x in factor_sets){
            out <- gsub(' ', '_', x) %>%
                   gsub('\\(', '', .) %>%
                   gsub('\\)', '', .) %>%
                   gsub('-', '_', .) %>%
                   tolower()
            results[[paste0('factor_', out)]] <- results$df_factor %>%
                                                 dplyr::filter(form == x) %>%
                                                 ggplot(aes(x = label, fill = value),
                                                        position = position_stack(reverse = TRUE)) +
                                                 geom_bar(position = 'fill') +
                                                 coord_flip() +
                                                 xlab('') + ylab('Proportion') +
                                                 ggtitle(x) +
                                                 facet_grid(form~group,
                                                            scales = 'free') +
                                                 theme +
                                                 theme(strip.background = element_blank(),
                                                       strip.placement  = 'outside')
        }
        results_length_post <- length(results)
        results$factor <- results$df_factor %>%
                          ggplot(aes(x = label, fill = value),
                                 position = position_stack(reverse = TRUE)) +
                          geom_bar(position = 'fill') +
                          coord_flip() +
                          xlab('') + ylab('Proportion') +
                          ggtitle(title.factor) +
                          facet_grid(form~group,
                                     scales = 'free') +
                          theme +
                          theme(strip.background = element_blank(),
                                strip.placement  = 'outside')
        if(individual == TRUE){
            for(x in factor_vars){
                ## Extract the label
                xlabel <- results$df_factor %>%
                          dplyr::filter(variable == x) %>%
                          dplyr::select(label) %>%
                          unique() %>%
                          as.data.frame()
                ## Plot current variable
                results[[x]] <- results$df_factor %>%
                                dplyr::filter(!is.na(!!quo_group) & variable == x) %>%
                                ggplot(aes(x = label, fill = value)) +
                                geom_bar(position = 'fill') +
                                coord_flip() +
                                xlab(xlabel[[1]]) +
                                ylab('N') +
                                theme
                if(plotly == TRUE){
                    results[[x]] <- results[[x]] %>%
                                    ggplotly()
                }
            }
        }
    }
    ## ToDo : How to plot using Likert, might not need to gather, instead rename using the lookup
    return(results)
}
