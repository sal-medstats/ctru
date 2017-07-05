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
#' @param lookup_fields Data frame with descriptions of variables.  If working with data from
#'               Prospect this should be the imported \code{Fields} worksheet from the
#'               database specification spreadsheet(/GoogleSheet).
#' @param group Variable by which to summarise the data by.
#' @param events Variable defining repeated events.
#' @param position Position adjustment for ggplot2, default \code{'dodge'} avoids overlapping histograms.
#' @param histogram Logical of whether to plot histogram of continuous variables.
#' @param boxplot Logical of whether to plot box-plot of continuous variables.
#' @param individual Logical of whether to plot outcomes indvidually.
#' @param plotly Logical of whether to make \code{ggplotly()} figures.  This is useful if outputing HTML since the embedded figures are zoomable.
#' @param remove_na Logical to remove NA from plotting (only affects factor variables since NA is excluded from continuous plots by default anyway).
#' @param title_continuous Title for faceted histogram plots of continuous variables.
#' @param title_factor TItle for faceted likert plots of factor variables.
#' @param legend Logical of whether to include legend in histogram and box-plots.
#'
#' @export
plot_summary <- function(df               = .,
                         id               = individual_id,
                         select           = c(),
                         lookup_fields    = master$lookups_fields,
                         levels_factor    = c(),
                         group            = group,
                         events           = NULL,
                         theme            = theme_bw(),
                         position         = 'identity',
                         histogram        = TRUE,
                         boxplot          = TRUE,
                         individual       = FALSE,
                         plotly           = FALSE,
                         remove_na        = TRUE,
                         title_continuous = 'Continuous outcomes by treatment group.',
                         title_factor     = 'Factor outcomes by treatment group',
                         legend           = FALSE,
                         ...){
    ## Results to return
    results <- list()
    ## Quote all arguments (see http://dplyr.tidyverse.org/articles/programming.html)
    quo_id     <- enquo(id)
    quo_select <- enquo(select)
    quo_group  <- enquo(group)
    quo_events <- enquo(events)
    ## Since quo_events is used to control subsequent steps if its NULL select()
    ## fails, therefore conditionally build the variables that are to be subset for
    ## subsequent plotting
    to_group <- c(quo_id, quo_group)
    if(!is.null(quo_events)) to_group <- c(to_group, quo_events)
    ## Subset the data and de-duplicate
    df <- df %>%
          ## dplyr::select(!!quo_id, !!quo_select, !!quo_events, !!quo_group) %>%
          dplyr::select(!!!to_group, !!quo_select) %>%
          unique()
    ## Obtain a list of the numeric variables and grouping variables
    numeric_vars <- which(sapply(df, class) == 'numeric') %>% names()
    ## Obtain a list of the factor variables
    factor_vars <- which(sapply(df, class) == 'factor') %>%
                   names()
    ## Remove 'event_name' which should be a factor to assist plotting but on a different axis
    factor_vars <- factor_vars[factor_vars != 'event_name']
    ##########################################################################
    ## Continuous Variables                                                 ##
    ##########################################################################
    ## Subset the continuous variables gather() and bind with the lookup descriptions
    ## so that when plotted the graphs have meaningful titles
    ## Logical check required to determine if numeric variables are to be plotted.
    ## If none are specified then left_join() fails...
    df_numeric_names <- df %>%
                        dplyr::select(which(sapply(., class) == 'numeric'),
                                      !!!to_group) %>%
                        gather(key = variable, value = value, numeric_vars) %>%
                        names()
    ## Need to save the the levels of the event_name
    quo_events_levels <- levels(df$event_name)
    ## paste0('Histogram : ', histogram) %>% print()
    ## names(results$df_numeric) %>% print()
    ## Histogram
    if(histogram == TRUE & c('variable') %in% df_numeric_names){
        ## print('We are going to draw histograms')
        results$df_numeric <- df %>%
                              dplyr::select(which(sapply(., class) == 'numeric'),
                                            !!!to_group) %>%
                              gather(key = variable, value = value, numeric_vars) %>%
                              left_join(.,
                                        lookup_fields,
                                        by = c('variable' = 'identifier')) %>%
                              ## Ensure value is numberic otherwise nothing to plot
                              mutate(value = as.numeric(value))
        ## Generate plot
        results$histogram <- results$df_numeric %>%
                              dplyr::filter(!is.na(!!quo_group)) %>%
                              ggplot(aes_(~value, fill = quo_group)) +
                              geom_histogram(alpha = 0.5, position = position) +
                              xlab('') + ylab('N') +
                              ggtitle(title_continuous) +
                              theme +
                              theme(strip.background = element_blank(),
                                    strip.placement  = 'outside')
        if(legend == FALSE){
            results$histogram <- results$histogram +
                                 guides(fill = FALSE)
        }
        if(plotly == TRUE){
            results$histogram <- results$histogram %>%
                                 ggplotly()
        }
        ## Facetted plot when no events specified
        if(is.null(quo_events)){
            ## names(results$df_numeric) %>% print()
            ## table(results$df_numeric$label) %>% print()
            results$histogram_tiled <- results$histogram +
                                       facet_wrap(~label,
                                                  scales = 'free',
                                                  strip.position = 'bottom')
        }
        else{
            ## print('Grid : ')
            ## table(results$df_numeric$label) %>% print()
            ## paste0('By    : ', quo_events) %>% print()
            results$histogram_tiled <- results$histogram +
                                       facet_grid(label~quo_events,
                                                  scales = 'free')
        }
        if(legend == FALSE){
            results$histogram       <- results$histogram +
                                       guides(fill = FALSE)
            results$histogram_tiled <- results$histogram_tiled +
                                       guides(fill = FALSE)
        }
        if(plotly == TRUE){
            results$histogram       <- results$histogram %>%
                                       ggplotly()
            results$histogram_tiled <- results$histogram_tiled %>%
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
                results[[paste0('histogram_', x)]] <- results$df_numeric %>%
                                dplyr::filter(!is.na(!!quo_group) & variable == x) %>%
                                ggplot(aes_(~value, fill = quo_group)) +
                                geom_histogram(alpha = 0.5, position = position) +
                                ggtitle(xlabel[[1]]) +
                                xlab(xlabel[[1]]) + ylab('N') +
                                ylab('N') +
                                theme
                if(legend == FALSE){
                    results[[paste0('histogram_', x)]] <- results[[paste0('histogram_', x)]] +
                                                          guides(fill = FALSE)
                }
                if(plotly == TRUE){
                    results[[paste0('histogram_', x)]] <- results[[paste0('histogram_', x)]] %>%
                                                          ggplotly()
                }
            }
        }
    }
    ## Boxplot
    ## paste0('Boxplot : ', boxplot) %>% print()
    ## names(results$df_numeric) %>% print()
    if(boxplot == TRUE & c('variable') %in% df_numeric_names){
        ## print('We are going to draw box-plots')
        results$df_numeric <- df %>%
                              dplyr::select(which(sapply(., class) == 'numeric'),
                                            !!!to_group) %>%
                              gather(key = variable, value = value, numeric_vars) %>%
                              left_join(.,
                                        lookup_fields,
                                        by = c('variable' = 'identifier')) %>%
                              ## Ensure value is numberic otherwise nothing to plot
                              mutate(value = as.numeric(value))
        ## Generate plot
        results$boxplot <- results$df_numeric %>%
                              dplyr::filter(!is.na(!!quo_group)) %>%
                              ggplot(aes_(quo_group, ~value, fill = quo_group)) +
                              geom_boxplot() +
                              xlab('') + ylab('N') +
                              ggtitle(title_continuous) +
                              theme +
                              theme(strip.background = element_blank(),
                                    strip.placement  = 'outside')
        ## Facetted plot when no events specified
        if(is.null(quo_events)){
            ## print('Facetting : ')
            ## table(results$df_numeric$label) %>% print()
            results$boxplot_tiled <- results$boxplot +
                                     facet_wrap(~label,
                                                scales = 'free',
                                                strip.position = 'bottom')
        }
        else{
            ## print('Grid : ')
            ## table(results$df_numeric$label) %>% print()
            ## paste0('By    : ', quo_events) %>% print()
            results$boxplot_tiled <- results$boxplot +
                                     ## facet_grid('label'~quo_events,
                                     facet_grid(label~event_name,
                                                scales = 'free')
        }
        if(legend == FALSE){
            results$boxplot       <- results$boxplot +
                                     guides(fill = FALSE)
            results$boxplot_tiled <- results$boxplot_tiled +
                                     guides(fill = FALSE)
        }
        if(plotly == TRUE){
            results$boxplot       <- results$boxplot %>%
                                     ggplotly()
            results$boxplot_tiled <- results$boxplot %>%
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
                results[[paste0('boxplot_', x)]] <- results$df_numeric %>%
                                dplyr::filter(!is.na(!!quo_group) & variable == x) %>%
                                ggplot(aes_(quo_group, ~value, fill = quo_group)) +
                                geom_boxplot() +
                                ## facet_wrap(~event_name, ncol = 1) +
                                ggtitle(xlabel[[1]]) +
                                xlab('Site') + ylab(xlabel[[1]]) +
                                theme +
                                theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
                if(legend == FALSE){
                    results[[paste0('boxplot_', x)]] <- results[[paste0('boxplot_', x)]] +
                                                        guides(fill = FALSE)
                }
                if(plotly == TRUE){
                    results[[paste0('boxplot_', x)]] <- results[[paste0('boxplot_', x)]] %>%
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
    ## Add the event_name variable
    ## factor_vars %>% print()
    ## Filter the factor lookups based on the factor variables so we can re-encode them
    ## Logical check required to determine if numeric variables are to be plotted.
    ## If none are specified then left_join() fails...
    df_factor_names <- df %>%
                       dplyr::select(which(sapply(., class) == 'factor'),
                                     !!!to_group) %>%
                       gather(key = variable, value = value, factor_vars) %>%
                       names()
    ## paste0('Names     : ', names(results$df_factor) %in% c('variable')) %>% print()
    ## names(results$df_factor) %>% print()
    if(c('variable') %in% df_factor_names){
        results$df_factor <- df %>%
                             dplyr::select(which(sapply(., class) == 'factor'),
                                           !!!to_group) %>%
                             gather(key = variable, value = value, factor_vars, factor_key = TRUE) %>%
                             left_join(.,
                                       lookup_fields,
                                       by = c('variable' = 'identifier'))
        ## Convert factors
        if(!is.null(levels_factor)){
            results$df_factor <- results$df_factor %>%
                mutate(value = factor(value,
                                      levels = levels_factor))
        }
        ## Remove NAs
        if(remove_na == TRUE){
            results$df_factor <- results$df_factor %>%
                                 dplyr::filter(!is.na(value))
        }
        ## ToDo : group factor variables based on the form, plot each as 1 x group
        ##        then use gridExtra() to arrange.
        factor_sets <- results$df_factor %>%
                       dplyr::select(form) %>%
                       table() %>%
                       names()
        ## Plot groups of factors based on the Form they are collected on
        ## ToDo : Need to actually split the data frame into multiple ones (stored in a list())
        ##        So that when gather() the levels of responses can be collated and used to
        ##        convert back to factor() automatically, then plot each.
        results_length_pre <- length(results)
        for(x in factor_sets){
            out <- gsub(' ', '_', x) %>%
                   gsub('\\(', '', .) %>%
                   gsub('\\)', '', .) %>%
                   gsub('-', '_', .) %>%
                tolower()
            results[[paste0('factor_', out)]] <- results$df_factor %>%
                                                 dplyr::filter(form == x) %>%
                                                 ## ggplot(aes_string(x = !!!quo_events, fill = 'value'),
                                                 ggplot(aes_string(x = 'event_name', fill = 'value')) +
                                                 geom_bar(position = 'fill') +
                                                 coord_flip() + scale_y_continuous(trans = 'reverse') +
                                                 xlab('') + ylab('Proportion') +
                                                 ggtitle(x) +
                                                 facet_wrap(~label,
                                                            scales = 'free',
                                                            ncol = 1) +
                                                 theme +
                                                 theme(strip.background = element_blank(),
                                                       strip.placement  = 'outside')
            if(legend == FALSE){
                results[[paste0('factor_', out)]] <- results[[paste0('factor_', out)]] +
                                                     guides(fill = FALSE)
            }
            if(plotly == TRUE){
                results[[paste0('factor_', out)]] <- results[[paste0('factor_', out)]] %>%
                                                     ggplotly()
            }
        }
        results_length_post <- length(results)
        results$factor <- results$df_factor %>%
                          ggplot(aes(x = label, fill = value)) +
                          geom_bar(position = 'fill') +
                          coord_flip() +
                          xlab('') + ylab('Proportion') +
                          ggtitle(title_factor) +
                          facet_grid(form~group,
                                     scales = 'free') +
                          theme +
                          theme(strip.background = element_blank(),
                                strip.placement  = 'outside')
        if(legend == FALSE){
            results$factor <- results$factor +
                              guides(fill = FALSE)
        }
        if(plotly == TRUE){
            results$factor <- results$factor %>%
                              ggplotly()
        }
        if(individual == TRUE){
            for(x in factor_vars){
                ## Extract the label
                xlabel <- results$df_factor %>%
                          dplyr::filter(variable == x) %>%
                          dplyr::select(label) %>%
                          unique() %>%
                          as.data.frame()
                ## Plot current variable
                results[[paste0('factor_', x)]] <- results$df_factor %>%
                                dplyr::filter(!is.na(!!quo_group) & variable == x) %>%
                                ggplot(aes(x = label, fill = value)) +
                                geom_bar(position = 'fill') +
                                coord_flip() + scale_y_continuous(trans = 'reverse') +
                                xlab(xlabel[[1]]) +
                                ylab('N') +
                                theme
                if(legend == FALSE){
                    results[[paste0('factor_', x)]] <- results[[paste0('factor_', x)]] +
                                                       guides(fill = FALSE)
                }
                if(plotly == TRUE){
                    results[[paste0('factor_', x)]] <- results[[paste0('factor_', x)]] %>%
                                                       ggplotly()
                }
            }
        }
    }
    ## ToDo : How to plot using Likert, might not need to gather, instead rename using the lookup
    ## ToDo : Time-series like line plots of proportions (y) over time (x)
    return(results)
}
