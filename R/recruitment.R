#' Summarise Screening and Recruitment
#'
#' @description Plot and tabulate aspects of screening and recruitment overall and by site
#'
#' @details
#'
#' Many studies wish to understand screening and recruitment of participants.  This function
#' takes the default \code{Screening Form.csv} output by Prospect and summarises screening
#' and recruitment across the whole site and by study site (NB - You should export your data
#' from Prospect with the 'Include site column in events/forms/subforms' option selected).
#'
#' For now the variable names are hard coded to match those used in Prospect, future updates
#' might revise this to allow user specified options.
#'
#'
#' @param df Data frame to summarise, default is \code{master$screening_form}
#' @param screening Variable that uniquely identifies screening (default \code{screening_no}).
#' @param enrolment Variable that uniquely identifies enrolment (default \code{randomisation}).
#' @param plot.by Plot overall (\code{all}) or by site (\code{site}).
#' @param facet.col Number of columns to facet a plot by, if \code{NULL} then no faceting is applied.
#' @param facet.scales \code{free} or \code{fixed} axis scales.
#' @param strip.position Position of titles when faceting passed to ggplot, see \code{?facet_wrap} for options.
#' @param theme ggplot2 theme to apply.
#' @param plotly Logical, to return ggplot2 graphs as plotly objects (default \code{FALSE}).
#'
#' @export
recruitment <- function(df              = master$screening_form,
                        screening       = screening_no,
                        enrolment       = randomisation,
                        plot.by         = 'both',
                        facet.col       = NULL,
                        facet.scales    = 'free',
                        strip.position  = NULL,
                        theme           = theme_bw(),
                        plotly          = FALSE,
                        ...){
    ## List to return results
    results <- list()
    ## Parse the supplied options for screening and enrolment
    ## TODO Parse the screening and enrolment options using dplyr-0.6.0 non-standard evaluation
    ##      Should allow to set options to group_by() when tabulating and for plotting
    ##      so that one data set is produced conditional on options and then only one set of
    ##      table/ploting is required leaving less code to maintain
    ## Summarise screend and recruited regardless of what has been requested and then
    ## conditionally use what is requested
    screen_all <- df %>%
                  group_by(event_date) %>%
                  summarise(n = n()) %>%
                  mutate(sum  = cumsum(n),
                         site      = 'All') %>%
                  as.data.frame()
    ## Screening by site
    screen_site <- df %>%
                   group_by(site, event_date) %>%
                   summarise(n = n()) %>%
                   mutate(sum  = cumsum(n)) %>%
                   ungroup() %>%
                   mutate(site = gsub('Hospital', '', site))
    results$screened <- bind_rows(screen_all,
                                  screen_site) %>%
                        mutate(status = 'Screened') %>%
                        as.data.frame()
    ## Empty strings imported for default 'randomisation' ensure these are
    ## NA
    df <- df %>%
          mutate(randomisation = ifelse(randomisation == '',
                                       yes = NA,
                                       no  = randomisation))
    ## Recruitment overall
    recruit_all <- df %>%
                   dplyr::filter(!is.na(randomisation)) %>%
                   group_by(event_date) %>%
                   summarise(n = n()) %>%
                   mutate(sum  = cumsum(n),
                          site       = 'All') %>%
                   as.data.frame()
    ## Recruitment by site
    recruit_site <- df %>%
                    dplyr::filter(!is.na(randomisation)) %>%
                    group_by(site, event_date) %>%
                    summarise(n = n()) %>%
                    mutate(sum  = cumsum(n)) %>%
                    ungroup() %>%
                    mutate(site = gsub('Hospital', '', site))
    results$recruited <- bind_rows(recruit_all,
                                   recruit_site) %>%
                         mutate(status = 'Recruited') %>%
                         as.data.frame()
    rm(screen_all, screen_site, recruit_all, recruit_site)
    ## Short function to reduce burden
    summarise_by <- function(df,
                             site   = 'Site',
                             date   = 'Date',
                             n      = 'N',
                             total  = 'Total',
                             status = FALSE){
        if(status == FALSE)      group <- quos(site, year_month)
        else if(status == TRUE)  group <- quos(site, year_month, status)
        results <- df %>%
                   mutate(year_month = paste(year(event_date),
                                             month(event_date),
                                             '01',
                                             sep = '-') %>% as.Date()) %>%
            ## group_by(site, year_month) %>%
                   group_by(!!!group) %>%
                   summarise(n = sum(n, na.rm = TRUE)) %>%
                   ungroup() %>%
                   mutate(sum = cumsum(n))
        names(results) <- gsub('site',       site,  names(results))
        names(results) <- gsub('year_month', date,  names(results))
        names(results) <- gsub('n',          n,     names(results))
        names(results) <- gsub('sum',        total, names(results))
        return(results)
    }
    ## Tabulate and plot Screening
    if(!is.null(screening)){
        ## Tables by Month
        if(plot.by %in% c('all', 'both')){
            results$table_screened_all_month <- dplyr::filter(results$screened, site == 'All') %>%
                                                summarise_by(site   = 'Site',
                                                             date   = 'Date',
                                                             n      = 'Screened',
                                                             total  = 'Total Screened',
                                                             status = FALSE)
        }
        if(plot.by %in% c('site', 'both')){
            results$table_screened_site_month <- dplyr::filter(results$screened, site != 'All') %>%
                                                 summarise_by(site  = 'Site',
                                                              date  = 'Date',
                                                              n     = 'Screened',
                                                              total = 'Total Screened',
                                                              status = FALSE)
        }
        if(plot.by %in% c('both')){
            results$table_screened_month <- bind_rows(results$table_screened_all_month,
                                                      results$table_screened_site_month)
        }
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Screening
            ## Site   : All
            results$plot_screened_all <- dplyr::filter(results$screened, site == 'All') %>%
                                         ggplot(aes(x = event_date, y = sum)) +
                                         geom_line() +
                                         xlab('Date') + ylab('N') + ggtitle('Recruitment across all Sites') +
                                         guides(color = guide_legend(ncol = 2)) +
                                         theme
        }
        if(plot.by %in% c('site', 'both')){
            ## Metric : Screening
            ## Site   : Site
            results$plot_screened_site <- dplyr::filter(results$screened, site != 'All') %>%
                                          ggplot(aes(x = event_date, y = sum, colour = site)) +
                                          geom_line() +
                                          xlab('Date') + ylab('N') + ggtitle('Recruitment by Site') +
                                          theme
        }
    }
    ## Recruitment
    if(!is.null(enrolment)){
        ## Tables by Month
        if(plot.by %in% c('all', 'both')){
            results$table_recruited_all_month <- dplyr::filter(results$recruited, site == 'All') %>%
                                                 summarise_by(site  = 'Site',
                                                              date  = 'Date',
                                                              n     = 'Recruited',
                                                              total = 'Total Recruited',
                                                              status = FALSE)
        }
        if(plot.by %in% c('site', 'both')){
            results$table_recruited_site_month <- dplyr::filter(results$recruited, site != 'All') %>%
                                                  summarise_by(site  = 'Site',
                                                               date  = 'Date',
                                                               n     = 'Recruited',
                                                               total = 'Total Recruited',
                                                               status = FALSE)
        }
        if(plot.by %in% c('both')){
            results$table_recruited_month <- bind_rows(results$table_recruited_all_month,
                                                       results$table_recruited_site_month)
        }
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Recruited
            ## Site   : All
            results$plot_recruited_all <- dplyr::filter(results$recruited, site == 'All') %>%
                                          ggplot(aes(x = event_date, y = sum)) +
                                          geom_line() +
                                          xlab('Date') + ylab('N') + ggtitle('Recruitment across all Sites') +
                                          guides(color = guide_legend(ncol = 2)) +
                                          theme
        }
        if(plot.by %in% c('site', 'both')){
            ## Metric : Recruited
            ## Site   : Site
            results$plot_recruited_site <- dplyr::filter(results$recruited, site != 'All') %>%
                                           ggplot(aes(x = event_date, y = sum, colour = site)) +
                                           geom_line() +
                                           xlab('Date') + ylab('N') + ggtitle('Recruitment by Site') +
                                           theme
        }
    }
    if(!is.null(screening) & !is.null(enrolment)){
        ## Combine the formatted tabular screening and recruitment output and derive percentages
        results$table_screened_recruited_month <- full_join(results$table_screened_month,
                                                            results$table_recruited_month,
                                                            by = c('Site', 'Date')) %>%
                                                  mutate(`Percent Recruited` = (Recruited * 100) / Screened,
                                                         `Cumulative Percent Recruited` = (`Total Recruited` * 100) / `Total Screened`)
        ## Bind the unformatted dataframes for plotting
        results$screened_recruited <- bind_rows(results$screened,
                                                results$recruited)
        results$screened_recruited <- results$screened_recruited %>%
                                      dplyr::select(-n) %>%
                                      spread(key = status, value = sum)
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screened_recruited_all <- dplyr::filter(results$screened_recruited,
                                                                 site == 'All') %>%
                                                   ggplot() +
                                                   geom_line(aes(x = event_date,
                                                                 y = Screened),
                                                             linetype = 'dashed') +
                                                   geom_line(aes(x = event_date,
                                                                 y = Recruited),
                                                             linetype = 'solid') +
                                                   xlab('Date') + ylab('N') +
                                                   ggtitle('Screening and Recruitment across all Sites') +
                                                   theme
        }
        if(plot.by %in% c('site', 'both')){
            ## Metric : Screened and Recruited
            ## Site   : Site
            results$plot_screened_recruited_site <- dplyr::filter(results$screened_recruited,
                                                                  site != 'All') %>%
                                                    ggplot() +
                                                    geom_line(aes(x = event_date,
                                                                  y = Screened, color = site),
                                                              linetype = 'dashed') +
                                                    geom_line(aes(x = event_date,
                                                                  y = Recruited, color = site),
                                                              linetype = 'solid') +
                                                    xlab('Date') + ylab('N') +
                                                    ggtitle('Screening and Recruitment across all Sites') +
                                                    theme
        }
    }
    ## Facet?
    if(!is.null(facet.col)){
        if(!is.null(screening)){
            ## Metric : Screening
            ## Site   : Site
            results$plot_screened_site <- results$plot_screened_site +
                                          facet_wrap(~site,
                                                     ncol = facet.col,
                                                     scales = facet.scales,
                                                     strip.position = strip.position) +
                                          guides(colour = FALSE) +
                                          theme(axis.text.x = element_text(angle = 90))
            sites <- results$screened$site %>% unique()
            results$plot_screened_each_site <- vector('list', length(sites))
            for(x in sites){
                out <- gsub(' $', '', x) %>%
                       gsub(' ', '_', .) %>%
                       gsub('\\. ', '', .) %>%
                       gsub("'", '', .) %>%
                       gsub('__', '_', .) %>%
                    tolower()
                results$plot_screened_each_site[[out]] <- dplyr::filter(results$screened,
                                                                         site == x) %>%
                                                           ggplot() +
                                                           geom_line(aes(x = event_date,
                                                                         y = sum, color = site),
                                                                     linetype = 'dashed') +
                                                           xlab('Date') + ylab('N') +
                                                           ggtitle(paste0('Screening at ', x)) +
                                                           theme +
                                                           theme(legend.position = 'none')
                if(plotly == TRUE){
                    results$plot_screened_each_site[[out]] <- ggplotly(results$plot_screened_each_site[[out]])
                }
            }
        }
        if(!is.null(enrolment)){
            ## Metric : Recruitment
            ## Site   : Site
            results$plot_recruited_site <- results$plot_recruited_site +
                                           facet_wrap(~site,
                                                      ncol = facet.col,
                                                      scales = facet.scales,
                                                      strip.position = strip.position) +
                                           guides(colour = FALSE) +
                                           theme(axis.text.x = element_text(angle = 90))
            sites <- results$recruited$site %>% unique()
            results$plot_recruited_each_site <- vector('list', length(sites))
            for(x in sites){
                out <- gsub(' $', '', x) %>%
                       gsub(' ', '_', .) %>%
                       gsub('\\. ', '', .) %>%
                       gsub("'", '', .) %>%
                       gsub('__', '_', .) %>%
                       tolower()
                results$plot_recruited_each_site[[out]] <- dplyr::filter(results$recruited,
                                                                         site == x) %>%
                                                           ggplot() +
                                                           geom_line(aes(x = event_date,
                                                                         y = sum, color = site),
                                                                     linetype = 'dashed') +
                                                           xlab('Date') + ylab('N') +
                                                           ggtitle(paste0('Screening at ', x)) +
                                                           theme +
                                                           theme(legend.position = 'none')
                if(plotly == TRUE){
                    results$plot_recruited_each_site[[out]] <- ggplotly(results$plot_recruited_each_site[[out]])
                }
            }
        }
        if(!is.null(screening) & !is.null(enrolment)){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screened_recruited_site <- results$plot_screened_recruited_site +
                                                    facet_wrap(~site,
                                                               ncol = facet.col,
                                                               scales = facet.scales,
                                                               strip.position = strip.position) +
                                                guides(colour = FALSE) +
                                                theme(axis.text.x = element_text(angle = 90))
            sites <- results$screened_recruited$site %>% unique()
            results$plot_screened_recruited_each_site <- vector('list', length(sites))
            for(x in sites){
                out <- gsub(' $', '', x) %>%
                       gsub(' ', '_', .) %>%
                       gsub('\\. ', '', .) %>%
                       gsub("'", '', .) %>%
                       gsub('__', '_', .) %>%
                       tolower()
                results$plot_screened_recruited_each_site[[out]] <- dplyr::filter(results$screened_recruited,
                                                                         site == x) %>%
                                                           ggplot() +
                                                           geom_line(aes(x = event_date,
                                                                         y = Screened, color = site),
                                                                     linetype = 'dashed') +
                                                           geom_line(aes(x = event_date,
                                                                         y = Recruited, color = site),
                                                                     linetype = 'solid') +
                                                           xlab('Date') + ylab('N') +
                                                           ggtitle(paste0('Screening and Recruitment at ', x)) +
                                                           theme +
                                                           theme(legend.position = 'none')
                if(plotly == TRUE){
                    results$plot_screened_recruited_each_site[[out]] <- ggplotly(results$plot_screened_recruited_each_site[[out]])
                }
            }
        }
    }
    ## Plotly?
    if(plotly == TRUE){
        if(!is.null(screening)){
            if(plot.by %in% c('all', 'both')){
                ## Metric : Screening
                ## Site   : All
                results$plotly_screened_all <- ggplotly(results$plot_screened_site)
            }
            if(plot.by %in% c('site', 'both')){
                ## Metric : Screening
                ## Site   : Site
                results$plotly_screened_site <- ggplotly(results$plot_screened_site)
            }
        }
        if(!is.null(enrolment)){
            if(plot.by %in% c('all', 'both')){
                ## Metric : Recruitment
                ## All   : All
                results$plotly_recruited_all <- ggplotly(results$plot_recruited_all)
            }
            if(plot.by %in% c('site', 'both')){
                ## Metric : Recruitment
                ## Site   : Site
                results$plotly_recruited_site <- ggplotly(results$plot_recruited_site)
            }
        }
        if(!is.null(screening) & !is.null(enrolment)){
            if(plot.by %in% c('all', 'both')){
                ## Metric : Screened and Recruited
                ## Site   : All
                results$plotly_screened_recruited_all <- ggplotly(results$plot_screened_recruited_all)
            }
            if(plot.by %in% c('site', 'both')){
                ## Metric : Screened and Recruited
                ## Site   : Site
                results$plotly_screened_recruited_site <- ggplotly(results$plot_screened_recruited_site)
            }
        }
    }
    return(results)
}
