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
#' @param enrolment Variable that uniquely identifies enrolment (default \code{enrolment_no}).
#' @param plot.by Plot overall (\code{all}) or by site (\code{site}).
#' @param facet.col Number of columns to facet a plot by, if \code{NULL} then no faceting is applied.
#' @param facet.scales \code{free} or \code{fixed} axis scales.
#' @param theme ggplot2 theme to apply.
#' @param plotly Logical, to return ggplot2 graphs as plotly objects (default \code{FALSE}).
#'
#' @export
recruitment <- function(df              = master$screening_form,
                        screening       = screening_no,
                        enrolment       = enrolment_no,
                        plot.by         = 'both',
                        facet.col       = NULL,
                        facet.scales    = 'free',
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
    ## Empty strings imported for default 'enrolment_no' ensure these are
    ## NA
    df <- df %>%
          mutate(enrolment_no = ifelse(enrolment_no == '',
                                       yes = NA,
                                       no  = enrolment_no))
    ## Recruitment overall
    recruit_all <- df %>%
                   dplyr::filter(!is.na(enrolment_no)) %>%
                   group_by(event_date) %>%
                   summarise(n = n()) %>%
                   mutate(sum  = cumsum(n),
                          site       = 'All') %>%
                   as.data.frame()
    ## Recruitment by site
    recruit_site <- df %>%
                    dplyr::filter(!is.na(enrolment_no)) %>%
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
    ## Tabulate and plot Screening
    if(!is.null(screening)){
        ## Tables by Month
        if(plot.by %in% c('all', 'both')){
            results$table_screened_all_month <- dplyr::filter(results$screened, site == 'All') %>%
                                            mutate(year_month = paste(year(event_date),
                                                                      month(event_date),
                                                                      sep = '-')) %>%
                                            group_by(site, year_month) %>%
                                            summarise(Screened = sum(sum, na.rm = TRUE)) %>%
                                            ungroup()
            names(results$table_screened_all_month) <- gsub('site', 'Site', names(results$table_screened_all_month))
            names(results$table_screened_all_month) <- gsub('year_month', 'Date', names(results$table_screened_all_month))
        }
        if(plot.by %in% c('site', 'both')){
            results$table_screened_site_month <- dplyr::filter(results$screened, site != 'All') %>%
                                            mutate(year_month = paste(year(event_date),
                                                                      month(event_date),
                                                                      sep = '-')) %>%
                                            group_by(site, year_month) %>%
                                            summarise(Screened = sum(sum, na.rm = TRUE)) %>%
                                            ungroup()
            names(results$table_screened_site_month) <- gsub('site', 'Site', names(results$table_screened_site_month))
            names(results$table_screened_site_month) <- gsub('year_month', 'Date', names(results$table_screened_site_month))
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
                                                 mutate(year_month = paste(year(event_date),
                                                                           month(event_date),
                                                                           sep = '-')) %>%
                                                 group_by(site, year_month) %>%
                                                 summarise(Recruited = sum(sum, na.rm = TRUE)) %>%
                                                 ungroup()
            names(results$table_recruited_all_month) <- gsub('site', 'Site', names(results$table_recruited_all_month))
            names(results$table_recruited_all_month) <- gsub('year_month', 'Date', names(results$table_recruited_all_month))
        }
        if(plot.by %in% c('site', 'both')){
            results$table_recruited_site_month <- dplyr::filter(results$recruited, site != 'All') %>%
                                                  mutate(year_month = paste(year(event_date),
                                                                            month(event_date),
                                                                            sep = '-')) %>%
                                                  group_by(site, year_month) %>%
                                                  summarise(Recruited = sum(sum, na.rm = TRUE)) %>%
                                                  ungroup()
            names(results$table_recruited_site_month) <- gsub('site', 'Site', names(results$table_recruited_site_month))
            names(results$table_recruited_site_month) <- gsub('year_month', 'Date', names(results$table_recruited_site_month))
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
        ## Combine Screening and Recruitment and gather
        results$screened_recruited <- bind_rows(results$screened,
                                                results$recruited)
        ## Tables by Month
        results$table_screened_recruited_month <- results$screened_recruited %>%
                                                  mutate(year_month = paste(year(event_date),
                                                                            month(event_date),
                                                                            sep = '-')) %>%
                                                  group_by(site, status, year_month) %>%
                                                  summarise(n = sum(sum, na.rm = TRUE)) %>%
                                                  spread(key = status, value = n) %>%
                                                  mutate(Percent = (Recruited * 100) / Screened)
        ## dplyr::select(site, year_month, Screened, Recruited)
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
                                          facet_wrap(~site, ncol = facet.col, scales = facet.scales) +
                                          guides(colour = FALSE) +
                                          theme(axis.text.x = element_text(angle = 90))
        }
        if(!is.null(enrolment)){
            ## Metric : Recruitment
            ## Site   : Site
            results$plot_recruited_site <- results$plot_recruited_site +
                                           facet_wrap(~site, ncol = facet.col, scales = facet.scales) +
                                           guides(colour = FALSE) +
                                           theme(axis.text.x = element_text(angle = 90))
        }
        if(!is.null(screening) & !is.null(enrolment)){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screened_recruited_site <- results$plot_screened_recruited_site +
                                                facet_wrap(~site, ncol = facet.col, scales = facet.scales) +
                                                guides(colour = FALSE) +
                                                theme(axis.text.x = element_text(angle = 90))
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
