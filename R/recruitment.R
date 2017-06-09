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
#' @param facet Number of columns to facet a plot by, if \code{NULL} then no faceting is applied.
#' @param theme ggplot2 theme to apply.
#' @param plotly Logical, to return ggplot2 graphs as plotly objects (default \code{FALSE}).
#'
#' @export
recruitment <- function(df              = master$screening_form,
                        screening       = screening_no,
                        enrolment       = enrolment_no,
                        plot.by         = 'both',
                        facet           = NULL,
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
    ## Screening overall...
    if(!is.null(screening)){
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
                       mutate(site = gsub('Hospital', '', site)) %>%
                       as.data.frame()
        results$screened <- rbind(screen_all,
                                  screen_site) %>%
                            mutate(status = 'Screened')
        ## Tables by Month
        if(plot.by %in% c('all')){
            results$table_screened_month <- dplyr::filter(results$screened, site == 'All') %>%
                                            mutate(year_month = paste(year(event_date),
                                                                      month(event_date),
                                                                      sep = '-')) %>%
                                            group_by(site, year_month) %>%
                                            summarise(Screened = n())
        }
        else if(plot.by %in% c('site')){
            results$table_screened_month <- dplyr::filter(results$screened, site != 'All') %>%
                                            mutate(year_month = paste(year(event_date),
                                                                      month(event_date),
                                                                      sep = '-')) %>%
                                            group_by(site, year_month) %>%
                                            summarise(Screened = n())
        }
        else if(plot.by %in% c('both')){
            results$table_screened_month <- results$screened %>%
                                            mutate(year_month = paste(year(event_date),
                                                                      month(event_date),
                                                                      sep = '-')) %>%
                                            group_by(site, year_month) %>%
                                            summarise(Screened = n())
        }
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Screening
            ## Site   : All
            results$plot_screened_all <- results$screened %>%
                                       ggplot(aes(x = event_date, y = sum)) +
                                       geom_line() +
                                       xlab('Date') + ylab('N') + ggtitle('Recruitment across all Sites') +
                                       guides(color = guide_legend(ncol = 2)) +
                                       theme
        }
        else if(plot.by %in% c('site', 'both')){
            ## Metric : Screening
            ## Site   : Site
            results$plot_screened_site <- results$screened %>%
                                           ggplot(aes(x = event_date, y = sum, colour = site)) +
                                           geom_line() +
                                           xlab('Date') + ylab('N') + ggtitle('Recruitment by Site') +
                                           theme
        }
    }
    ## Recruitment
    if(!is.null(enrolment)){
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
                        mutate(site = gsub('Hospital', '', site)) %>%
                        as.data.frame()
        results$recruited <- rbind(recruit_all,
                                   recruit_site) %>%
                             mutate(status = 'Recruited')
        ## Tables by Month
        if(plot.by %in% c('all')){
            results$table_recruited_month <- dplyr::filter(results$recruited, site == 'All') %>%
                                             mutate(year_month = paste(year(event_date),
                                                                       month(event_date),
                                                                       sep = '-')) %>%
                                             group_by(site, year_month) %>%
                                             summarise(Recruited = n())
        }
        if(plot.by %in% c('site')){
            results$table_recruited_month <- dplyr::filter(results$recruited, site != 'All') %>%
                                             mutate(year_month = paste(year(event_date),
                                                                       month(event_date),
                                                                       sep = '-')) %>%
                                             group_by(site, year_month) %>%
                                             summarise(Recruited = n())
        }
        else if(plot.by %in% c('both')){
            results$table_recruited_month <- results$recruited %>%
                                             mutate(year_month = paste(year(event_date),
                                                                       month(event_date),
                                                                       sep = '-')) %>%
                                             group_by(site, year_month) %>%
                                             summarise(Recruited = n())
        }
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Recruited
            ## Site   : All
            results$plot_recruited_all <- results$recruited %>%
                                       ggplot(aes(x = event_date, y = sum)) +
                                       geom_line() +
                                       xlab('Date') + ylab('N') + ggtitle('Recruitment across all Sites') +
                                       guides(color = guide_legend(ncol = 2)) +
                                       theme
        }
        else if(plot.by %in% c('site', 'both')){
            ## Metric : Recruited
            ## Site   : Site
            results$plot_recruited_site <- results$recruited %>%
                                           ggplot(aes(x = event_date, y = sum, colour = site)) +
                                           geom_line() +
                                           xlab('Date') + ylab('N') + ggtitle('Recruitment by Site') +
                                           theme
        }
    }
    if(!is.null(screening) & !is.null(enrolment)){
        ## Combine Screening and Recruitment and gather
        results$screened_recruited <- rbind(results$screened,
                                            results$recruited) ## %>%
                                      ## gather()
        ## Tables by Month
        results$table_screened_recruited_month <- results$screened %>%
                                                  mutate(year_month = paste(year(event_date),
                                                                            month(event_date),
                                                                            sep = '-')) %>%
                                                  group_by(site, status, year_month) %>%
                                                  summarise(n = n()) %>%
                                                  spread(key = status, value = n) %>%
                                                  mutate(Percent = (Recruited * 100) / Screened) %>%
                                                  dplyr::select(site, year_month, Screened, Recruited)
        results$screened_recruited <- results$screened_recruited %>%
                                      dplyr::select(-n) %>%
                                      spread(key = status, value = sum)
        ## Plot
        if(plot.by %in% c('all', 'both')){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screen_recruit_all <- dplyr::filter(results$screened_recruited,
                                                             site == 'All') %>%
                                               ggplot() +
                                               geom_line(aes(x = event_date, y = Screened)) +
                                               geom_line(aes(x = event_date, y = Recruited)) +
                                               xlab('Date') + ylab('N') + ggtitle('Screening and Recruitment across all Sites') +
                                               theme
        }
        if(plot.by %in% c('site', 'both')){
            ## Metric : Screened and Recruited
            ## Site   : Site
            results$plot_screen_recruit_site <- dplyr::filter(results$screened_recruited,
                                                              site != 'All') %>%
                                                ggplot() +
                                                geom_line(aes(x = event_date, y = Screened, color = site)) +
                                                geom_line(aes(x = event_date, y = Recruited, color = site)) +
                                                xlab('Date') + ylab('N') + ggtitle('Screening and Recruitment across all Sites') +
                                                theme
        }
    }
    ## Facet?
    if(!is.null(facet)){
        if(!is.null(screening)){
            ## Metric : Screening
            ## Site   : Site
            results$plot_screen_site <- results$plot_screen_site +
                                        facet_wrap(~site, ncol = facet) +
                                        guides(colour = FALSE) +
                                        theme(axis.text.x = element_text(angle = 90))
        }
        if(!is.null(enrolment)){
            ## Metric : Recruitment
            ## Site   : Site
            results$plot_recruit_site <- results$plot_recruit_site +
                                         facet_wrap(~site, ncol = facet) +
                                         guides(colour = FALSE) +
                                         theme(axis.text.x = element_text(angle = 90))
        }
        if(!is.null(screening) & !is.null(enrolment)){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screen_recruit_site <- results$plot_screen_recruit_site +
                                                facet_wrap(~site, ncol = facet) +
                                                guides(colour = FALSE) +
                                                theme(axis.text.x = element_text(angle = 90))
        }
    }
    ## Plotly?
    if(plotly == TRUE){
        if(!is.null(screening)){
            ## Metric : Screening
            ## Site   : Site
            results$plot_screen_site <- ggplotly(results$plot_screen_site)
        }
        if(!is.null(enrolment)){
            ## Metric : Recruitment
            ## Site   : Site
            results$plot_recruit_site <- ggplotly(results$plot_recruit_site)
        }
        if(!is.null(screening) & !is.null(enrolment)){
            ## Metric : Screened and Recruited
            ## Site   : All
            results$plot_screen_recruit_site <- ggplotly(results$plot_screen_recruit_site)
        }
    }
    return(results)
}
