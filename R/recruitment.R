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
#' @param enrollment Variable that uniquely identifies enrollment (default \code{enrollment_no}).
#' @param facet Number of columns to facet a plot by, if \code{NULL} then no faceting is applied.
#' @param theme ggplot2 theme to apply.
#' @param plotly Logical, to return ggplot2 graphs as plotly objects (default \code{FALSE}).
#'
#' @export
recruitment <- function(df              = master$screening_form,
                        screening       = screening_no,
                        enrollment      = enrolment_no,
                        facet           = NULL,
                        theme           = theme_bw(),
                        plotly          = FALSE,
                        ...){
    ## List to return results
    results <- list()
    ## Parse the supplied options for screening and enrollment
    ## TODO Parse the screening and enrollment options using dplyr-0.6.0 non-standard evaluation
    ## Screening overall...
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
    screened <- rbind(screen_all,
                      screen_site) %>%
                mutate(status = 'Screened')
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
    recruited <- rbind(recruit_all,
                       recruit_site) %>%
                 mutate(status = 'Recruited')
    ## Combine Screening and Recruitment and gather
    results$screened_recruited <- rbind(screened,
                                        recruited) ## %>%
                                  ## gather()
    ## Tables by Month
    results$table_month <- results$screened_recruited %>%
                           mutate(year_month = paste(year(event_date),
                                                     month(event_date),
                                                     sep = '-')) %>%
                           group_by(site, status, year_month) %>%
                           summarise(n = n()) %>%
                           spread(key = status, value = n) %>%
                           mutate(Percent = (Recruited * 100) / Screened) %>%
                           dplyr::select(site, year_month, Screened, Recruited)
    ## Metric : Screening
    ## Site   : All
    results$plot_screen_all <- dplyr::filter(results$screened_recruited,
                                             site == 'All' & status == 'Screened') %>%
                               ggplot(aes(x = event_date, y = sum)) +
                               geom_line() +
                               xlab('Date') + ylab('N') + ggtitle('Screening across all Sites') +
                               guides(color = guide_legend(ncol = 2)) +
                               theme
    ## Metric : Screening
    ## Site   : Site
    results$plot_screen_site <- dplyr::filter(results$screened_recruited,
                                              site != 'All' & status == 'Screened') %>%
                                ggplot(aes(x = event_date, y = sum, colour = site)) +
                                geom_line() +
                                xlab('Date') + ylab('N') + ggtitle('Screening by Site') +
                                theme
    ## Metric : Recruited
    ## Site   : All
    results$plot_recruit_all <- dplyr::filter(results$screened_recruited,
                                              site == 'All' & status == 'Recruited') %>%
                                ggplot(aes(x = event_date, y = sum)) +
                                geom_line() +
                                xlab('Date') + ylab('N') + ggtitle('Recruitment across all Sites') +
                                guides(color = guide_legend(ncol = 2)) +
                                theme
    ## Metric : Recruited
    ## Site   : Site
    results$plot_recruit_site <- dplyr::filter(results$screened_recruited,
                                               site != 'All' & status == 'Recruited') %>%
                                 ggplot(aes(x = event_date, y = sum, colour = site)) +
                                 geom_line() +
                                 xlab('Date') + ylab('N') + ggtitle('Recruitment by Site') +
                                 theme
    ## Metric : Screened and Recruited
    ## Site   : All
    results$t <- results$screened_recruited %>%
                 dplyr::select(-n) %>%
                 spread(key = status, value = sum)
    results$plot_screen_recruit_all <- dplyr::filter(results$t,
                                                     site == 'All') %>%
                                       ggplot() +
                                       geom_line(aes(x = event_date, y = Screened)) +
                                       geom_line(aes(x = event_date, y = Recruited)) +
                               xlab('Date') + ylab('N') + ggtitle('Screening and Recruitment across all Sites') +
                               theme
    ## Metric : Screened and Recruited
    ## Site   : Site
    results$plot_screen_recruit_site <- dplyr::filter(results$t,
                                                     site != 'All') %>%
                                       ggplot() +
                                       geom_line(aes(x = event_date, y = Screened, color = site)) +
                                       geom_line(aes(x = event_date, y = Recruited, color = site)) +
                               xlab('Date') + ylab('N') + ggtitle('Screening and Recruitment across all Sites') +
                               theme
    ## Facet?
    if(!is.null(facet)){
        ## Metric : Screening
        ## Site   : Site
        results$plot_screen_site <- results$plot_screen_site +
                                    facet_wrap(~site, ncol = facet) +
                                    guides(colour = FALSE) +
                                    theme(axis.text.x = element_text(angle = 90))
        ## Metric : Recruitment
        ## Site   : Site
        results$plot_recruit_site <- results$plot_recruit_site +
                                     facet_wrap(~site, ncol = facet) +
                                     guides(colour = FALSE) +
                                     theme(axis.text.x = element_text(angle = 90))
        ## Metric : Screened and Recruited
        ## Site   : All
        results$plot_screen_recruit_site <- results$plot_screen_recruit_site +
                                     facet_wrap(~site, ncol = facet) +
                                     guides(colour = FALSE) +
                                     theme(axis.text.x = element_text(angle = 90))
    }
    ## Plotly?
    if(plotly == TRUE){

    }
    return(results)
}
