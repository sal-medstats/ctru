#' Plot patterns of missing data using ggplot2
#'
#' @description Plot missing data.
#'
#' @details It can be useful to visualise patterns of missing data prior to imputation.
#' This function does so using ggplot.  This is an extension of a function from
#' Nicholas Tierney with my own customisations (e.g. ability to explicitly exclude
#' some variables).
#'
#' @param df Data frame to summarise.
#' @param exclude Variables to exclude from assessment.
#' @param id Variable that uniquely identifies individuals.
#' @param event Optional variable that defines events if there are repeated measurements.  If provided the plot is faceted by event.
#' @param site Optional variable that defines sites if there are multiple sites (as it may be of interest to know if a site systematically failed to collect data).  If provided the plot is faceted by site in conjunction with any facetting that is triggered by \code{event}.
#' @param labels Data frame that lists the variable description.
#' @param theme ggplot2 theme to apply.
#'
#' @return A ggplot2 object that can be further tweaked.
#'
#' @examples
#'
#' ## Run mortality analyses for Bishop Auckland and its matched site
#' ## producing time-series plot, step and dose models using both the
#' ## panelAR and prais package.
#'
#' ts.mortality <- closed_regress(df       = ,)
#'
#'
#' @references
#'
#' https://njtierney.github.io/r/missing%20data/rbloggers/2015/12/01/ggplot-missing-data/
#'
#' @export
ggplot_missing <- function(df      = .data,
                           exclude = NULL,
                           id      = individual_id,
                           event   = event_name,
                           site    = site,
                           labels  = master$lookups_fields,
                           theme   = theme_bw(),
                           ...){
    ## Results
    results <- list()
    event_quo <- enquo(event)
    id_quo    <- enquo(id)
    site_quo  <- enquo(site)
    ## Build a list of the quoted variables
    to_keep <- c(event_quo, id_quo, site_quo)
    ## Remove variables if specified
    if(!is.null(exclude)){
        quo_exclude <- quos(exclude)
        df <- df %>%
              dplyr::select(-quo_exclude)
    }
    ## Assess missing and melt
    to_keep %>% print()
    results$df <- df %>%
                  gather(key = identifier, value = value, -(!!to_keep)) %>%
                  mutate(missing = is.na(value),
                         id = factor(id))
    ## Apply labels if supplied
    if(!is.null(master$lookups_fields)){
        results$df <- left_join(df,
                                labels)
    }
    ## Plot!
    results$missing_plot <- ggplot(data = ,,
                                   aes(x    = label,
                                       y    = id,
                                       fill = missing)) +
                            geom_raster() +
                            theme +
                            theme(axis.text.x = element_text(angle = 45)) +
                            labs(x = 'Variables in Dataset',
                                 y = 'Rows / observations')
    return(results)
}
