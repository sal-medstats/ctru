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
ggplot_missing <- function(df = ,
                           exclude = NULL,
                           theme   = theme_bw(),
                           ...){
    ## Remove variables if specified
    if(!is.null(exclude)){
        quo_exclude <- quos(exclude)
        df <- df %>%
              dplyr::select(-quo_exclude)
    }
    ## Assess missing, melt and plot
    df %>%
        is.na() %>%
        melt() %>%
        ggplot(data = ,,
               aes (x = Var2,
                    y = Var1)) +
        geom_raster(aes(fill = value)) +
        scale_fill() +
        theme +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(x = 'Variables in Dataset',
             y = 'Rows / observations')
}
