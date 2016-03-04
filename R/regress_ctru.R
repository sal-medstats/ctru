#' Regression Models in Clinical Trials
#'
#' @description Run ITT and PP regression models.
#'
#' @details
#' Run Intention To Treat (ITT) and Per-Protocol (PP) regression models on the specified
#' data with the specified model.  Results are returned for both models and optionally a
#' Stargazer table is produced in the user specified format(s).
#'
#' 
#' @param df Data frame containing data.
#' @param pp Variable defining Per-Protocol compliance.
#' @param outcome Outcome variable.
#' @param predictors A list of predictor variables.
#' @param cluster Variable around which to cluster random effects (if required).
#' @param regress The regression function to use.
#' @param latex Produce a LaTeX formatted table of results using Stargazer.
#' @param html Produce a HTML formatted table of results using Stargazer.
#' @param ascii Produce a ASCII formatted table of results using Stargazer.
#' @param caption Caption for Stargazer table.
#' @param label Label for Stargazer table (only used in LaTeX tables).
#'
#' @return A list containing the model fit from the ITT analyses (\code{$itt}), the model
#'         fit from PP analyses (\code{$pp}) and optionally LaTeX (\code{$latex}), HTML
#'         (\code{$html}) or ASCII (\code{$ascii}) Stargazer tables.
#'
#' @examples
#'
#' @export
regress_ctru <- function(df          = test,
                         pp          = 'pp',
                         outcome     = 'outcome',
                         predictors  = c('age', 'gender', 'allocation'),
                         cluster     = NA,
                         regress     = 'lm',
                         link        = 'binomial',
                         latex       = TRUE,
                         html        = FALSE,
                         ascii       = FALSE,
                         caption     = 'ITT and PP analyses of the primary outcome.',
                         label       = 'regress-primary',
                         ...){
    ## Initialise results list
    results <- list()
    ## Parse the equation without clustering
    if(is.na(cluster)){
        .formula <- reformulate(response   = outcome,
                                termlabels = predictors)
    }
    ## ...and with clustering
    else{
        .formula <- reformulate(response   = outcome,
                                termlabels = c(predictors, paste0("(1 |", cluster, ")")))
    }
    if(regress == 'lm'){
        results$itt <- lm(data    = df,
                          formula = .formula)
        results$pp <- dplyr::filter(df,
                                    pp = "Per-Protocol") %>%
                      lm(formula = .formula)
    }
    else if(resgress == 'glmr'){
        
    }
    ## Combine results into Stargazer tables of different formats
    if(latex == TRUE){
        results$stargazer.latex <- stargazer(list(results$itt, results$pp),
                                             type          = 'latex',
                                             column.labels = c("ITT", "PP"),
                                             ci            = TRUE)
    }
    if(html == TRUE){
        
    }
    if(ascii == TRUE){
        
    }
    return(results)
}
