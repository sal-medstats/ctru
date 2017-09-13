#' Labelling of regression model variables from CTRU Prospect
#'
#' @description Label variables with human readable descriptions
#'
#' @details
#'
#' Variable names and factor levels are often not quite the format required for
#' human readable tables/figures.  This function ameliorates the burdensome task
#' of labelling regression model outputs by taking the \code{master$lookups} for
#' the project and using it to derive meaningful, human readable labels.
#'
#' The \code{Lookups.csv} that is exported from Prospect should have already been
#' read into an R data frame using \code{read_prospect()} from the \code{ctru}
#' package. as should the \code{Fields} tab from the database specification
#' (invariably)
#'
#'
#' @param lookups The \code{Lookups.csv} file as an R data frame. For now it \emph{must} have the variables \code{field} (denoting the variable name) and \code{label} denoting the possible levels of factor variables
#' @param df Data frame to label, this might be a user generated tabulation, or tidy regression results produced by \code{broom}.
#'
#' @export
label <- function(fields          = fields,
                  lookups         = lookups,
                  df              = .data,
                  ...){
    ## Combine field labels with factor variable lookups and factor levels with
    ## the label.  Its a shame Prospect doesn't use consistent nomenclature so
    ## a rename is required first
    names(fields) <- gsub('identifier', 'field', names(fields))
    names(fields) <- gsub('label', 'description', names(fields))
    names(lookups) <- gsub('label', 'levels', names(lookups))
    ## Combine the key fields and subset for merging with supplied df
    lookups <- left_join(dplyr::select(fields, field, description),
                         dplyr::select(lookups, field, levels),
                         by = c('field')) %>%
               mutate(term  = paste0(field, levels),
                      term  = gsub('NA', '', term),
                      label = paste(description,
                                    levels,
                                    sep = ' : '),
                      label = gsub(' : NA', '', label)) %>%
               dplyr::select(term, label)
    if(!is.null(df)){
        df <- df %>%
              left_join(.,
                        lookups) %>%
              mutate(label = ifelse(term == '(Intercept)',
                                    yes  =  term,
                                    no   =  label))
    }
    return(df)
}
