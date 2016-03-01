#' Field Descriptions
#'
#' @description Descriptions of fields (variables) in the Dipep dataset
#'
#' @details
#' Variable names are sometimes informative, sometimes cryptic.  To avoid this each data frames
#' variables are described using the database specification produced by Data Management.
#'
#' @param df The data frame who's variables need describing
#' @param fields A data frame that contains the information from the database specification
#'               spreadsheet produced by Data Management. It should have a bare minimum of two
#'               columns the 'Identifier' column (normally column D) and the 'Label' column
#'               (usually column E) and these should be named \code{variable} and
#'               \code{description} respectively.
#'
#' @return Data frame of two columns, \code{variable} listing the variables in the data frame
#'         and \code{description} which provides the description of the variable.
#'
#'
#' @examples
#'
#' ## You will need to download the 'fields' form from the Data Management database
#' ## specification, read it in and rename columns appropriately.
#' fields <- <- read.csv('fields.csv')
#' fields <- dplyr::select(fields, Identifier, Label)
#' names(fields) <- c('variable', 'description')
#' ## Finally because I work with '.' rather than '_' between words convert...
#' fields$variable <- gsub('_', '.', fields$variable)
#' ## Save a description of your variables to a data frame that is part of alist of
#' ## data frames (since there are multiple data frames in a given project)
#' README.variables <- list()
#' README.variables$screening <- fields_prospect(df     = master$screening,
#'                                               fields = fields)
#'
#' 
#' @export
fields_prospect <- function(df      = master$screening,
                            fields  = fields){
    ## Get the dataframes variable names and merge with the fields data frame
    names.df <- names(df) %>%
        data.frame()
    names(names.df) <- c("variable")
    print(names.df)
    names.df <- merge(names.df,
                      fields,
                      by    = 'variable',
                      all.x = TRUE)
    ## Additional fields not documented (e.g. from forms/fields or derived variable)
    ## Add additional fields to this section
    names.df$description[names.df$variable == 'event.name'] <- 'Event Name'
    names.df$description[names.df$variable == 'event.date'] <- 'Event Date'
    names.df$description[names.df$variable == 'form.name']  <- 'Form Name'
    names.df$description[names.df$variable == 'identifier'] <- 'identifier'
    return(names.df)
}

