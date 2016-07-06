#' Label SAS exports from Prospect
#'
#' @description Label factor variables of datasets exported in SAS format from Prospect
#'
#' @details
#' Case Report Forms are stored in the Prospect Database (https://www.ctru-prospect.shef.ac.uk/)
#' and can be exported as CSV files.  They can also be exported as SAS files, albeit stil as
#' individual files for each database table (each of which respresents individual Case Report
#' Forms).  Problems have been encountered when trying to read the supplied SAS Format file
#' which includes the levels and labels for factor variables.
#'
#' This programme allows the reading of the SAS data file but uses the 'Lookups.csv' exported
#' from the Database Specification Spreadsheet to convert and label all factor variables.
#'
#' Prior to using this function the _first_ file that should be converted is 'Lookups.csv' as
#' this is the dictionary file that can be used to automatically encode all factor variables
#' across all subsequent files.
#'
#'
#' @param df Data frame to label.
#' @param header Header option (default \code{TRUE} shouldn't need changing).
#' @param sep Seperator used (default \code{,} (comma) shouldn't need changing).
#' @param dates A list of character strings to use as regular expressions to identify date.
#'              variables that need converting from character to string.
#' @param dictionary Dictionary object.
#'
#' @return Data frame containing the specified file with dates converted to POSIX and factors
#'         converted to with labels applied to them.
#'
#'
#' @examples
#'
#' ## Read in the 'Lookups.csv' which is the data dictionary for all factor variables
#' ## using read_prospect()
#' data.dictionary <- read_prospect(file          = 'Lookups.csv',
#'                                  header        = TRUE,
#'                                  sep           = ',',
#'                                  convert.dates = FALSE,
#'                                  dictionary    = NULL)
#'
#' ## Read the SAS data file e.g. using haven
#' my.df <- read_sas(file = 'my.df.sas7bdat')
#'
#' ## Now use the data dictionary to encode factor variables and convert dates
#' screening <- label_sas_export(df            = my.df,
#'                               convert.dates = TRUE,
#'                               dictionary    = data.dictionary)
#'
#'
#' @export
label_sas_export <- function(df              = data,
                             convert.dates   = TRUE,
                             dictionary      = data.dictionary,
                             ...){
    ## Lowercase and convert variable names
    names(new) <- gsub("_", ".", names(new)) %>%
                  tolower()
    ## Convert specified dates
    if(convert.dates == TRUE){
        ## Convert 'dt' variables
        for(x in grep('dt', colnames(new))){
            new[[x]] <- ymd(new[[x]])
        }
        ## Convert 'date' variables
        for(x in grep('date', colnames(new))){
            new[[x]] <- ymd(new[[x]])
        }
    }
    ## Convert factors
    if(!is.null(dictionary)){
        ## Loop over all variables
        for(x in colnames(new)){
            ## ...but only if the variable is in the dictionary
            if(subset(dictionary, field == x) %>% nrow() > 0){
                new[[x]] <- factor(new[[x]],
                                   levels = c(subset(dictionary,
                                                     field == x))$code,
                                   labels = c(subset(dictionary,
                                                     field == x))$label)
            }
        }
    }
    return(new)
}
