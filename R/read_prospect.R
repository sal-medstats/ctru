#' Read Prospect Exports
#'
#' @description Read in the raw CSV files exported from Prospect.
#'
#' @details
#' Data collected on Case Report Forms are stored in the Prospect Database
#' (https://www.ctru-prospect.shef.ac.uk/) and are exported as CSV files, one for
#' each table in the datatbase.  These need reading into R for linking because
#' the relational structure that exists within Prospect is lost on export prior
#' to analysis.  Many of the fields are factor variables and need encoding.  This
#' function facilitates that process.
#'
#' When using this function the _first_ file that should be converted is 'Lookups.csv' as this
#' is the dictionary file that can be used to automatically encode all factor variables across
#' all subsequent files.
#'
#'
#' @param file File to read in.
#' @param header Header option (default \code{TRUE} shouldn't need changing).
#' @param sep Seperator used (default \code{,} (comma) shouldn't need changing).
#' @param convert.dates Convert dates to internal date format.
#' @param convert.underscore Optionally convert underscores (\code{_}) in filenames to periods (\code{.}.)
#' @param dictionary Dictionary object.
#'
#' @return Data frame containing the specified file with dates converted to POSIX and factors
#'         converted to with labels applied to them.
#'
#'
#' @examples
#'
#' ## Read in the 'Lookups.csv' which is the data dictionary for all factor variables
#' data.dictionary <- read_prospect(file          = 'Lookups.csv',
#'                                  header        = TRUE,
#'                                  sep           = ',',
#'                                  convert.dates = FALSE,
#'                                  dictionary    = NULL)
#'
#' ## Now use the data dictionary to encode factor variables and convert dates
#' screening <- read_prospect(file          = 'Screening.csv',
#'                            header        = TRUE,
#'                            sep           = ',',
#'                            convert.dates = TRUE,
#'                            dictionary    = data.dictionary)
#'
#' ## You can use lapply() to read in all files in a given directory
#' lapply(x        = list.files(pattern = ".csv"),
#'        function = read_prospect(file          = x,
#'                                 header        = TRUE,
#'                                 sep           = ',',
#'                                 convert.dates = TRUE,
#'                                 dictionary    = data.dictionary)
#'
#' @export
read_prospect <- function(file               = 'Lookups.csv',
                          header             = TRUE,
                          sep                = ',',
                          convert.dates      = TRUE,
                          convert.underscore = FALSE,
                          dictionary         = data.dictionary,
                          ...){
    # Read in the file
    new <- read.csv(file     = file,
                    header   = header,
                    sep      = sep)
    ## Lowercase variable names
    names(new) <- names(new) %>% tolower()
    ## Optionally replace periods to underscore
    if(convert.underscore == TRUE){
        names(new) <- gsub("_", ".", names(new))
    }
    ## Replace any periods in names to underscore
    else{
        names(new) <- gsub("\\.", "_", names(new))
    }
    ## If this is the data dictionary convert '_' in field to '.' so that
    ## we can use it for labelling variables later on
    if(file == 'Lookups.csv' & convert.underscore == TRUE){
        new <- mutate(new,
                      field = gsub("_", ".", field))
    }
    if(!is.null(dictionary)){
        ## Subset the dictionary, required because sometimes a field name
        ## is used multiple times across different forms
        ## TODO - Work out how to handle subforms?
        dictionary <- dplyr::filter(dictionary,
                                    form == gsub('\\.csv', '', file))
    }
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
        ## Convert 'date' variables
        for(x in grep('dob', colnames(new))){
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
