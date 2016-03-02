#' Read Prospect Exports
#'
#' @description Read in the raw CSV files exported from Prospect.
#'
#' @details
#' Case Report Forms are stored in the Prospect Database (https://www.ctru-prospect.shef.ac.uk/)
#' and are exported as CSV files.  These need reading into R for linking because the relational
#' structure that exists within Prospect is lost on export prior to analysis.
#'
#' When using this function the _first_ file that should be converted is 'Lookups.csv' as this
#' is the dictionary file that can be used to automatically encode all factor variables across
#' all subsequent files.
#'
#' 
#' @param file File to read in
#' @param header Header option (default \code{TRUE} shouldn't need changing)
#' @param sep Seperator used (default \code{,} (comma) shouldn't need changing)
#' @param dates A list of character strings to use as regular expressions to identify date
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
read_prospect <- function(file            = 'Lookups.csv',
                          header          = TRUE,
                          sep             = ',',
                          convert.dates   = TRUE,
                          dictionary      = data.dictionary){
    # Read in the file
    new <- read.csv(file     = file,
                    header   = header,
                    sep      = sep)
    ## Lowercase and convert variable names
    names(new) <- gsub("_", ".", names(new)) %>%
                  tolower()
    ## If this is the data dictionary convert '_' in field to '.' so that
    ## we can use it for labelling variables later on
    if(file == 'Lookups.csv'){
        new <- within(new,{
                      field <- gsub("_", ".", field)
        })
    }
    ## Convert specified dates
    if(convert.dates == TRUE){
        ## Convert 'dt' variables
        for(x in grep('dt', colnames(new))){
            print(x)
            new[,x] <- ymd(new[,x])
        }
        ## Convert 'date' variables
        for(x in grep('date', colnames(new))){
            print(x)
            new[,x] <- ymd(new[,x])
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
