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
#' @param check.duplicates Optionally check for duplicate rows across all variables.
#' @param purge.special Remove special characters from the \code{form} field of \code{Lookups.csv} so that they match exported file names which do not include them (currently only removes \code{/}).
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
                          check.duplicates   = TRUE,
                          purge.special      = TRUE,
                          ...){
    # Read in the file
    new <- read.csv(file     = file,
                    header   = header,
                    sep      = sep)
    ## Lowercase variable names
    names(new) <- names(new) %>% tolower()
    ## Remove '_o' that is appended to Flag variables when they are expanded
    ## to multiple binary variables, they are NOT used in the Lookups.csv
    names(new) <- gsub('oth_o$', 'other', names(new))
    names(new) <- gsub('_o$', '', names(new))
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
    ## Potential for duplicate variable names when removing '_o'  from 'oth_o' variables
    ## correct this in the lookup
    if(file == 'Lookups.csv'){
        new <- mutate(new,
                      field = gsub('oth_o$', 'other', field),
                      field = gsub('_oth$', '_other', field),
                      field = gsub('_o$', '', field))
    }
    ## Remove special characters from the 'form' field if asked to do so
    if(file == 'Lookups.csv' & purge.special == TRUE){
        new <- mutate(new,
                      form = gsub('/', '', form))
    }
    if(!is.null(dictionary)){
        ## Subset the dictionary, required because sometimes a field name
        ## is used multiple times across different forms
        ## TODO - Work out how to handle subforms?
        current_form    <- strsplit(file, ' - ', fixed = TRUE) %>%
                           unlist() %>%
                           data.frame()
        if(nrow(current_form) == 1){
            dictionary <- dplyr::filter(dictionary,
                                        form    == gsub('\\.csv', '', file))
        }
        else{
            dictionary <- dplyr::filter(dictionary,
                                        form    == current_form[1,1]  &
                                        subform == gsub('\\.csv', '', current_form[2,1]))
        }
    }
    ## Check for duplicates
    if(check.duplicates == TRUE){
        if(anyDuplicated(new) > 0){
            print('Error : There are duplicated rows in this file.')
            exit
        }
    }
    ## Convert specified dates
    if(convert.dates == TRUE){
        ## Convert 'dt' variables
        for(x in grep('_dt', colnames(new))){
            new[[x]] <- lubridate::ymd(new[[x]])
        }
        ## Convert 'date' variables
        for(x in grep('date', colnames(new))){
            new[[x]] <- lubridate::ymd(new[[x]])
        }
        ## Convert 'date' variables
        for(x in grep('dob', colnames(new))){
            new[[x]] <- lubridate::ymd(new[[x]])
        }
    }
    ## Convert 'event_name' to factor if present
    ## if('event_name' %in% names(new)){
    ##     new <- new %>%
    ##            mutate(event_name = factor(event_name))
    ## }
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
    ## If this is EQ5d data then recode variables
    if(file == 'EQ5D.csv'){
        new <- new %>%
               mutate(mobility = recode_factor(mobility,
                                               'I have no problem in walking about'        = 'None',
                                               'I have slight problems walking about'      = 'Slight',
                                               'I have moderate problems in walking about' = 'Moderate',
                                               'I have severe problems in walking about'   = 'Severe',
                                               'I am unable to walk'                       = 'Extreme'),
                      self_care = recode_factor(self_care,
                                                'I have no problem washing or dressing myself'        = 'None',
                                                'I have slight problems washing or dressing myself'   = 'Slight',
                                                'I have moderate problems washing or dressing myself' = 'Moderate',
                                                'I have severe problems washing or dressing myself'   = 'Severe',
                                                'I am unable to wash or dress myself'                 = 'Extreme'),
                      usual_activity = recode_factor(usual_activity,
                                                     'I have no problems doing my usual activities'       = 'None',
                                                     'I have slight problems doing my usual activities'   = 'Slight',
                                                     'I have moderate problems doing my usual activities' = 'Moderate',
                                                     'I have severe problems doing my usual activities'   = 'Severe',
                                                     'I am unable to do my usual activities'            = 'Extreme'),
                      pain_discomfort = recode_factor(pain_discomfort,
                                                      'I have no pain or discomfort'       = 'None',
                                                      'I have slight pain or discomfort'   = 'Slight',
                                                      'I have moderate pain or discomfort' = 'Moderate',
                                                      'I have severe pain or discomfort'   = 'Severe',
                                                      'I have extreme pain or discomfort'  = 'Extreme'),
                      anxiety_depression = recode_factor(anxiety_depression,
                                                         'I am not anxious or depressed'        = 'None',
                                                         'I am slightly anxious or depressed'   = 'Slight',
                                                         'I am moderately anxious or depressed' = 'Moderate',
                                                         'I am severely anxious or depressed'   = 'Severe',
                                                         'I am extremely anxious or depressed'  = 'Extreme'))
    }
    return(new)
}
