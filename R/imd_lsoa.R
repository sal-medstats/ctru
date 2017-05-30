#' Combine postcodes with Indeces of Multiple Deprivation Scores
#'
#' @description Takes a data frame containing postcodes and combines with
#'              the Lower Super Output Area (LSOA) Index Of Multiple Deprivation
#'              from the UK Government Office for National Statistics
#'
#' @details
#'
#' This function takes a data frame with a list of postcodes and combines them
#' with the Index of Multiple Deprivation (IMD) for the specified year.
#'
#'
#' @param df Data frame.
#' @param postcode The variable in the data frame the contains the postcode.
#' @param imd_year The year of IMDs to combine with the postcodes.
#' @param lsoa_postcode Data frame that contains the Postcode to LSOA mappings
#' @param postcode_length Whether to use the seven or eight digit version of postocde.
#'
#' @export
## ToDo - Get Wales codes
imd_lsoa <- function(df              = .,
                     postcode        = postcode,
                     imd_year        = 2015,
                     lsoa_postcode   = imd_lsoa_postcode,
                     postcode_length = 8,
                     ...){
    ## List to return
    results <- list()
    ## Obtain the LSOA IMD for the specified year
    if(imd_year == 2015){
        if(postcode_length == 8){
            results <- merge(df,
                             imd_lsoa_postcode,
                             by.x  = postcode,
                             by.y  = pcd8,
                             all.x = TRUE)
        }
        else if(postcode_length == 7){
            results <- merge(df,
                             imd_lsoa_postcode,
                             by.x  = postcode,
                             by.y  = pcd7,
                             all.x = TRUE)

        }
    }
    else if(imd_year == 2010){
        ## ToDo : Write this section
        imd2010 <- list()
    }
    else{
        print('Error : Please specify imd_year as either 2015 or 2010')
        exit()
    }
    return(results)
}
