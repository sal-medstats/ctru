#' Produce a dataframe with numbers and percentages
#'
#' @description Takes a data frame and tabulates two variables producing
#'              the number and percentage either by row, column or total.
#'
#' @details
#'
#' People like to see the raw numbers but often think about them in
#' percentages, this wrapper function facilitates production of such tables.
#'
#'
#' @param df Data frame.
#' @param rows Variable that is to be rows
#' @param columns Variable that is to be columns
#' @param prop.by Should percentages be by \code{rows}, \code{columns} or \code{total}.
#' @param percent Logical indicator of whether proportions should be converted to percentages.
#' @param digits Number of decimal places to be used in proportion/percentages.
#'
#' @export
table_prop <- function(df              = .data,
                       rows            = NULL,
                       columns         = NULL,
                       prop.by         = 'rows',
                       percent         = TRUE,
                       digits          = 2,
                       ...){
    results <- list()
    ## Tabulate the data
    tbl <- table(df[[substitute(rows)]], df[[substitute(columns)]])
    ## Produce proportions conditional on rows or columns
    if(prop.by == 'rows'){
        prop <- prop.table(tbl, 1)
    }
    else if(prop.by == 'columns'){
        prop <- prop.table(tbl, 2)
    }
    else if(prop.by == 'total'){
        prop <- prop.table(tbl)
    }
    ## Append the two as data frames
    tbl      <- as.data.frame(tbl)
    prop     <- as.data.frame(prop)
    ## Convert to percentages
    if(percent == TRUE){
        prop$Freq <- prop$Freq * 100
    }
    out <- merge(tbl,
                 prop,
                 by = c('Var1', 'Var2')) %>%
        ## Combine and format count and proportion/percent
        mutate(Out = paste0(formatC(Freq.x, format = 'f', digits = 0),
                            ' (',
                            formatC(Freq.y, format = 'f', digits = digits),
                            ')')) %>%
           dplyr::select(-Freq.x, -Freq.y)
    ## Conditionally add in percentage
    if(percent == TRUE){
        out$Out <- gsub(')', '%)', out$Out)
    }
    ## Reshape
    out <- dcast(out, Var1 ~ Var2, value.var = 'Out')
    ## Tidy names
    names(out) <- gsub('Var1', 'Group', names(out))
    return(out)
}
