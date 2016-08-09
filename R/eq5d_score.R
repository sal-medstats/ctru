#' Derive EQ5D Scores
#'
#' @description Derive EQ5D scores from the individual responses
#'
#' @details
#'
#'
#'
#' @param df Data frame containing EQ5D data.
#' @param dimensions Number of dimensions to the EQ5D data (invariably 5 but included in case older versions are to be used)
#' @param levels Number of levels in responses, older versions of EQ5D used 3, newer 5.
#' @param mobility Variable name in data frame that holds the \code{mobility} response.
#' @param self Variable name in data frame that holds the \code{self} response.
#' @param activity Variable name in data frame that holds the \code{activity} response.
#' @param pain Variable name in data frame that holds the \code{pain} response.
#' @param anxiety Variable name in data frame that holds the \code{anxiety} response.
#' @param mobility.repsonse List of custom responses for \code{mobility} item.
#' @param self.repsonse List of custom responses for \code{self} item.
#' @param activity.repsonse List of custom responses for \code{activity} item.
#' @param pain.repsonse List of custom responses for \code{pain} item.
#' @param anxiety.repsonse List of custom responses for \code{anxiety} item.
#'
#' @return A list containing the model fit from the ITT analyses (\code{$itt}), the model
#'         fit from PP analyses (\code{$pp}) and optionally LaTeX (\code{$latex}), HTML
#'         (\code{$html}) or ASCII (\code{$ascii}) Stargazer tables.
#'
#' @examples
#'
#' @references
#'
#' As of writing the scoring for the EQ5D-5L had not been published but is detailed on slides 40-41 of...
#'
#' http://www.slideshare.net/OHENews/ohe-seminar-5ll-value-set-oct2014-revised-jun15
#'
#' @export
eq5d_score<- function(df                = test,
                      dimensions        = 5,
                      levels            = 5,
                      mobility          = 'mobility',
                      self              = 'self.care',
                      activity          = 'usual.activity',
                      pain              = 'pain.discomfort',
                      anxiety           = 'anxiety.depression',
                      mobility.repsonse = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
                      self.repsonse     = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
                      activity.repsonse = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
                      pain.repsonse     = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
                      anxiety.repsonse  = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
                      ...){
    if(dimensions == 5 & levels == 5){
        ## Check if custom responses specified and if so that their lengths are 5
        if(is.na(mobility.response)){
            mobility.response <- c("I have no problems washing or dressing myself",
                                   "I have slight problems washing or dressing myself",
                                   "I have moderate problems washing or dressing myself",
                                   "I have severe problems washing or dressing myself",
                                   "I am unable to wash or dress myself")
        }
        else if(length(mobility.response) != 5){
            print('Error : mobility.response is not of length 5')
        }
        if(is.na(self.response)){
            self.response <- c("I have no problems in walking about",
                               "I have slight problems in walking about",
                               "I have moderate problems in walking about",
                               "I have severe problems in walking about",
                               "I am unable to walk about")
        }
        else if(length(self.response)     != 5){
            print('Error : self.response is not of length 5')
        }
        if(is.na(activity.response)){
            activity.response <- c("I have no problems washing or dressing myself",
                                   "I have slight problems washing or dressing myself",
                                   "I have moderate problems washing or dressing myself",
                                   "I have severe problems washing or dressing myself",
                                   "I am unable to wash or dress myself")
        }
        else if(length(activity.response) != 5){
            print('Error : activity.response is not of length 5')
        }
        if(is.na(pain.response)){
            pain.response <- c("I have no pain or discomfort",
                               "I have slight pain or discomfort",
                               "I have moderate pain or discomfort",
                               "I have severe pain or discomfort",
                               "I have extreme pain or discomfort")
        }
        else if(length(pain.response)     != 5){
            print('Error : pain.response is not of length 5')
        }
        if(is.na(anxiety.response)){
            anxiety.response <- c("I am not anxious or depressed",
                                  "I am slightly anxious or depressed",
                                  "I am moderately anxious or depressed",
                                  "I am severely anxious or depressed",
                                  "I am extremely anxious or depressed" )
        }
        else if(length(anxiety.response)  != 5){
            print('Error : anxiety.response is not of length 5')
        }
        ## Calculate the score
        df <- within(df, {
                     eq5d <- 1.003
                     eq5d[pain == pain.response[2]]         <- eq5d[pain == pain.response[2]] - 0.059
                     eq5d[pain == pain.repsonse[3]]         <- eq5d[pain == pain.response[3]] - 0.099
                     eq5d[pain == pain.response[4]]         <- eq5d[pain == pain.response[4]] - 0.245
                     eq5d[pain == pain.response[5]]         <- eq5d[pain == pain.response[5]] - 0.298
                     eq5d[self == self.response[2]]         <- eq5d[self == self.response[2]] - 0.058
                     eq5d[self == self.response[3]]         <- eq5d[self == self.response[3]] - 0.083
                     eq5d[self == self.response[4]]         <- eq5d[self == self.response[4]] - 0.176
                     eq5d[self == self.response[5]]         <- eq5d[self == self.response[5]] - 0.208
                     eq5d[activity == activity.repsonse[2]] <- eq5d[activity == activity.repsonse[2]] - 0.048
                     eq5d[activity == activity.repsonse[3]] <- eq5d[activity == activity.repsonse[3]] - 0.067
                     eq5d[activity == activity.repsonse[4]] <- eq5d[activity == activity.repsonse[4]] - 0.165
                     eq5d[activity == activity.repsonse[5]] <- eq5d[activity == activity.repsonse[5]] - 0.165
                     eq5d[mobility == mobility.response[2]] <- eq5d[activity == mobility.repsonse[2]] - 0.057
                     eq5d[mobility == mobility.response[3]] <- eq5d[activity == mobility.repsonse[3]] - 0.075
                     eq5d[mobility == mobility.response[4]] <- eq5d[activity == mobility.repsonse[4]] - 0.208
                     eq5d[mobility == mobility.response[5]] <- eq5d[activity == mobility.repsonse[5]] - 0.255
                     eq5d[anxiety == anxiety.response[2]]   <- eq5d[anxiety == anxiety.response[2]] - 0.073
                     eq5d[anxiety == anxiety.response[3]]   <- eq5d[anxiety == anxiety.response[3]] - 0.099
                     eq5d[anxiety == anxiety.response[4]]   <- eq5d[anxiety == anxiety.response[4]] - 0.282
                     eq5d[anxiety == anxiety.response[5]]   <- eq5d[anxiety == anxiety.response[5]] - 0.282
                     eq5d[pain     == pain.response[1] &
                          self     == self.response[1] &
                          activity == activity.repsonse[1] &
                          mobility == mobility.response[1] &
                          anxiety  == anxiety.response[1]] <- 1
                     eq5d[is.na(pain) |
                          is.na(self) |
                          is.na(activity) |
                          is.na(mobility) |
                          is.na(anxiety)] <- NA

        })
    }
    if(dimensions == 5 & levels == 3){
        ## ToDo - Find scoring method
    }
    return(df)
}
