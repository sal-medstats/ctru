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
#' @param shorten Logical indicator of whether to convert supplied responses to shorter \code{None} / \code{Slight} / \code{Moderate} / \code{Severe} / \code{Extreme}.
#'
#' @return A list containing the model fit from the ITT analyses (\code{$itt}), the model
#'         fit from PP analyses (\code{$pp}) and optionally LaTeX (\code{$latex}), HTML
#'         (\code{$html}) or ASCII (\code{$ascii}) Stargazer tables.
#'
#' @examples
#'
#' @references
#'
#' EQ-5D-5L scoring is based on the method published at...
#'
#' https://www.ohe.org/publications/valuing-health-related-quality-life-eq-5d-5l-value-set-england
#'
#' @export
eq5d_score <- function(df                = test,
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
                       shorten           = FALSE,
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
            self.response <- c("I have no problem in walking about",
                               "I have slight problems in walking about",
                               "I have moderate problems in walking about",
                               "I have severe problems in walking about",
                               "I am unable to walk")
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
        df <- df %>%
              mutate(eq5d <- 1.003,
                     ## Deduct pain.response
                     eq5d <- case_when(.$pain == pain.response[1] ~ .$eq5d,
                                       .$pain == pain.response[2] ~ .$eq5d - 0.060,
                                       .$pain == pain.response[3] ~ .$eq5d - 0.075,
                                       .$pain == pain.response[4] ~ .$eq5d - 0.276,
                                       .$pain == pain.response[5] ~ .$eq5d - 0.341),
                     ## Deduct self
                     eq5d <- case_when(.$self == self.response[1] ~ .$eq5d,
                                       .$self == self.response[2] ~ .$eq5d - 0.057,
                                       .$self == self.response[3] ~ .$eq5d - 0.076,
                                       .$self == self.response[4] ~ .$eq5d - 0.181,
                                       .$self == self.response[5] ~ .$eq5d - 0.217),
                     ## Deduct activity
                     eq5d <- case_when(.$activity == activity.response[1] ~ .$eq5d,
                                       .$activity == activity.response[2] ~ .$eq5d - 0.051,
                                       .$activity == activity.response[3] ~ .$eq5d - 0.067,
                                       .$activity == activity.response[4] ~ .$eq5d - 0.174,
                                       .$activity == activity.response[5] ~ .$eq5d - 0.190),
                     ## Deduct mobility
                     eq5d <- case_when(.$mobility == mobility.response[1] ~ .$eq5d,
                                       .$mobility == mobility.response[2] ~ .$eq5d - 0.051,
                                       .$mobility == mobility.response[3] ~ .$eq5d - 0.063,
                                       .$mobility == mobility.response[4] ~ .$eq5d - 0.212,
                                       .$mobility == mobility.response[5] ~ .$eq5d - 0.275),
                     ## Deduct anxiety
                     eq5d <- case_when(.$anxiety == anxiety.response[1] ~ .$eq5d,
                                       .$anxiety == anxiety.response[2] ~ .$eq5d - 0.079,
                                       .$anxiety == anxiety.response[3] ~ .$eq5d - 0.104,
                                       .$anxiety == anxiety.response[4] ~ .$eq5d - 0.296,
                                       .$anxiety == anxiety.response[5] ~ .$eq5d - 0.301),
                     ## Set to 1 if all is fine
                     eq5d <- ifelse(pain     == pain.response[1] &
                                    self     == self.response[1] &
                                    activity == activity.repsonse[1] &
                                    mobility == mobility.response[1] &
                                    anxiety  == anxiety.response[1],
                                    yes = 1,
                                    no  = eq5d),
                     ## Double check if anything is missing
                     eq5d <- ifelse(is.na(pain) |
                                    is.na(self) |
                                    is.na(activity) |
                                    is.na(mobility) |
                                    is.na(anxiety),
                                    yes = NA,
                                    no  = eq5d))
        if(shorten == TRUE){
            eq5d <- eq5d %>%
                mutate(pain = recode_factor(pain,
                                            pain.response[1] = 'None',
                                            pain.response[2] = 'Slight',
                                            pain.response[3] = 'Moderate',
                                            pain.response[4] = 'Severe',
                                            pain.response[5] = 'Extreme'),
                       self = recode_factor(self,
                                            self.response[1] = 'None',
                                            self.response[2] = 'Slight',
                                            self.response[3] = 'Moderate',
                                            self.response[4] = 'Severe',
                                            self.response[5] = 'Extreme'),
                       activity = recode_factor(activity,
                                                activity.response[1] = 'None',
                                                activity.response[2] = 'Slight',
                                                activity.response[3] = 'Moderate',
                                                activity.response[4] = 'Severe',
                                                activity.response[5] = 'Extreme'),
                       mobility = recode_factor(mobility,
                                                mobility.response[1] = 'None',
                                                mobility.response[2] = 'Slight',
                                                mobility.response[3] = 'Moderate',
                                                mobility.response[4] = 'Severe',
                                                mobility.response[5] = 'Extreme'),
                       anxiety = recode_factor(anxiety,
                                               anxiety.response[1] = 'None',
                                               anxiety.response[2] = 'Slight',
                                               anxiety.response[3] = 'Moderate',
                                               anxiety.response[4] = 'Severe',
                                               anxiety.response[5] = 'Extreme'),
                       )
        }
    }
    if(dimensions == 5 & levels == 3){
        ## ToDo - Find scoring method
    }
    return(df)
}
