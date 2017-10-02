#' Simulation based sample size calculations for panel data
#'
#' @description Sample size calculations for panel/longitudinal study designs.
#'
#' @details
#'
#'
#' @param nper Sequence of sample sizes to simulate.
#' @param nsim Number of simulations to perform for each set of parameters.
#' @param n_follow_up Number of follow-up time points.
#'
#' @export
sim_panel<- function(nper        = seq(100, 1000, by = 100),
                     nsim        = 1000,
                     n_follow_up = 8,
                     ...){
    ## See http://educate-r.org//2017/05/23/simglmcran.html
}
