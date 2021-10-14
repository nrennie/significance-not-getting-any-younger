#' Estimate Trend
#'
#' This function estimates trend in life expectancy over a moving window.
#'
#' @param input_vec A numeric vector of life expectancies across different years.
#' @param year_vec A numeric vector of the years observed. Default 1960:2020,
#' @param window_vec A numeric giving length of window. Default 10.
#' @return A numeric vector.
#' @export


est_trend <- function(input_vec, year_vec=1960:2020, window_val=10){
  #check conditions
  if (length(input_vec) != length(year_vec)){
    stop('input_vec and year_vec must be the same length')
  }

  #identify data range
  test_years <- (year_vec[1]+window_val - 1):tail(year_vec,1)

  #empty vectors to store data
  est_beta <- numeric(length(test_years))
  upper_beta <- numeric(length(test_years))
  lower_beta <- numeric(length(test_years))

  #apply linear models
  for (i in 1:length(test_years)){
    test_year <- test_years[i]
    #select data
    x <- year_vec[(which(year_vec == test_year-window_val+1)):which(year_vec == test_year)]
    y <- input_vec[(which(year_vec == test_year-window_val+1)):which(year_vec == test_year)]

    #fit model
    mod <- lm(y~x)
    est_beta[i] <- mod$coefficients[2]

    #intervals
    k <- confint(mod)
    upper_beta[i] <- k[2,2]
    lower_beta[i] <- k[2,1]
  }

  #output values
  list("Trend estimate"=est_beta,
       "Lower limit"=lower_beta,
       "Upper limit"=upper_beta,
       "Test years"=test_years)
}




