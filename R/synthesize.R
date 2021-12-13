
#' Create randomly sampled time series
#' 
#' @param dates A vector of months along which to sample
#' 
#' @return A numeric vector
#' 
#' @examples 
#' dates <- seq(as.Date("2016-01-01"), as.Date("2022-12-01"), by = "month")
#' 
#' synthesize(dates = dates)
#' 
#' @export
synthesize <- function(dates) {
  
  checkmate::assert_date(x = dates)
  if (any(as.numeric(format(dates, "%d")) != 1)) {
    stop("The months provided must be formatted as: YYYY-MM-01")
  }
  
  n_obs <- length(dates)
  y <- rep(NA, n_obs)
  
  mu_monthly <- 100
  year_end_shift <- 0
  sigma_year_end_shift <- 2
  tmp_mom_shift <- 0
  mom_shift <- rnorm(n = 1)
  seasonality_scale <- rnorm(n = 1, mean = 0, sd = 8)
  
  for (i in seq_along(dates)) {
    if (i != 1 && as.numeric(format(dates[i], "%m")) == 1) {
      year_end_shift <- stats::rcauchy(n = 1, scale = sigma_year_end_shift)
    }
    
    tmp_mom_shift <- tmp_mom_shift + mom_shift
    
    y[i] <- mu_monthly + tmp_mom_shift + year_end_shift + 
      stats::rcauchy(n = 1, scale = 2)
    
    y[i] <- y[i] + 
      stats::rnorm(
        n = 1, 
        cospi(as.numeric(format(dates[i], "%m")) / 6) * seasonality_scale, 
        sd = 5
      )
  }
  
  return(y)
}
