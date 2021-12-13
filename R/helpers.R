#' Naive median equivalent to `dplyr::cummean()`
#' 
#' @param x Numeric vector
#' @param na.rm Remove NAs (passed to `median()`)
#' 
cummedian <- function(x, na.rm = FALSE) {
  x_len <- length(x)
  if (x_len == 0) return(x)
  
  y <- rep(NA, x_len)
  
  for (i in seq_along(x)) {
    y[i] <- stats::median(x[1:i], na.rm = na.rm)
  }
  
  return(y)
}

#' Add date-based feature columns to a data frame
#' 
#' @param df A data frame with at least a column called `date`
#' 
add_date_features <- function(df) {
  checkmate::assert_names(
    x = names(df),
    must.include = "date"
  )
  checkmate::assert_date(x = df$date)
  
  df <- dplyr::mutate(
    df,
    month = format(date, "%m"),
    new_year = as.numeric(month == "01"),
    year = as.numeric(format(date, "%Y"))
  )
  
  return(df)
}

#' Return differenced series prepared as training data frame
#' 
#' @param y A numeric vector
#' @param dates A date vector of equal length as `y`
#' 
#' @return Data frame
#' 
preprocess <- function(y, dates) {
  df <- data.frame(
    y = y,
    date = dates
    ) %>%
    add_date_features() %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      y_ytd_median = cummedian(y, na.rm = TRUE),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      y_ytd_median_lag_01 = dplyr::lag(x = y_ytd_median, n = 1),
      y_ytd_median_lag_01_diff = y - y_ytd_median_lag_01
    )
  
  return(df)
}
