
#' Fits a Robust Median Filter model to a monthly timeseries
#' 
#' @param y A numeric vector representing a univariate timeseries to be forecast
#' @param dates A date vector
#' @param use_seasonality logical; if `TRUE`, monthly dummies are added to the
#'     forecast model
#' 
#' @return A list of a fitted model and other objects
#' 
#' @export
rmf <- function(y, dates, use_seasonality = FALSE) {

  checkmate::assert_numeric(y, any.missing = FALSE)
  checkmate::assert_date(dates, any.missing = FALSE)
  checkmate::assert_true(length(y) == length(dates))
  if (any(as.numeric(format(dates, "%d")) != 1)) {
    stop("The months provided must be formatted as: YYYY-MM-01")
  }
  
  median <- stats::median(y, na.rm = TRUE)
  mad <- stats::mad(y, na.rm = TRUE)
  y_scaled <- (y - median) / mad
  
  df_train <- preprocess(y = y_scaled, dates = dates)
  df_train_narm <- df_train[!is.na(df_train$y_ytd_median_lag_01_diff), ]
  features_during_train <- c("y_ytd_median_lag_01_diff", "new_year", "month")
  df_train_narm_feat <- df_train_narm[features_during_train]
  
  if (use_seasonality) {
    model <- gamlss::gamlss(
      formula = y_ytd_median_lag_01_diff ~ 1 + month,
      sigma.formula = y_ytd_median_lag_01_diff ~ 1 + new_year,
      family = gamlss.dist::TF(),
      data = df_train_narm_feat
    )
  } else {
    model <- gamlss::gamlss(
      formula = y_ytd_median_lag_01_diff ~ 1,
      sigma.formula = y_ytd_median_lag_01_diff ~ 1 + new_year,
      family = gamlss.dist::TF(),
      data = df_train_narm_feat
    )
  }
  
  fitted_location <- stats::predict(
    object = model,
    type = "response",
    what = "mu"
  )
  
  fitted_scale <- stats::predict(
    object = model,
    type = "response",
    what = "sigma"
  )
  
  return(
    list(
      model = model,
      df_train = df_train,
      dates = dates,
      y = y,
      y_scaled = y_scaled,
      median = median,
      mad = mad,
      fitted_location = fitted_location,
      fitted_scale = fitted_scale,
      features_during_train = features_during_train,
      df_train_narm_feat = df_train_narm_feat,
      use_seasonality = use_seasonality
    )
  )
}
