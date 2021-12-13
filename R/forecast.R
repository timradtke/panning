
#' Generate forecast sample paths from an `rmf()` model
#' 
#' @param object List returned by `rmf()`
#' @param h Forecast horizon (number of months into the future to predict)
#' @param n_samples Number of sample paths to be generated
#' 
#' @export
panning <- function(object, h = 12, n_samples = 5000) {
  
  train_end <- max(object$dates)
  year_start <- as.Date(paste0(substr(train_end, 1, 4), "-01-01"))
  test_dates <- seq(
    year_start, 
    seq(train_end, by = "month", length.out = h + 1)[h + 1],
    by = "month"
  )
  
  df_train_narm_feat <- object$df_train_narm_feat
  
  df_test_skeleton <- expand.grid(
    sample_index = 1:n_samples,
    date = test_dates,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      object$df_train[object$df_train$date %in% test_dates, ],
      by = c(date = "date")
    ) %>%
    add_date_features()
  
  df_test_skeleton$fitted_mu <- stats::predict(
    object = object$model, 
    newdata = df_test_skeleton[object$features_during_train],
    type = "response",
    what = "mu",
    data = df_train_narm_feat
  )
  
  df_test_skeleton$fitted_sigma <- stats::predict(
    object = object$model, 
    newdata = df_test_skeleton[object$features_during_train],
    type = "response",
    what = "sigma",
    data = df_train_narm_feat
  )
  
  df_test_skeleton$fitted_nu <- stats::predict(
    object = object$model, 
    newdata = df_test_skeleton[object$features_during_train],
    type = "response",
    what = "nu",
    data = df_train_narm_feat
  )
  
  df_test_skeleton$sample <- gamlss.dist::rTF(
    n = NROW(df_test_skeleton), 
    mu = df_test_skeleton$fitted_mu,
    sigma = df_test_skeleton$fitted_sigma,
    nu = df_test_skeleton$fitted_nu
  )
  
  mm1 <- df_test_skeleton %>%
    dplyr::select(date, sample_index, sample) %>%
    tidyr::pivot_wider(names_from = sample_index, values_from = sample) %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  mm2 <- df_test_skeleton %>%
    dplyr::select(date, sample_index, y_ytd_median_lag_01) %>%
    tidyr::pivot_wider(names_from = sample_index, values_from = y_ytd_median_lag_01) %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  mm3 <- df_test_skeleton %>%
    dplyr::select(date, sample_index, y) %>%
    tidyr::pivot_wider(names_from = sample_index, values_from = y) %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  train_offset <- as.numeric(format(train_end, "%m"))
  mm3_for_cummedian <- mm3
  
  for (h_step in 1:h) {
    # if the previous month was January and there are rows before, set those
    # rows to NA for the cummedian calculation input
    if ((format(test_dates[train_offset + h_step - 1], "%m") == "01") &&
        (train_offset + h_step - 1 > 1)) {
      mm3_for_cummedian[1:(train_offset + h_step - 2), ] <- NA
    }
    
    mm2[train_offset + h_step, ] <- apply(
      X = mm3_for_cummedian[1:(train_offset + h_step - 1), ], 
      MARGIN = 2, 
      FUN = stats::median, 
      na.rm = TRUE
    )

    mm3[train_offset + h_step, ] <- mm2[train_offset + h_step, ] + 
      mm1[train_offset + h_step, ]
    
    mm3_for_cummedian[train_offset + h_step, ] <- mm3[train_offset + h_step, ]
  }
  
  df_forecast <- mm3 %>%
    as.data.frame() %>%
    dplyr::mutate(date = test_dates) %>%
    tidyr::pivot_longer(names_to = "sample_index", values_to = "y", -date) %>%
    dplyr::filter(date > train_end) %>%
    dplyr::mutate(y = y * object$mad + object$median) %>%
    dplyr::select(date, y, sample_index)
  
  return(df_forecast)
}
