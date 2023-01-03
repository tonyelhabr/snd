
predict.wp_model_state <- function(object, new_data, ...) {
  model <- object$model
  nms <- names(model)
  n_row <- nrow(new_data)
  n_col <- length(nms)
  m <- matrix(rep(0L, times = n_row * n_col), nrow = n_row, ncol = n_col)

  colnames(m) <- nms
  for(el in nms) {
    ## was having some issues with last second predictions (even if they showed 45 exactly)
    ##   without rounding
    m[, el] <- predict(model[[el]], newdata = round(new_data$model_seconds_elapsed))
  }

  new_data$`(Intercept)` <- 1L
  ## Check that they are in the same order
  stopifnot(colnames(m) == colnames(new_data[, nms]))

  log_odds <- rowSums(m * as.matrix(new_data[, nms]))
  stats::plogis(log_odds) ## logit
}

predict.wp_model <- function(object, new_data, ...) {

  validate_colnames(data = new_data, is_predict = TRUE)

  pred <- ifelse(
    new_data[['is_pre_plant']],
    predict(object[['pre']], new_data),
    predict(object[['post']], new_data)
  )

  pred <- ifelse(
    new_data[['side']] == 'o',
    pred,
    1 - pred
  )

  ifelse(
    new_data[['is_wp_hardcoded']],
    new_data[['hardcoded_wp']],
    pred
  )

}

#' @export
augment.wp_model <- function(x, data, ...) {
  data[['wp']] <- stats::predict(x, data, ...)
  data
}

predict.wp_model_naive <- function(object, new_data, ...) {

  validate_naive_colnames(data = object, is_predict = FALSE) ## should have win_round column
  validate_naive_colnames(data = new_data, is_predict = TRUE)

  object |>
    dplyr::inner_join(
      new_data |>
        dplyr::select(
          -tidyselect::vars_select_helpers$any_of("wp")
        ),
      by = setdiff(colnames(object, 'wp'))
    )
}
