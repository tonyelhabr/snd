
safely_glm <- purrr::safely(glm)
fit_window_wp_coefs <- function(data, earliest_seconds_elapsed, latest_seconds_elapsed, is_pre_plant) {
  filt <- dplyr::filter(
    data,
    .data[['model_seconds_elapsed']] >= earliest_seconds_elapsed,
    .data[['model_seconds_elapsed']] <= latest_seconds_elapsed
  )

  msg <- sprintf(
    'for `model_seconds_elapsed >= %s & model_seconds_elapsed <= %s.',
    earliest_seconds_elapsed,
    latest_seconds_elapsed
  )
  if (nrow(filt) <= 2) {
    message(sprintf('Less than 3 records %s', msg))
    return(NULL)
  }

  if (length(unique(filt$win_round)) == 1) {
    warning(sprintf('Only one unique value in target variable %s', msg))
  }

  feature_names <-   get_all_features(
    is_pre_plant = is_pre_plant,
    named = FALSE
  )

  default_result <- rlang::set_names(
    rep(NA, length(feature_names)),
    feature_names
  )

  form <- sprintf('%s ~ .', target_name)
  fit <- safely_glm(
    stats::as.formula(form),
    data = dplyr::select(
      filt,
      .data[['win_round']],
      tidyselect::vars_select_helpers$all_of(feature_names)
    ),
    family = stats::binomial(link = 'logit')
  )

  if (!is.null(fit$error)) {
    message(sprintf('Error %s.\n%s', msg, fit$error))
    return(default_result)
  }
  stats::coefficients(fit$result)
}

estimate_window_coefs <- function(data, is_pre_plant) {

  split_name <- ifelse(isTRUE(is_pre_plant), 'pre', 'post')
  seconds_grid <- seconds_grid[[split_name]]
  seconds_grid |>
    dplyr::transmute(
      'model_seconds_elapsed' = .data[['min_second']],
      'coefs' = purrr::map2(
        .data[['min_second']],
        .data[['max_second']],
        ~fit_window_wp_coefs(
          data = data,
          earliest_seconds_elapsed = ..1,
          latest_seconds_elapsed = ..2,
          is_pre_plant = is_pre_plant
        )
      )
    ) |>
    tidyr::unnest_wider(.data[['coefs']])
}

# halflife <- \(x) 1/(x^0.5)
convert_seconds_to_weights <- function(x, is_pre_plant) {
  max_second <- ifelse(isTRUE(is_pre_plant), max_pre_plant_second, max_post_plant_second)
  dplyr::na_if(sqrt((max_second - x) / max_second), NaN)
}

fit_coef_model <- function(data, y, is_pre_plant) {
  # wts <- convert_seconds_to_weights(data[['model_seconds_elapsed']], is_pre_plant)
  stats::loess(
    data[[y]] ~ data[['model_seconds_elapsed']],
    # weights = wts,
    span = 0.5
  )
}

fit_wp_model_state <- function(data, is_pre_plant) {

  coefs <- estimate_window_coefs(
    data,
    is_pre_plant = is_pre_plant
  )

  feature_names <-   get_all_features(
    is_pre_plant = is_pre_plant,
    named = FALSE
  )
  feature_names <- c(feature_names, '(Intercept)')

  coefs <- tidyr::fill(
    coefs,
    tidyselect::vars_select_helpers$all_of(feature_names),
    .direction = 'downup'
  )

  models <- feature_names |>
    rlang::set_names() |>
    purrr::map(
      ~fit_coef_model(coefs, y = .x, is_pre_plant = is_pre_plant)
    )

  res <- list(
    model = models,
    coefs = coefs
  )
  class(res) <- c('wp_model_state', class(res))
  res
}
