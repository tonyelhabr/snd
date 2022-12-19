
target_name <- 'win_round'
default_side <- 'o'
max_pre_plant_second <- 90L
max_post_plant_second <- 45L

get_max_second <- function(is_pre_plant) {
  ifelse(isTRUE(is_pre_plant), max_pre_plant_second, max_post_plant_second)
}

get_all_features <- function(is_pre_plant, named = FALSE) {

  features <- c(
    'opponent_diff' = 'integer',
    'is_kill_on_attempted_clinch' = 'binary',
    'has_started_clinch' = 'binary',
    'is_initial_bomb_carrier_killed' = 'binary'
  )

  if (isFALSE(is_pre_plant)) {
    idx_to_drop <- which(names(features) == 'is_initial_bomb_carrier_killed')
    idx_to_keep <- setdiff(seq.int(1, length(features)), idx_to_drop)
    features <- features[idx_to_keep]
  }

  if (isTRUE(named)) {
    return(features)
  }

  names(features)
}

validate_colnames <- function(data, is_predict = FALSE) {

  base_cols <- c(
    'is_pre_plant',
    'side'
  )

  base_cols <- if (isFALSE(is_predict)) {
    c(
      base_cols,
      'n_team_remaining',
      'n_opponent_remaining',
      'model_seconds_remaining'
    )
  } else {
    c(
      base_cols,
      'is_wp_hardcoded',
      'hardcoded_wp'
    )
  }

  nms_diff <- setdiff(
    base_cols,
    colnames(data)
  )

  if (length(nms_diff) > 0L) {
    stop(
      sprintf('Missing essential non-feature columns:\n%s', paste0(nms_diff, collapse = ', '))
    )
  }

  all_feature_names <- c(TRUE, FALSE) |>
    purrr::map(
      ~get_all_features(
        is_pre_plant = .x,
        named = FALSE
      )
    ) |>
    purrr::flatten_chr() |>
    unique()

  nms_diff <- setdiff(
    all_feature_names,
    colnames(data)
  )

  if (length(nms_diff) > 0L) {
    stop(
      sprintf('Missing feature names:\n%s', paste0(nms_diff, collapse = ', '))
    )
  }

  invisible(TRUE)
}

split_model_data <- function(data) {

  validate_colnames(data = data)

  model_data <- dplyr::filter(data, .data[['side']] == default_side)

  list(
    'pre' = dplyr::filter(data, .data[['is_pre_plant']]),
    'post' = dplyr::filter(data, !.data[['is_pre_plant']])
  )
}

fit_wp_model_states <- function(data, ...) {

  model_data <- split_model_data(data = data)

  ## things get messy if using imap here, so just be explicit instead
  models <- list(
    'pre' = fit_wp_model_state(model_data[['pre']], is_pre_plant = TRUE, ...),
    'post' = fit_wp_model_state(model_data[['post']], is_pre_plant = FALSE, ...)
  )

  class(models) <- c('wp_model', class(models))
  models
}


add_hardcoded_wp_cols <- function(data) {
  data |>
    dplyr::mutate(
      'is_1v0_post_plant' = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 1) & (.data[['n_opponent_remaining']] == 0),
      'is_0v1_post_plant' = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & (.data[['n_opponent_remaining']] == 1),
      'is_Nv0_post_plant' = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] > 1) & (.data[['n_opponent_remaining']] == 0),
      'is_0vN_post_plant' = !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & (.data[['n_opponent_remaining']] > 1),

      'hardcoded_wp' = dplyr::case_when(

        .data[['side']] == 'o' & .data[['activity']] == 'Defuse' ~ 0,
        .data[['side']] == 'd' & .data[['activity']] == 'Defuse' ~ 1,

        .data[['side']] == 'o' & !.data[['is_pre_plant']] & (.data[['n_team_remaining']] == 0) & .data[['model_seconds_remaining']] >= 7.5 & .data[['activity']] == 'Start Defuse' ~ 0,
        .data[['side']] == 'd' & !.data[['is_pre_plant']] & (.data[['n_opponent_remaining']] == 0) & .data[['model_seconds_remaining']] >= 7.5 & .data[['activity']] == 'Start Defuse' ~ 1,

        'is_1v0_post_plant' & .data[['model_seconds_remaining']] >= 15 ~ 1,
        'is_1v0_post_plant' & .data[['model_seconds_remaining']] >= 12.5 ~ 0.9,
        'is_1v0_post_plant' & .data[['model_seconds_remaining']] >= 10 ~ 0.65,
        'is_1v0_post_plant' & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        'is_1v0_post_plant' & .data[['model_seconds_remaining']] < 7.5  ~ 0,

        'is_0v1_post_plant' & .data[['model_seconds_remaining']] >= 15 ~ 0,
        'is_0v1_post_plant' & .data[['model_seconds_remaining']] >= 12.5  ~ 0.1,
        'is_0v1_post_plant' & .data[['model_seconds_remaining']] >= 10 ~ 0.35,
        'is_0v1_post_plant' & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        'is_0v1_post_plant' & .data[['model_seconds_remaining']] < 7.5  ~ 1,

        'is_Nv0_post_plant' & .data[['model_seconds_remaining']] >= 12.5 ~ 1,
        'is_Nv0_post_plant' & .data[['model_seconds_remaining']] >= 10 ~ 0.75,
        'is_Nv0_post_plant' & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        'is_Nv0_post_plant' & .data[['model_seconds_remaining']] < 7.5  ~ 0.25,

        'is_0vN_post_plant' & .data[['model_seconds_remaining']] >= 12.5 ~ 0,
        'is_0vN_post_plant' & .data[['model_seconds_remaining']] >= 10 ~ 0.25,
        'is_0vN_post_plant' & .data[['model_seconds_remaining']] >= 7.5 ~ 0.5,
        'is_0vN_post_plant' & .data[['model_seconds_remaining']] < 7.5  ~ 0.75,

        TRUE ~ NA_real_
      ),
      'is_wp_hardcoded' = !is.na(.data[['hardcoded_wp']])
    ) |>
    dplyr::select(
      -tidyselect::vars_select_helpers$matches('is_[10N]v[10N]_post_plant')
    )
}
