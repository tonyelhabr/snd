
generate_pred_grid <- function(is_pre_plant) {
  feature_names <- get_all_features(
    is_pre_plant = is_pre_plant,
    named = TRUE
  )

  binary_feature_names <- feature_names |>
    purrr::keep(~.x == 'binary') |>
    names()

  binary_l <- vector(mode = 'list', length(binary_feature_names))
  names(binary_l) <- binary_feature_names
  for(el in names(binary_l)) {
    binary_l[[el]] <- c(0L, 1L)
  }

  max_second <- get_max_second(is_pre_plant)
  max_players <- 4L

  binary_values <- 0L:1L
  is_initial_bomb_carrier_killed <- if (isTRUE(is_pre_plant)) {
    binary_values
  } else {
    NA_integer_
  }
  tidyr::crossing(
    'side' = c('o', 'd'),
    'is_pre_plant' = is_pre_plant,
    'model_seconds_elapsed' = seq(0L, (max_second - 1L), by = 0.5),
    'n_team_remaining' = 1L:max_players,
    'n_opponent_remaining' = 1L:max_players,
    'is_kill_on_attempted_clinch' = binary_values,
    'has_started_clinch' = binary_values,
    'is_initial_bomb_carrier_killed' = is_initial_bomb_carrier_killed
  ) |>
    dplyr::mutate(
      'is_offense' = .data[['side']] == 'o',
      'opponent_diff' = .data[['n_team_remaining']] - .data[['n_opponent_remaining']],
      'model_seconds_remaining' = ifelse(
        .data[['is_pre_plant']],
        max_pre_plant_second - .data[['model_seconds_elapsed']],
        max_post_plant_second - .data[['model_seconds_elapsed']]
      ),
      'hardcoded_wp' = NA_real_,
      'is_wp_hardcoded' = FALSE
    )
}

generate_wp_state_grid <- function(model, is_pre_plant) {

  pred_grid <- generate_pred_grid(is_pre_plant = is_pre_plant)

  augment(
    model,
    pred_grid
  )
}

generate_wp_grid <- function(model) {
  c(TRUE, FALSE) |>
    purrr::map_dfr(
      ~generate_wp_state_grid(
        model,
        is_pre_plant = .x
      )
    )
}

