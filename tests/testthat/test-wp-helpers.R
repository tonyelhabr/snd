test_that('get_all_features works', {
  pre_plant_features <- c(
    'opponent_diff' = 'integer',
    'is_kill_on_attempted_clinch' = 'binary',
    'has_started_clinch' = 'binary',
    'is_initial_bomb_carrier_killed' = 'binary'
  )

  post_plant_features <- c(
    'opponent_diff' = 'integer',
    'is_kill_on_attempted_clinch' = 'binary',
    'has_started_clinch' = 'binary'
  )

  expect_equal(
    get_all_features(is_pre_plant = TRUE, named = FALSE),
    names(pre_plant_features)
  )

  expect_equal(
    get_all_features(is_pre_plant = FALSE, named = FALSE),
    names(post_plant_features)
  )

  expect_equal(
    get_all_features(is_pre_plant = TRUE, named = TRUE),
    pre_plant_features
  )

  expect_equal(
    get_all_features(is_pre_plant = FALSE, named = TRUE),
    post_plant_features
  )
})

test_that('validate_colnames works', {
  d <- tibble(
    'is_pre_plant' = TRUE,
    'side' = 'o',
    'n_team_remaining' = 3L,
    'n_opponent_remaining' = 2L,
    'model_seconds_remaining' = 15.5,

    'opponent_diff' = 1L,
    'is_kill_on_attempted_clinch' = FALSE,
    'has_started_clinch' = FALSE,
    'is_initial_bomb_carrier_killed' = FALSE
  )

  expect_true(validate_colnames(d, is_predict = FALSE))
})
