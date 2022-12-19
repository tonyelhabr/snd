generate_seconds_grid <- function(is_pre_plant) {
  if (isTRUE(is_pre_plant)) {
    min_second <- c(seq(0, 75, by = 3), seq(75, 85, by = 1), seq(86, 90, by = 0.5))
    max_second <- c(seq(15, 90, by = 3), seq(80, 90, by = 1), rep(90, 9))
  } else {
    min_second <- c(seq(0, 30, 2), seq(30, 38, 1), seq(39, 45, by = 0.5))
    max_second <- c(seq(10, 40, 2), seq(37, 45, 1), rep(45, 13))
  }
  tibble::tibble(
    min_second = min_second,
    max_second = max_second
  )
}

seconds_grid <- list(
  'pre' = generate_seconds_grid(is_pre_plant = TRUE),
  'post' = generate_seconds_grid(is_pre_plant = FALSE)
)

usethis::use_data(seconds_grid, internal = TRUE)
