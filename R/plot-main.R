
#' Autoplot for win probability model object
#'
#' @param object win probability model
#' @param type one of `grid` (default), `coefs`, and `round`
#' @importFrom ggplot2 autoplot
#' @return A ggplot2 plot
#' @export
autoplot.wp_model <- function(object, type = 'grid', ...) {
  type <- rlang::arg_match(type, c('grid', 'coefs', 'round'))

  switch(
    type,
    'grid' = plot_wp_grid(generate_wp_grid(object), ...),
    'coefs' = plot_coefs(object, ...),
    'round' = plot_round(object, ...)
  )
}
