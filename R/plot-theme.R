blackish_background <- '#1c1c1c'
gray_points <- '#4d4d4d'
gray_text <- '#999999'
font <- 'Titillium Web'

#' SnD theme
#'
#' @rdname theme_snd
#' @export
theme_snd <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme_update(
      text = ggplot2::element_text(family = font),
      title = ggplot2::element_text(size = 14, color = 'white'),
      plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = 'white'),
      plot.title.position = 'plot',
      plot.subtitle = ggplot2::element_text(size = 14, color = '#f1f1f1'),
      axis.text = ggplot2::element_text(color = 'white', size = 14),
      axis.title = ggplot2::element_text(size = 14, color = 'white', face = 'bold', hjust = 0.99),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = gray_points),
      panel.grid.minor = ggplot2::element_line(color = gray_points),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = 'white', size = 12, face = 'bold'),
      strip.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background),
      legend.title = ggplot2::element_text(color = 'white', size = 12, face = 'bold'),
      legend.text = ggplot2::element_text(color = 'white', size = 12, face = 'plain'),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background),
      plot.caption = ggplot2::element_text(size = 12, color = 'white', hjust = 1),
      plot.caption.position = 'plot',
      plot.tag = ggplot2::element_text(size = 12, color = 'white', hjust = 0),
      plot.tag.position = c(0.01, 0.01),
      panel.background = ggplot2::element_rect(fill = blackish_background, color = blackish_background)
    )

}

#' Set SnD theme
#'
#' @rdname theme_snd
#' @export
theme_set_snd <- function() {
  extrafont::loadfonts('all', quiet = TRUE)
  ggplot2::theme_set(theme_snd())
  ggplot2::update_geom_defaults('text', list(family = font, size = 10 / ggplot2::.pt, fontface = 'bold'))
  ggplot2::update_geom_defaults('point', list(color = 'white'))
  ggplot2::update_geom_defaults('segment', list(color = 'white'))
  ggplot2::update_geom_defaults('step', list(color = 'white'))
  ggplot2::update_geom_defaults('line', list(color = 'white'))
}
