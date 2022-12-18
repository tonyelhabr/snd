
# RColorBrewer::brewer.pal(7, 'PRGn') |> datapasta::vector_paste_vertical()
opponent_diff_pal <- c(
  '-3' = '#762A83',
  '-2' = '#AF8DC3',
  '-1' = '#E7D4E8',
  '0' = '#F7F7F7',
  '1' = '#D9F0D3',
  '2' = '#7FBF7B',
  '3' = '#1B7837'
)

summarize_pred_grid_across_features <- function(data, binary_feature_name) {
  data |>
    dplyr::group_by(
      .data[['is_pre_plant']],
      .data[['model_seconds_elapsed']],
      .data[['opponent_diff']],
      value = .data[[binary_feature_name]]
    ) |>
    dplyr::summarize(
      'wp' = mean(.data[['wp']])
    ) |>
    dplyr::ungroup()
}
max_pre_plant_second_buffer <- 0L ## for plot

scale_x_continuous_wp_states <- function(...) {
  list(
    ...,
    ggplot2::scale_x_continuous(
      breaks = c(
        0,
        max_pre_plant_second / 2,
        max_pre_plant_second,
        max_pre_plant_second + max_pre_plant_second_buffer / 2,
        max_pre_plant_second + max_pre_plant_second_buffer,
        max_pre_plant_second + max_pre_plant_second_buffer + 25,
        max_pre_plant_second + max_pre_plant_second_buffer + 45
      ),
      labels = c('90', '45', '', 'Plant', '', '20', '0')
    ),
    ggplot2::theme(
      legend.position = 'top'
    ),
    ggplot2::labs(
      x = 'Seconds remaining'
    )
  )
}

prep_wp_grid_data <- function(data, feature_name) {
  filt <- dplyr::filter(
    data,
    .data[['side']] == 'o'
  )

  agg <- if (!is.null(feature_name)) {
    filt |>
      dplyr::group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']],
        'feature_value' = .data[[feature_name]]
      ) |>
      dplyr::summarize(
        'wp' = mean(.data[['wp']])
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        'feature' = sprintf(
          '%s: %s',
          feature_name,
          .data[['feature_value']]
        )
      )
  } else {
    filt |>
      dplyr::group_by(
        .data[['is_pre_plant']],
        .data[['model_seconds_elapsed']],
        .data[['opponent_diff']]
      ) |>
      dplyr::summarize(
        'wp' = mean(.data[['wp']])
      ) |>
      dplyr::ungroup()
  }

  dplyr::bind_rows(
    dplyr::filter(agg, .data[['is_pre_plant']]),
    agg |>
      dplyr::filter(!.data[['is_pre_plant']]) |>
      dplyr::mutate(
        'model_seconds_elapsed' = .data[['model_seconds_elapsed']] + max_pre_plant_second + max_pre_plant_second_buffer
      )
  )

}

plot_wp_grid <- function(data, feature_name, ...) {

  df <- prep_wp_grid_data(
    data = data,
    feature_name = feature_name
  )

  p <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[['model_seconds_elapsed']], y = .data[['wp']]) +
    ggplot2::geom_hline(
      color = 'white',
      ggplot2::aes(yintercept = 0.5)
    ) +
    ggplot2::geom_vline(
      color = 'white',
      data = tibble::tibble(
        'is_pre_plant' = TRUE,
        'x' = max_pre_plant_second
      ),
      ggplot2::aes(xintercept = .data[['x']])
    ) +
    ggplot2::geom_step(
      size = 1,
      ggplot2::aes(color = factor(.data[['opponent_diff']]))
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = 'Net # of players',
        override.aes = list(size = 3)
      )
    ) +
    ggplot2::scale_color_manual(values = opponent_diff_pal) +
    scale_x_continuous_wp_states() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme(
      legend.position = 'top'
    ) +
    ggplot2::labs(
      title = 'Offensive Win Probability',
      y = 'Win probability'
    )

  if (is.null(feature_name)) {
    return(p)
  }

  p + ggplot2::facet_wrap(~.data[['feature']])

}
