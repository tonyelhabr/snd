
logos_dir <- file.path('figs', 'img')

bpgg_team_mapping <- c(
  'ATL' = 'Atlanta FaZe',
  'BOS' = 'Boston Breach',
  'DAL' = 'Dallas Empire',
  'FLA' = 'Florida Mutineers',
  'LAG' = 'Los Angeles Guerrillas',
  'LAT' = 'Los Angeles Thieves',
  'LDN' = 'London Royal Ravents',
  'MIN' = 'Minnesota ROKKR',
  'NYS' = 'New York Subliners',
  'OPT' = 'OpTic Texas',
  'LVL' = 'Las Vegas Legion',
  'PAR' = 'Paris Legion',
  'SEA' = 'Seattle Surge',
  'TOR' = 'Toronto Ultra'
)

bpgg_team_logo_urls_mapping <- c(
  'Minnesota ROKKR' = file.path(logos_dir, 'Minnesota-Rokkr-Logo-119x128_cropped.png'),
  'Boston Breach' = file.path(logos_dir, 'Boston-Breach-128x122_cropped.png'),
  'London Royal Ravens' = file.path(logos_dir, 'London-Royal-Ravens-Logo-128x68_cropped.png'),
  'OpTic Texas' = file.path(logos_dir, 'OpTic-Texas-Logo-128x72_cropped.png'),
  'Los Angeles Guerrillas' = file.path(logos_dir, 'Los-Angeles-Guerrillas-Logo-93x128_cropped.png'),
  'Toronto Ultra' = file.path(logos_dir, 'Toronto-Ultra-Logo-128x128_cropped.png'),
  'Atlanta FaZe' = file.path(logos_dir, 'Atlanta-FaZe-Logo-128x62_cropped.png'),
  'Los Angeles Thieves' = file.path(logos_dir, 'Los-Angeles-Thieves-Logo-128x128_cropped.png'),
  'Florida Mutineers' = file.path(logos_dir, 'Florida-Mutineers-Logo-102x128_cropped.png'),
  'Seattle Surge' = file.path(logos_dir, 'Seattle-Surge-Logo-99x128_cropped.png'),
  'Las Vegas Legion' = file.path(logos_dir, 'Paris-Legion-Logo-104x128_cropped.png'),
  'New York Subliners' = file.path(logos_dir, 'New-York-Subliners-Logo-1-103x128_cropped.png')
)

team_color_mapping <- c(
  'ATL' = '#e43d30',
  'BOS' = '#02FF5B',
  'DAL' = '#B5A26A',
  'FLA' = '#025157',
  'LAG' = '#60269e',
  'LAT' = '#ef3232',
  'LDN' = '#CF152D',
  'MIN' = '#351f68',
  'NYS' = '#FEE306',
  'OPT' = '#9dc73b',
  'PAR' = '#EE7623',
  'SEA' = '#16667D',
  'TOR' = '#773dbd'
)

add_aesthetic_cols <- function(data) {
  data |>
    dplyr::mutate(
      'team_label' = bpgg_team_mapping[.data[['team']]],
      'opponent_label' = bpgg_team_mapping[.data[['opponent']]],
      'team_logo_url' = bpgg_team_logo_urls_mapping[.data[['team_label']]],
      'opponent_logo_url' = bpgg_team_logo_urls_mapping[.data[['opponent_label']]],
      'team_color' = team_color_mapping[.data[['team']]],
      'opponent_color' = team_color_mapping[.data[['opponent']]],
      'activity_team_color' = dplyr::case_when(
        .data[['team']] == .data[['activity_team']] ~ .data[['team_color']],
        .data[['team']] == .data[['activity_opponent']] ~ .data[['opponent_color']],
        TRUE ~ NA_character_
      ),
      'activity_opponent_color' = dplyr::case_when(
        .data[['opponent']] == .data[['activity_opponent']] ~ .data[['opponent_color']],
        .data[['opponent']] == .data[['activity_team']] ~ .data[['team_color']],
        TRUE ~ NA_character_
      )
    )

}
