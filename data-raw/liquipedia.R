library(rvest)
library(dplyr)
library(tibble)
library(purrr)

liquipedia_page <- rvest::read_html('https://liquipedia.net/callofduty/Portal:Teams')
liquipedia_team_links <- liquipedia_page |>
  rvest::html_elements('.team-template-image-icon.team-template-lightmode') |>
  rvest::html_children()

all_liquipedia_team_logos <- tibble::tibble(
  team = rvest::html_attr(team_links, 'title'),
  url = sprintf('https://liquipedia.net%s', team_links |> rvest::html_children() |> rvest::html_attr('src'))
)

liquipedia_team_mapping <- c(
  'ATL' = 'Atlanta FaZe',
  'BOS' = 'Boston Breach',
  'DAL' = 'Dallas Empire',
  'FLA' = 'Florida Mutineers',
  'LAG' = 'Los Angeles Guerrillas',
  'LAT' = 'Los Angeles Thieves',
  'LDN' = 'London Royal Ravens',
  'MIN' = 'Minnesota R\U00D8KKR',
  'NYSL' = 'New York Subliners',
  'OPT' = 'OpTic Texas',
  'LVL' = 'Las Vegas Legion',
  'PAR' = 'Paris Legion',
  'SEA' = 'Seattle Surge',
  'TOR' = 'Toronto Ultra'
)

rev_liquipedia_team_mapping <- set_names(
  names(liquipedia_team_mapping),
  unname(liquipedia_team_mapping)
)

liquipedia_team_logos <- all_liquipedia_team_logos |>
  dplyr::filter(
    team %in% names(rev_liquipedia_team_mapping)
  ) |>
  dplyr::transmute(
    team_abbr = rev_liquipedia_team_mapping[team],
    url
  ) |>
  tibble::deframe()

liquipedia_team_logos |>
  purrr::iwalk(
    ~{
      path <- file.path('inst', 'logos', 'liquipedia', sprintf('%s.png', .y))
      if (file.exists(path)) {
        return(path)
      }
      download.file(.x, destfile = path, mode = 'wb')
      invisible(path)
    }
  )
