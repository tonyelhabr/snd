library(rvest)
library(dplyr)
library(tibble)
library(purrr)
library(cropcircles)

bpgg_team_mapping <- c(
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

rev_bpgg_team_mapping <- set_names(
  names(bpgg_team_mapping),
  unname(bpgg_team_mapping)
)

scrape_team_logos <- function(season) {
  url <- sprintf('https://breakingpoint.gg/table/call-of-duty-league-%s/', season)
  page <- rvest::read_html(url)

  logo_elements <- rvest::html_elements(page, 'tbody > tr > td > a')

  logos <- tibble::tibble(
    team = rvest::html_text2(logo_elements),
    url =  logo_elements |>
      rvest::html_elements('span.team-logo > img') |>
      rvest::html_attr('src')
  )

  logos |>
    dplyr::filter(
      team %in% names(rev_bpgg_team_mapping)
    ) |>
    dplyr::transmute(
      team_abbr = rev_bpgg_team_mapping[team],
      url
    ) |>
    tibble::deframe() |>
    purrr::iwalk(
      ~{
        path <- file.path('inst', 'logos', 'bpgg', sprintf('%s.png', .y))
        path_cropped <- file.path('inst', 'logos', 'bpgg', 'circular', sprintf('%s.png', .y))
        if (file.exists(path) & file.exists(path_cropped)) {
          return(path)
        }
        download.file(.x, destfile = path, mode = 'wb')

        temp_path <- cropcircles::circle_crop(path)
        file.copy(temp_path, path_cropped)
        file.remove(temp_path)
        invisible(path)
      }
    )
}

c(2021, 2022, 2023) |> purrr::walk(scrape_team_logos)
# list.files(
#   file.path('inst', 'logos', 'bpgg'),
#   full.names = TRUE,
#   include.dirs = FALSE
# )
