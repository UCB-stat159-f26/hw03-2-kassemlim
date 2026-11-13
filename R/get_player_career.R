#' Get a player's career match data from Tennis Abstract
#'
#' Downloads a player's career match file from Tennis Abstract, along with
#' the player's date of birth from their profile page, and returns one row per
#' played singles match with the ages needed for plotting.
#'
#' Women's career files list each opponent's date of birth, so the opponent's
#' age is computed from it. Men's career files already list the opponent's
#' age, which is used as is. The file format is detected automatically.
#'
#' Ages are based on the tournament start date, which Tennis Abstract uses as
#' the match date, so they are approximate to within about two weeks.
#' Walkovers, defaults, and matches with a missing or invalid date, result,
#' or age are dropped.
#'
#' @param player_name A player's name as a single string. Case and spacing are
#'   flexible: `"serena williams"`, `"Serena Williams"`, and
#'   `"SerenaWilliams"` all work.
#'
#' @return A data frame sorted by date with columns:
#'   \describe{
#'     \item{player}{Player's full name.}
#'     \item{match_date}{Tournament start date (`Date`).}
#'     \item{tournament, surface, round, opponent, score}{Match details.}
#'     \item{result}{Factor with levels `"Win"` and `"Loss"`.}
#'     \item{player_age}{Player's age in years at the match.}
#'     \item{opponent_age}{Opponent's age in years at the match.}
#'     \item{age_diff}{`player_age - opponent_age`; positive means the
#'       player was older.}
#'   }
#'
#' @seealso [plot_player_career()] to plot the result.
#' @export
#' @examples
#' \dontrun{
#' serena <- get_player_career("serena williams")
#' rafa <- get_player_career("Rafael Nadal")
#' head(rafa)
#' }
get_player_career <- function(player_name) {
  player_id <- normalize_player_name(player_name)
  career_js <- download_career_js(player_id, player_name)
  profile <- parse_player_profile(download_player_profile(player_id), player_id)
  raw_matches <- parse_career_js(career_js)
  process_career_data(raw_matches, profile$dob, profile$full_name)
}
