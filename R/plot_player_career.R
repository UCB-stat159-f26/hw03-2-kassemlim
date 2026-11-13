#' Plot a player's career: age difference against age
#'
#' Draws a scatter plot in the style of the New York Times graphic built on
#' Tennis Abstract data (without annotations). Each point is a match; the
#' x-axis is the player's age, the y-axis is the player's age minus the
#' opponent's age, and colour shows whether the player won or lost.
#'
#' @param career A data frame returned by [get_player_career()]. It must have
#'   numeric `player_age` and `age_diff` columns and a `result` column
#'   containing only `"Win"` and `"Loss"`.
#'
#' @return A `ggplot` object, which can be printed, customised with further
#'   ggplot2 layers, or saved with [ggplot2::ggsave()].
#'
#' @export
#' @examples
#' \dontrun{
#' serena <- get_player_career("Serena Williams")
#' plot_player_career(serena)
#' }
plot_player_career <- function(career) {
  check_career_data(career)
  career$result <- factor(career$result, levels = c("Win", "Loss"))
  ggplot2::ggplot(
    career,
    ggplot2::aes(x = .data$player_age, y = .data$age_diff, color = .data$result)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_manual(values = c(Win = "blue", Loss = "purple")) +
    ggplot2::labs(
      title = career_plot_title(career),
      x = "Player's age (years)",
      y = "Age difference (player - opponent, years)",
      color = "Outcome",
      caption = "Source: Tennis Abstract"
    ) +
    ggplot2::theme_minimal()
}

# Stop unless `career` has the columns and types the plot needs.
check_career_data <- function(career) {
  if (!is.data.frame(career)) {
    stop("`career` must be a data frame from get_player_career().",
         call. = FALSE)
  }
  missing_cols <- setdiff(c("player_age", "age_diff", "result"), names(career))
  if (length(missing_cols) > 0) {
    stop("`career` is missing column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  check_career_values(career)
}

# Stop unless the plotted columns hold usable values.
check_career_values <- function(career) {
  if (nrow(career) == 0) stop("`career` has no rows to plot.", call. = FALSE)
  if (!is.numeric(career$player_age) || !is.numeric(career$age_diff)) {
    stop("`player_age` and `age_diff` must be numeric.", call. = FALSE)
  }
  if (!all(as.character(career$result) %in% c("Win", "Loss"))) {
    stop("`result` must contain only \"Win\" and \"Loss\".", call. = FALSE)
  }
  invisible(career)
}

# "Serena Williams: Career Matches" when the data name one player.
career_plot_title <- function(career) {
  player <- unique(career$player)
  if (length(player) != 1 || is.na(player)) return("Career Matches")
  paste0(player, ": Career Matches")
}
