# Helpers that clean the raw match data and compute ages.

# Convert "YYYYMMDD" strings to Dates; anything else becomes NA.
parse_compact_date <- function(x) {
  x <- trimws(as.character(x))
  x[!grepl("^[0-9]{8}$", x)] <- NA
  as.Date(x, format = "%Y%m%d")
}

# Age in years on `on_date` for someone born on `birth_date`.
age_in_years <- function(birth_date, on_date) {
  as.numeric(difftime(on_date, birth_date, units = "days")) / 365.25
}

# Replace missing or implausible ages with NA.
clean_age <- function(age, valid_range = c(10, 70)) {
  age[!is.finite(age) | age < valid_range[1] | age > valid_range[2]] <- NA
  age
}

# Decide whether position 17 holds opponent birth dates ("female" format,
# e.g. "19770119") or opponent ages ("male" format, e.g. "22.264202601").
# Whichever kind of value is more common wins; blanks and junk are ignored.
detect_career_format <- function(values) {
  values <- trimws(values)
  n_dob <- sum(grepl("^[0-9]{8}$", values))
  n_age <- sum(grepl("^[0-9]{1,2}(\\.[0-9]+)?$", values))
  if (n_dob > n_age) return("female")
  if (n_age > n_dob) return("male")
  stop("Could not tell whether the career file lists opponent birth dates ",
       "or opponent ages.", call. = FALSE)
}

# Opponent age at each match: derived from the birth date for the female
# format, read directly for the male format.
compute_opponent_age <- function(values, match_date, format) {
  if (format == "female") {
    return(age_in_years(parse_compact_date(values), match_date))
  }
  suppressWarnings(as.numeric(values))
}

# Map "W"/"L" codes to a Win/Loss factor; other codes become NA.
standardize_result <- function(result) {
  result <- toupper(trimws(result))
  labels <- c(W = "Win", L = "Loss")
  factor(unname(labels[result]), levels = c("Win", "Loss"))
}

# TRUE for walkovers and defaults, which were never played.
is_unplayed <- function(score) {
  grepl("W/O|DEF", toupper(score))
}

# Stop unless `raw_matches` looks like the output of parse_career_js().
check_raw_matches <- function(raw_matches) {
  if (!is.data.frame(raw_matches) ||
      !all(names(career_js_columns) %in% names(raw_matches))) {
    stop("`raw_matches` must be a data frame with columns: ",
         paste(names(career_js_columns), collapse = ", "), ".", call. = FALSE)
  }
  if (nrow(raw_matches) == 0) stop("`raw_matches` has no rows.", call. = FALSE)
  invisible(raw_matches)
}

# Tidy the descriptive (non-age) columns of the raw matches.
clean_match_details <- function(raw_matches) {
  data.frame(
    match_date = parse_compact_date(raw_matches$date),
    tournament = trimws(raw_matches$tournament),
    surface = trimws(raw_matches$surface),
    round = trimws(raw_matches$round),
    opponent = trimws(raw_matches$opponent),
    score = trimws(raw_matches$score),
    result = standardize_result(raw_matches$result),
    stringsAsFactors = FALSE
  )
}

# Keep played matches with every value needed for plotting, sorted by date.
keep_complete_matches <- function(career) {
  needed <- c("match_date", "result", "player_age", "opponent_age")
  usable <- rowSums(is.na(career[needed])) == 0 & !is_unplayed(career$score)
  career <- career[usable, , drop = FALSE]
  if (nrow(career) == 0) {
    stop("No usable matches remain after cleaning the career data.",
         call. = FALSE)
  }
  career <- career[order(career$match_date), , drop = FALSE]
  rownames(career) <- NULL
  career
}

# Turn raw matches into the clean data frame returned by get_player_career().
process_career_data <- function(raw_matches, player_dob, player_name) {
  check_raw_matches(raw_matches)
  career <- clean_match_details(raw_matches)
  format <- detect_career_format(raw_matches$opponent_dob_or_age)
  opponent_age <- compute_opponent_age(
    raw_matches$opponent_dob_or_age, career$match_date, format
  )
  career$player_age <- clean_age(age_in_years(player_dob, career$match_date))
  career$opponent_age <- clean_age(opponent_age)
  career$age_diff <- career$player_age - career$opponent_age
  career <- data.frame(player = player_name, career, stringsAsFactors = FALSE)
  keep_complete_matches(career)
}
