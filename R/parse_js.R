# Helpers that turn Tennis Abstract JavaScript into R objects.

# Positions (1-based) of the fields used from each match array. They are the
# same in the women's and men's career files; only the meaning of position 17
# differs (opponent date of birth for women, opponent age for men).
career_js_columns <- c(
  date = 1, tournament = 2, surface = 3, result = 5, round = 9,
  score = 10, opponent = 12, opponent_dob_or_age = 17
)

# Return the text of the array assigned to `var <variable> = [...]`.
extract_js_array <- function(js_text, variable = "morematchmx") {
  js_text <- paste(js_text, collapse = "\n")
  if (!nzchar(trimws(js_text))) {
    stop("The career JS file is empty.", call. = FALSE)
  }
  pattern <- paste0("(?s)var\\s+", variable, "\\s*=\\s*(\\[.*\\])")
  found <- regmatches(js_text, regexec(pattern, js_text, perl = TRUE))[[1]]
  if (length(found) == 0) {
    stop("Could not find the `", variable, "` match array in the career ",
         "JS file.", call. = FALSE)
  }
  found[2]
}

# Parse the array text into a list with one element (a list) per match.
parse_match_rows <- function(array_text) {
  rows <- tryCatch(jsonlite::fromJSON(array_text, simplifyVector = FALSE),
                   error = function(err) NULL)
  if (!is.list(rows) || !all(vapply(rows, is.list, logical(1)))) {
    stop("The career JS file is malformed: expected an array of match ",
         "arrays.", call. = FALSE)
  }
  rows
}

# Pull field `position` from every match as a string (NA when absent).
pull_field <- function(rows, position) {
  vapply(rows, function(row) {
    value <- if (length(row) >= position) row[[position]] else NULL
    if (length(value) != 1) return(NA_character_)
    as.character(value)
  }, character(1))
}

# Parse a career JS file into a raw data frame of character columns.
parse_career_js <- function(js_text) {
  array_text <- extract_js_array(js_text)
  rows <- parse_match_rows(array_text)
  if (length(rows) == 0) {
    stop("The career JS file contains no matches.", call. = FALSE)
  }
  columns <- lapply(career_js_columns, pull_field, rows = rows)
  as.data.frame(columns, stringsAsFactors = FALSE)
}
