# Helpers that turn a user-supplied player name into the identifier Tennis
# Abstract uses in its URLs ("serena williams" -> "SerenaWilliams").

# Stop unless `player_name` is a single, non-missing string.
check_player_name <- function(player_name) {
  if (!is.character(player_name) || length(player_name) != 1 ||
      is.na(player_name)) {
    stop("`player_name` must be a single string, e.g. \"Serena Williams\".",
         call. = FALSE)
  }
  invisible(player_name)
}

# Upper-case the first letter of each word, leaving the rest untouched.
capitalize_first <- function(words) {
  paste0(toupper(substring(words, 1, 1)), substring(words, 2))
}

# Split a name into ASCII letter-only words. Accents are transliterated and
# apostrophes/periods are dropped so "O'Connell" stays one word.
split_name_words <- function(player_name) {
  ascii_name <- iconv(trimws(player_name), to = "ASCII//TRANSLIT")
  if (is.na(ascii_name)) ascii_name <- player_name
  ascii_name <- gsub("['`\".^~]", "", ascii_name)
  words <- strsplit(ascii_name, "[^A-Za-z]+")[[1]]
  words[nzchar(words)]
}

# Build the Tennis Abstract identifier. Separated words are title-cased
# ("rafael nadal" -> "RafaelNadal"); a single word such as "SerenaWilliams"
# keeps its own capitalisation because word boundaries cannot be inferred.
normalize_player_name <- function(player_name) {
  check_player_name(player_name)
  words <- split_name_words(player_name)
  if (length(words) == 0) {
    stop("`player_name` must contain letters, e.g. \"Serena Williams\".",
         call. = FALSE)
  }
  if (length(words) == 1) return(capitalize_first(words))
  paste(capitalize_first(tolower(words)), collapse = "")
}

# URL of a player's career match file.
career_js_url <- function(player_id) {
  paste0(tennis_abstract_url, "/jsmatches/", player_id, "Career.js")
}

# URL of a player's profile page (works for both men and women).
player_profile_url <- function(player_id) {
  paste0(tennis_abstract_url, "/cgi-bin/player.cgi?p=", player_id)
}
