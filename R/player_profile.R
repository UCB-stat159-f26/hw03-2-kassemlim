# Helpers that read the player's own details from their profile page. The
# career JS file does not include the player's date of birth, but the profile
# page defines it as `var dob = 19810926;` (for both men and women).

# Return the value of a scalar `var <variable> = value;` in page text.
extract_js_value <- function(page_text, variable) {
  page_text <- paste(page_text, collapse = "\n")
  pattern <- paste0("var\\s+", variable, "\\s*=\\s*['\"]?([^'\";\n]*)")
  found <- regmatches(page_text, regexec(pattern, page_text, perl = TRUE))[[1]]
  if (length(found) == 0) return(NA_character_)
  trimws(found[2])
}

# Read the player's full name and date of birth from their profile page.
parse_player_profile <- function(page_text, player_id) {
  dob <- parse_compact_date(extract_js_value(page_text, "dob"))
  if (is.na(dob)) {
    stop("Could not find a date of birth for ", player_id,
         " on Tennis Abstract.", call. = FALSE)
  }
  full_name <- extract_js_value(page_text, "fullname")
  if (is.na(full_name) || !nzchar(full_name)) full_name <- player_id
  list(full_name = full_name, dob = dob)
}
