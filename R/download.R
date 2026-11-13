# Network helpers. `fetch_url()` is the only function in the package that
# talks to the internet, so tests replace it with a local mock.

# Download a URL and return its HTTP status code and body text.
fetch_url <- function(url) {
  handle <- curl::new_handle(useragent = "tenniscareer R package")
  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(err) stop("Could not connect to Tennis Abstract: ",
                               conditionMessage(err), call. = FALSE)
  )
  text <- rawToChar(response$content)
  Encoding(text) <- "UTF-8"
  list(status = response$status_code, text = text)
}

# Stop with a helpful message for any unsuccessful HTTP status.
check_http_status <- function(status, url) {
  if (status == 429) {
    stop("Tennis Abstract is limiting requests (HTTP 429). ",
         "Please wait a minute and try again.", call. = FALSE)
  }
  if (status != 200) {
    stop("Tennis Abstract returned HTTP status ", status, " for ", url, ".",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Download the text of a player's career JS file. A 404 means Tennis
# Abstract has no career file under that name.
download_career_js <- function(player_id, player_name) {
  url <- career_js_url(player_id)
  response <- fetch_url(url)
  if (response$status == 404) {
    stop("No Tennis Abstract career data found for \"", player_name,
         "\" (looked for ", url, "). Check the spelling and separate first ",
         "and last names with a space, e.g. \"Serena Williams\".",
         call. = FALSE)
  }
  check_http_status(response$status, url)
  response$text
}

# Download the HTML of a player's profile page.
download_player_profile <- function(player_id) {
  url <- player_profile_url(player_id)
  response <- fetch_url(url)
  check_http_status(response$status, url)
  response$text
}
