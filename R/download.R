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

# Seconds to wait before each retry of a rate-limited (HTTP 429) request.
retry_wait_seconds <- c(10, 20, 30)

# Pause before retry number `attempt`; a separate function so tests can skip
# the wait.
pause_before_retry <- function(attempt) {
  Sys.sleep(retry_wait_seconds[attempt])
}

# Download a URL, waiting and retrying when Tennis Abstract rate-limits
# requests, which happens when several players are fetched in a row.
fetch_with_retry <- function(url) {
  max_tries <- length(retry_wait_seconds) + 1
  for (attempt in seq_len(max_tries)) {
    response <- fetch_url(url)
    if (response$status != 429 || attempt == max_tries) return(response)
    message("Tennis Abstract is limiting requests; retrying in ",
            retry_wait_seconds[attempt], " seconds...")
    pause_before_retry(attempt)
  }
}

# Stop with a helpful message for any unsuccessful HTTP status.
check_http_status <- function(status, url) {
  if (status == 429) {
    stop("Tennis Abstract is limiting requests (HTTP 429). ",
         "Please wait a few minutes and try again.", call. = FALSE)
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
  response <- fetch_with_retry(url)
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
  response <- fetch_with_retry(url)
  check_http_status(response$status, url)
  response$text
}
