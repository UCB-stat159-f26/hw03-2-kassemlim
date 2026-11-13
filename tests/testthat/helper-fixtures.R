# Shared helpers for offline tests.

# Text of a JS fixture shipped in inst/extdata.
read_fixture <- function(file) {
  path <- system.file("extdata", file, package = "tenniscareer")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# Minimal profile page with the two variables the package reads; mirrors the
# `var fullname = '...'` / `var dob = ...` lines on real Tennis Abstract pages.
fake_profile <- function(full_name, dob) {
  sprintf("<script>\nvar fullname = '%s';\nvar dob = %s;\n</script>",
          full_name, dob)
}

# The mockup files hold real rows from these two players' career files.
fixture_players <- list(
  SerenaWilliams = list(file = "FemalePlayerCareer.js",
                        name = "Serena Williams", dob = "19810926"),
  RafaelNadal = list(file = "MalePlayerCareer.js",
                     name = "Rafael Nadal", dob = "19860603")
)

# Replace fetch_url() with a fake Tennis Abstract serving the fixtures.
# Unknown players get a 404, like the real site. Requested URLs are logged.
local_fake_tennis_abstract <- function(env = parent.frame()) {
  log <- new.env()
  log$urls <- character(0)
  local_mocked_bindings(fetch_url = function(url) {
    log$urls <- c(log$urls, url)
    fake_response(url)
  }, .env = env)
  log
}

fake_response <- function(url) {
  player_id <- sub("Career\\.js$", "", sub(".*(jsmatches/|p=)", "", url))
  player <- fixture_players[[player_id]]
  if (is.null(player)) return(list(status = 404, text = "Not Found"))
  if (grepl("Career.js", url, fixed = TRUE)) {
    return(list(status = 200, text = read_fixture(player$file)))
  }
  list(status = 200, text = fake_profile(player$name, player$dob))
}

# Raw match data frame with the given values (one row per element).
make_raw_matches <- function(date = "20010910", result = "W",
                             score = "6-4 6-4", opponent_dob_or_age = "22.5") {
  data.frame(date = date, tournament = "Test Open", surface = "Hard",
             result = result, round = "R32", score = score,
             opponent = "Opponent", opponent_dob_or_age = opponent_dob_or_age,
             stringsAsFactors = FALSE)
}
