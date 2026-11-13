test_that("female player data are retrieved through the mocked site", {
  requests <- local_fake_tennis_abstract()
  serena <- get_player_career("serena williams")
  expect_equal(nrow(serena), 3)
  expect_equal(unique(serena$player), "Serena Williams")
  expect_equal(serena$opponent_age[1], 6858 / 365.25)
  expect_equal(requests$urls, c(career_js_url("SerenaWilliams"),
                                player_profile_url("SerenaWilliams")))
})

test_that("male player data are retrieved through the mocked site", {
  local_fake_tennis_abstract()
  rafa <- get_player_career("Rafael Nadal")
  expect_equal(unique(rafa$player), "Rafael Nadal")
  expect_equal(rafa$opponent_age[3], 18.7597535934)
  expect_equal(as.character(rafa$result), c("Loss", "Loss", "Win"))
})

test_that("different name formats return the same data", {
  local_fake_tennis_abstract()
  reference <- get_player_career("Serena Williams")
  expect_equal(get_player_career("serena williams"), reference)
  expect_equal(get_player_career("SerenaWilliams"), reference)
  expect_equal(get_player_career("rafael nadal"),
               get_player_career("Rafael Nadal"))
})

test_that("invalid player names give a meaningful error", {
  requests <- local_fake_tennis_abstract()
  expect_error(get_player_career("GastonSanchez"),
               "No Tennis Abstract career data found for \"GastonSanchez\"")
  expect_error(get_player_career("Gaston Sanchez"), "Check the spelling")
  expect_false(any(grepl("player.cgi", requests$urls)))
  expect_error(get_player_career(NULL), "single string")
})

test_that("HTTP failures other than 404 give meaningful errors", {
  local_mocked_bindings(fetch_url = function(url) {
    list(status = 429, text = "Too Many Requests")
  })
  expect_error(get_player_career("Serena Williams"), "HTTP 429")
  local_mocked_bindings(fetch_url = function(url) {
    list(status = 500, text = "")
  })
  expect_error(get_player_career("Serena Williams"), "HTTP status 500")
})

test_that("a non-JS response for a valid URL is reported as malformed", {
  local_mocked_bindings(fetch_url = function(url) {
    list(status = 200, text = "var dob = 19810926; <html></html>")
  })
  expect_error(get_player_career("Serena Williams"), "morematchmx")
})
