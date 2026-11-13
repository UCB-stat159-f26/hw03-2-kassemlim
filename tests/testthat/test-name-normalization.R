test_that("lowercase names with spaces are title-cased", {
  expect_equal(normalize_player_name("serena williams"), "SerenaWilliams")
  expect_equal(normalize_player_name("rafael nadal"), "RafaelNadal")
})

test_that("capitalized names are joined", {
  expect_equal(normalize_player_name("Serena Williams"), "SerenaWilliams")
  expect_equal(normalize_player_name("Rafael Nadal"), "RafaelNadal")
})

test_that("names without spaces keep their capitalisation", {
  expect_equal(normalize_player_name("SerenaWilliams"), "SerenaWilliams")
  expect_equal(normalize_player_name("GastonSanchez"), "GastonSanchez")
  expect_equal(normalize_player_name("serenawilliams"), "Serenawilliams")
})

test_that("upper case, extra spaces, hyphens and particles are handled", {
  expect_equal(normalize_player_name("SERENA WILLIAMS"), "SerenaWilliams")
  expect_equal(normalize_player_name("  serena   williams "), "SerenaWilliams")
  expect_equal(normalize_player_name("Felix Auger-Aliassime"),
               "FelixAugerAliassime")
  expect_equal(normalize_player_name("Juan Martin del Potro"),
               "JuanMartinDelPotro")
  expect_equal(normalize_player_name("John McEnroe"), "JohnMcenroe")
})

test_that("accents and apostrophes are removed", {
  expect_equal(normalize_player_name("Gaël Monfils"), "GaelMonfils")
  expect_equal(normalize_player_name("Christopher O'Connell"),
               "ChristopherOconnell")
})

test_that("non-string or empty names give meaningful errors", {
  expect_error(normalize_player_name(42), "single string")
  expect_error(normalize_player_name(c("a b", "c d")), "single string")
  expect_error(normalize_player_name(NA_character_), "single string")
  expect_error(normalize_player_name(""), "must contain letters")
  expect_error(normalize_player_name("123 !!"), "must contain letters")
})

test_that("Tennis Abstract URLs are built from the player id", {
  expect_equal(
    career_js_url("SerenaWilliams"),
    "https://www.tennisabstract.com/jsmatches/SerenaWilliamsCareer.js"
  )
  expect_equal(
    player_profile_url("RafaelNadal"),
    "https://www.tennisabstract.com/cgi-bin/player.cgi?p=RafaelNadal"
  )
})
