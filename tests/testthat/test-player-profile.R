test_that("profile pages yield the player's name and date of birth", {
  profile <- parse_player_profile(
    fake_profile("Serena Williams", "19810926"), "SerenaWilliams"
  )
  expect_equal(profile$full_name, "Serena Williams")
  expect_equal(profile$dob, as.Date("1981-09-26"))
})

test_that("profile variables are not confused with similar names", {
  page <- "var dobStr = dob+''\nvar dob = 19860603;\nvar fullname = 'Rafael Nadal';"
  expect_equal(extract_js_value(page, "dob"), "19860603")
  expect_true(is.na(extract_js_value("var dobStr = 1;", "dob")))
})

test_that("profiles without a valid date of birth give an error", {
  expect_error(parse_player_profile("<html></html>", "GastonSanchez"),
               "Could not find a date of birth")
  expect_error(parse_player_profile("var dob = 0;", "X"),
               "Could not find a date of birth")
})

test_that("a missing full name falls back to the player id", {
  profile <- parse_player_profile("var dob = 19860603;", "RafaelNadal")
  expect_equal(profile$full_name, "RafaelNadal")
})
