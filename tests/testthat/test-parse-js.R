test_that("female fixture parses into the expected raw columns", {
  raw <- parse_career_js(read_fixture("FemalePlayerCareer.js"))
  expect_equal(nrow(raw), 3)
  expect_named(raw, names(career_js_columns))
  expect_equal(raw$date, c("19951030", "19970307", "19970804"))
  expect_equal(raw$opponent[2], "Alexia Dechaume Balleret")
  expect_equal(raw$opponent_dob_or_age, c("19770119", "19700503", "19790301"))
  expect_equal(raw$result, c("L", "L", "L"))
})

test_that("male fixture parses into the expected raw columns", {
  raw <- parse_career_js(read_fixture("MalePlayerCareer.js"))
  expect_equal(nrow(raw), 3)
  expect_equal(raw$tournament, c("Spain F10", "Seville CH", "Seville CH"))
  expect_equal(raw$opponent_dob_or_age,
               c("22.264202601", "24.2902121834", "18.7597535934"))
  expect_equal(raw$result, c("L", "L", "W"))
  expect_equal(raw$score[1], "2-6 7-5 6-2")
})

test_that("empty strings are kept as empty strings by the parser", {
  raw <- parse_career_js(read_fixture("FemalePlayerCareer.js"))
  expect_true(all(is.character(unlist(raw))))
  js <- 'var morematchmx = [["20010910", "", "", "", "W", "", "", "", "", "",
    "", "", "", "", "", "", ""]];'
  raw <- parse_career_js(js)
  expect_equal(raw$tournament, "")
  expect_equal(raw$opponent_dob_or_age, "")
})

test_that("short rows and JSON nulls become NA instead of failing", {
  js <- 'var morematchmx = [["20010910", "Open", "Hard", "S", "W"],
    ["20010910", null, "Hard", "S", "L", "", "", "", "", "", "", "X", "",
     "", "", "", "20.5"]];'
  raw <- parse_career_js(js)
  expect_equal(raw$opponent_dob_or_age, c(NA, "20.5"))
  expect_equal(raw$tournament, c("Open", NA))
})

test_that("leading comments and missing semicolons are tolerated", {
  js <- paste0("// header comment\n",
               sub(";\\s*$", "", read_fixture("MalePlayerCareer.js")))
  expect_equal(nrow(parse_career_js(js)), 3)
})

test_that("empty JS files give meaningful errors", {
  expect_error(parse_career_js(""), "empty")
  expect_error(parse_career_js(character(0)), "empty")
  expect_error(parse_career_js("var morematchmx = [];"), "no matches")
})

test_that("malformed JS files give meaningful errors", {
  expect_error(parse_career_js("<html>404 Not Found</html>"),
               "Could not find the `morematchmx`")
  expect_error(parse_career_js('var morematchmx = [["2001", "a"],, ];'),
               "malformed")
  truncated <- substr(read_fixture("FemalePlayerCareer.js"), 1, 300)
  expect_error(parse_career_js(paste0(truncated, "]")), "malformed")
  expect_error(parse_career_js('var morematchmx = ["a", "b"];'),
               "expected an array of match arrays")
  expect_error(parse_career_js("var morematchmx = [null];"), "malformed")
})
