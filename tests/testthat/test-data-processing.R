# Day counts below were worked out by hand from the calendar, so the
# expected ages do not reuse the package's own date arithmetic.
serena_dob <- as.Date("1981-09-26")
rafa_dob <- as.Date("1986-06-03")

female_career <- function() {
  raw <- parse_career_js(read_fixture("FemalePlayerCareer.js"))
  process_career_data(raw, serena_dob, "Serena Williams")
}

male_career <- function() {
  raw <- parse_career_js(read_fixture("MalePlayerCareer.js"))
  process_career_data(raw, rafa_dob, "Rafael Nadal")
}

test_that("the career file format is detected from position 17", {
  expect_equal(detect_career_format(c("19770119", "19700503")), "female")
  expect_equal(detect_career_format(c("22.264202601", "18", "")), "male")
  expect_equal(detect_career_format(c("19770119", "", NA, "bad")), "female")
  expect_error(detect_career_format(c("", NA)), "Could not tell")
  expect_error(detect_career_format(c("abc", "x")), "Could not tell")
})

test_that("female data produce the expected clean data frame", {
  career <- female_career()
  expect_s3_class(career, "data.frame")
  expect_named(career, c("player", "match_date", "tournament", "surface",
                         "round", "opponent", "score", "result",
                         "player_age", "opponent_age", "age_diff"))
  expect_equal(nrow(career), 3)
  expect_equal(career$player, rep("Serena Williams", 3))
  expect_equal(career$match_date,
               as.Date(c("1995-10-30", "1997-03-07", "1997-08-04")))
})

test_that("female opponent ages are computed from their birth dates", {
  career <- female_career()
  # Anne Miller, born 1977-01-19, played 1995-10-30: 6858 days.
  expect_equal(career$opponent_age[1], 6858 / 365.25)
  # Magui Serna, born 1979-03-01, played 1997-08-04: 6731 days.
  expect_equal(career$opponent_age[3], 6731 / 365.25)
})

test_that("male opponent ages are used as given", {
  career <- male_career()
  expect_equal(career$opponent_age,
               c(22.264202601, 24.2902121834, 18.7597535934))
})

test_that("player ages are computed from the player's date of birth", {
  # Serena (1981-09-26) on 1995-10-30: 5147 days.
  expect_equal(female_career()$player_age[1], 5147 / 365.25)
  # Rafael Nadal (1986-06-03) on 2001-09-10: 5578 days.
  expect_equal(male_career()$player_age[1], 5578 / 365.25)
})

test_that("age differences are player age minus opponent age", {
  female <- female_career()
  male <- male_career()
  expect_equal(female$age_diff, female$player_age - female$opponent_age)
  expect_equal(female$age_diff[1], (5147 - 6858) / 365.25)
  expect_equal(male$age_diff[1], 5578 / 365.25 - 22.264202601)
})

test_that("match outcomes are converted to a Win/Loss factor", {
  expect_equal(levels(male_career()$result), c("Win", "Loss"))
  expect_equal(as.character(male_career()$result), c("Loss", "Loss", "Win"))
  expect_equal(as.character(female_career()$result), rep("Loss", 3))
  expect_equal(as.character(standardize_result(c("w", " L ", "X", ""))),
               c("Win", "Loss", NA, NA))
})

test_that("matches are sorted by date with clean row names", {
  raw <- make_raw_matches(date = c("20030101", "20010101"),
                          opponent_dob_or_age = c("20", "21"))
  career <- process_career_data(raw, rafa_dob, "Rafael Nadal")
  expect_equal(career$match_date, as.Date(c("2001-01-01", "2003-01-01")))
  expect_equal(rownames(career), c("1", "2"))
})

test_that("missing values and empty strings drop only the affected rows", {
  raw <- make_raw_matches(
    date = rep("20050101", 5),
    result = c("W", "", "L", NA, "W"),
    opponent_dob_or_age = c("20", "21", "", NA, "23")
  )
  career <- process_career_data(raw, rafa_dob, "Rafael Nadal")
  expect_equal(career$opponent_age, c(20, 23))
})

test_that("malformed dates are dropped", {
  expect_equal(parse_compact_date(c("19951030", "19971340", "2001-09-10",
                                    "", NA, "1995103")),
               as.Date(c("1995-10-30", NA, NA, NA, NA, NA)))
  raw <- make_raw_matches(date = c("20050101", "20051340", "2005-01-01"),
                          opponent_dob_or_age = c("20", "21", "22"))
  career <- process_career_data(raw, rafa_dob, "Rafael Nadal")
  expect_equal(nrow(career), 1)
})

test_that("invalid opponent ages are dropped", {
  raw <- make_raw_matches(
    date = rep("20050101", 6),
    opponent_dob_or_age = c("20.5", "abc", "-5", "150", "0", "31")
  )
  career <- process_career_data(raw, rafa_dob, "Rafael Nadal")
  expect_equal(career$opponent_age, c(20.5, 31))
  expect_equal(clean_age(c(25, NA, Inf, 5, 90)), c(25, NA, NA, NA, NA))
})

test_that("malformed female opponent birth dates are dropped", {
  raw <- make_raw_matches(
    date = rep("19970804", 3),
    opponent_dob_or_age = c("19790301", "19791399", "19790301")
  )
  raw$score[3] <- "W/O"
  career <- process_career_data(raw, serena_dob, "Serena Williams")
  expect_equal(nrow(career), 1)
})

test_that("walkovers and defaults are dropped but retirements are kept", {
  raw <- make_raw_matches(date = rep("20050101", 4),
                          score = c("W/O", "6-1 DEF", "6-3 2-1 RET", "6-0"),
                          opponent_dob_or_age = rep("20", 4))
  career <- process_career_data(raw, rafa_dob, "Rafael Nadal")
  expect_equal(career$score, c("6-3 2-1 RET", "6-0"))
})

test_that("unexpected input data frames give meaningful errors", {
  expect_error(process_career_data(list(a = 1), rafa_dob, "X"),
               "must be a data frame with columns")
  expect_error(process_career_data(data.frame(date = "20010101"),
                                   rafa_dob, "X"),
               "must be a data frame with columns")
  expect_error(process_career_data(make_raw_matches()[0, ], rafa_dob, "X"),
               "no rows")
})

test_that("an error is raised when no usable matches remain", {
  raw <- make_raw_matches(score = "W/O")
  expect_error(process_career_data(raw, rafa_dob, "X"), "No usable matches")
})
