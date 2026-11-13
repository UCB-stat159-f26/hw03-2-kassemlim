career_fixture <- function() {
  raw <- parse_career_js(read_fixture("MalePlayerCareer.js"))
  process_career_data(raw, as.Date("1986-06-03"), "Rafael Nadal")
}

test_that("the plot function accepts processed data and returns a ggplot", {
  plot <- plot_player_career(career_fixture())
  expect_true(inherits(plot, "ggplot"))
  expect_no_error(ggplot2::ggplot_build(plot))
})

test_that("x is the player's age and y is the age difference", {
  career <- career_fixture()
  points <- ggplot2::layer_data(plot_player_career(career))
  expect_equal(points$x, career$player_age)
  expect_equal(points$y, career$age_diff)
})

test_that("wins are blue and losses are purple", {
  career <- career_fixture()
  points <- ggplot2::layer_data(plot_player_career(career))
  expected <- ifelse(career$result == "Win", "blue", "purple")
  expect_equal(points$colour, expected)
})

test_that("the plot has a scatter layer, labels, and no annotations", {
  plot <- plot_player_career(career_fixture())
  expect_length(plot$layers, 1)
  expect_true(inherits(plot$layers[[1]]$geom, "GeomPoint"))
  expect_equal(plot$labels$title, "Rafael Nadal: Career Matches")
  expect_equal(plot$labels$x, "Player's age (years)")
  expect_equal(plot$labels$y, "Age difference (player - opponent, years)")
})

test_that("character Win/Loss results and missing player names are fine", {
  career <- career_fixture()
  career$result <- as.character(career$result)
  career$player <- NULL
  plot <- plot_player_career(career)
  expect_equal(plot$labels$title, "Career Matches")
  expect_equal(ggplot2::layer_data(plot)$colour,
               ifelse(career$result == "Win", "blue", "purple"))
})

test_that("unexpected input data frames give meaningful errors", {
  career <- career_fixture()
  expect_error(plot_player_career("Serena"), "must be a data frame")
  expect_error(plot_player_career(career[, c("player", "result")]),
               "missing column\\(s\\): player_age, age_diff")
  expect_error(plot_player_career(career[0, ]), "no rows")
  bad_age <- transform(career, player_age = as.character(player_age))
  expect_error(plot_player_career(bad_age), "must be numeric")
  bad_result <- transform(career, result = "W")
  expect_error(plot_player_career(bad_result), "only \"Win\" and \"Loss\"")
})
