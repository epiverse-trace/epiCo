#### Tests for utils module ####

test_that("Epidemiological calendar throws errors", {
  expect_error(epi_calendar("2004"))
  expect_error(epi_calendar(2004, "3"))
})

test_that("Epidemiological calendar works as expected", {
  # class
  expect_s3_class(epi_calendar(2004), "Date")

  # results
  expect_true(lubridate::is.Date(epi_calendar(2004)))
  expect_true(lubridate::is.Date(epi_calendar(2008, 2)))

  # dimensions and count
  expect_length(epi_calendar(2004), 52)
  expect_length(epi_calendar(2023), 52)
})

## data for incidence rate

# dates
set.seed(3)
sample_data <- as.integer(sample(1:50, 200, replace = TRUE))
sample_dates <- as.Date("2018-12-31") + sample_data

# groups
sample_groups_1 <- sample(c("05", "08", "11"), 200, replace = TRUE)
sample_groups_2 <- sample(c("05001", "05264", "05615", "05607"), 200,
  replace = TRUE
)

sample_df_0 <- data.frame(CASES = sample_dates)
sample_df_1 <- data.frame(CASES = sample_dates, GROUP = sample_groups_1)
sample_df_2 <- data.frame(CASES = sample_dates, GROUP = sample_groups_2)

# incidence objects
incidence_object_0 <- incidence::incidence(sample_df_0$CASES,
  interval = "weeks"
)
incidence_object_1 <- incidence::incidence(sample_df_1$CASES,
  interval = "weeks",
  group = sample_df_1$GROUP
)
incidence_object_2 <- incidence::incidence(sample_df_2$CASES,
  interval = "weeks",
  group = sample_df_2$GROUP
)

# examples for each level
test_incidence_rate_0 <- incidence_rate(incidence_object_0, 0)
test_incidence_rate_1 <- incidence_rate(incidence_object_1, 1)
test_incidence_rate_2 <- incidence_rate(incidence_object_2, 2)

test_that("Incidence rate throws errors", {
  expect_error(incidence_rate(1, 2))
  expect_error(incidence_rate(incidence_object_0, 5))
  expect_error(incidence_rate(incidence_object_0, "2"))
  expect_error(incidence_rate(incidence_object_0, 2, "100"))
})

test_that("Incidence rate construction", {
  expect_s3_class(incidence_rate(incidence_object_2, 2), "incidence")
  expect_type(test_incidence_rate_2$rates, "double")
})

test_that("Incidence rate calculate rates", {
  # results
  expect_length(test_incidence_rate_0, 9L)
  expect_length(test_incidence_rate_1, 9L)
  expect_length(test_incidence_rate_2, 9L)

  expect_null(colnames(test_incidence_rate_0$rates))
  expect_identical(
    sort(colnames(test_incidence_rate_1$rates)),
    sort(as.character(unique(sample_groups_1)))
  )
  expect_identical(
    sort(colnames(test_incidence_rate_2$rates)),
    sort(as.character(unique(sample_groups_2)))
  )
})

test_that("Geometric mean throws errors", {
  expect_error(geometric_mean(c(45, 20, 1000, "a")))
  expect_error(geometric_mean(c(45, 20, 1000, 100), method = "test"))
  expect_error(geometric_mean(c(45, 20, 1000, 100),
    method = "shifted",
    shift = "2"
  ))
  expect_error(geometric_mean(c(45, 20, 1000, 100), epsilon = "test"))
  expect_error(geometric_mean(c(45, 20, 1000, -100), method = "shifted"))
  expect_error(geometric_mean(c(45, 20, 1000, -100), epsilon = "positive"))
  expect_error(geometric_mean(c(45, 20, 1000, -100), method = "positive"))
})

test_that("Geometric mean works as expected", {
  expect_type(geometric_mean(c(45, 20, 1000, 100)), "double")
  expect_length(geometric_mean(c(45, 20, 1000, 100)), 1L)
  expect_length(geometric_mean(c(45, 20, 1000, 100), method = "optimized"), 2L)

  expect_gt(geometric_mean(c(45, 20, 1000, 100), method = "positive"), 0)
  expect_gt(geometric_mean(c(45, 20, 1000, -100), method = "weighted"), 0)
  expect_gt(geometric_mean(c(45, 20, 1000, 100), method = "shifted"), 0)
})
