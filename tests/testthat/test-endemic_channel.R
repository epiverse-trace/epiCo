## data for incidence rate

# dates
set.seed(3)
sample_data <- as.integer(sample(1:3285, 500, replace = TRUE))
sample_dates <- as.Date("2013-01-01") + sample_data

sample_dates <- sample_dates[lubridate::year(lubridate::as_date(sample_dates,
  origin = "1970-01-01"
)) < 2020]
sample_dates <- c(as.Date("2013-01-01"), sample_dates, as.Date("2019-12-28"))

sample_df <- data.frame(CASES = sample_dates)

# incidence objects
historic_data <- incidence::incidence(sample_df$CASES, interval = "1 epiweek")
historic_data_d <- incidence::incidence(sample_df$CASES, interval = "1 day")
historic_data_short <- historic_data[historic_data$dates <= "2013-05-26", ]
historic_data_w_short <- historic_data[historic_data$dates <= "2017-05-01", ]
historic_data_w_short <- historic_data_w_short[historic_data_w_short$dates >=
  "2013-05-01", ]

test_that("Endemic channel throws expected erors", {
  expect_error(endemic_channel(incidence_historic = c(20, 53, 90, 63)))
  expect_error(endemic_channel(incidence_historic = historic_data_short))
  expect_error(endemic_channel(
    incidence_historic = historic_data,
    observations = c(0, 0, -1)
  ))
  expect_error(endemic_channel(
    incidence_historic = historic_data_d,
    observations = seq(1, 52)
  ))
  expect_error(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 54)
  ))
})

test_that("Endemic channel works as expected", {
  expect_type(endemic_channel(
    incidence_historic = historic_data_w_short
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52)
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    plot = TRUE
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "median"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "mean"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "unusual_behavior"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "ignored"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "included"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "replaced_by_median"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "replaced_by_mean"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "replaced_by_geometric_mean"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    method = "geometric",
    outliers_handling = "replaced_by_geometric_mean",
    geometric_method = "optimized"
  ), "list")
  expect_type(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52),
    plot = TRUE
  ), "list")
  expect_identical(dim(endemic_channel(
    incidence_historic = historic_data,
    observations = seq(1, 52)
  )), c(52L, 4L))
})
