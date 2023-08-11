## data for incidence rate

# dates
set.seed(3)
sample_data <- as.integer(sample(1:3285, 500, replace = TRUE))
sample_dates <- as.Date("2013-01-01") + sample_data

sample_dates <- sample_dates[lubridate::year(lubridate::as_date(sample_dates, origin = "1970-01-01")) < 2020]
sample_dates <- c(as.Date("2013-01-01"), sample_dates, as.Date("2019-12-28"))

sample_df <- data.frame(CASES = sample_dates)

# incidence objects
historic_data <- incidence::incidence(sample_df$CASES, interval = "1 epiweek")
historic_data_month <- incidence::incidence(sample_df$CASES, interval = "1 month")
historic_data_day <- incidence::incidence(sample_df$CASES, interval = "1 day")


test_that("Endemic channel throws expected erors", {
  expect_error(endemic_channel(incidence_historic = c(20, 53, 90, 63)))
  expect_error(endemic_channel(incidence_historic = historic_data,
    observations = c(0, 0, -1)))  
  expect_error(endemic_channel(incidence_historic = historic_data_day,
                               observations = seq(1,52)))
  expect_error(endemic_channel(incidence_historic = historic_data,
    observations = seq(1, 54)))
  expect_error(endemic_channel(incidence_historic = historic_data_month,
                               observations = seq(1, 14)))
  expect_error(endemic_channel(incidence_historic = historic_data_month,
                               observations = seq(1, 14), method = "poisson"))
  expect_error(endemic_channel(incidence_historic = historic_data_month,
                               observations = seq(1, 14), ci = "0.95"))
  expect_error(endemic_channel(incidence_historic = historic_data_month,
                               observations = seq(1, 14), ci = 1.2))
  expect_error(endemic_channel(incidence_historic = historic_data_month,
                               observations = seq(1, 14), plot =  "TRUE"))
  
  
})

test_that("Endemic channel works as expected", {
  expect_type(endemic_channel(incidence_historic = historic_data,
                              observations = seq(1,52)), "list")
  expect_type(endemic_channel(incidence_historic = historic_data,
                              observations = seq(1,52),
                              method = "geometric"), "list")
  expect_type(endemic_channel(incidence_historic = historic_data,
                              observations = seq(1,52),
                              method = "median"), "list")
  expect_type(endemic_channel(incidence_historic = historic_data,
                              observations = seq(1,52),
                              method = "mean"), "list")
  # expect_type(endemic_channel(incidence_historic = historic_data,
  #                             observations = seq(1,52),
  #                             method = "unusual behavior"), "list")
  expect_type(endemic_channel(incidence_historic = historic_data,
                              observations = seq(1,52),
                              plot = TRUE), "list")
  expect_equal(dim(endemic_channel(incidence_historic = historic_data,
                                observations = seq(1,52))), c(52,4))
  expect_equal(dim(endemic_channel(incidence_historic = historic_data_month,
                                   observations = seq(1,12))), c(12,4))
})

