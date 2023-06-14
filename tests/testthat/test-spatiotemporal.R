#### Tests for spatiotemporal module ####

test_that("Neighborhoods errors and warnings are thrown",{
  # error on parameters
  expect_error(neighborhoods(c("5001", 5148, 5206, 5266, 5088, 5440, 5615)))
  expect_warning(neighborhoods(c(500010, 5148, 5206, 5266, 5088, 5440, 5615)))
})

test_that("Neighborhoods are built as expected",{
  # class
  expect_s3_class(neighborhoods(c(5001, 5148, 5206, 5266, 5088, 5440, 5615)), 
                  "nb")
  expect_length(neighborhoods(c(5001, 5148, 5206, 5266, 5088, 5440, 5615)), 
                  7)
  expect_length(neighborhoods(c(5001, 500148, 15206, 5266, 5088, 5440, 5615)), 
                5)
})

##  Incidence rate objects for morans Index

# Functional incidence rate
set.seed(3)
sample_groups <- c(5001, 5148, 5615, 5088, 5266, 5440, 5318, 5368, 5659)
sample_data <- sample(sample_groups, 200, replace = TRUE)

sample_df <- data.frame(GROUP = sample_data)
sample_df$CASES <- "2010-03-01"

incidence_object <- incidence::incidence(sample_df$CASES, 
                                           interval = "month", 
                                           group = sample_df$GROUP)
incidence_rate <- incidence_rate(incidence_object, 2)

# Failing incidence rate
sample_data_2 <- as.integer(sample(1:50, 200, replace = TRUE))
sample_dates_2 <- as.Date("2018-12-31") + sample_data_2
sample_groups_2 <- sample(c(5001, 5264, 5615, 5607), 200, replace = TRUE)
sample_df_2 <- data.frame(CASES = sample_dates_2, GROUP = sample_groups_2)
incidence_object_2 <- incidence::incidence(sample_df_2$CASES, 
                                           interval = "weeks", 
                                           group = sample_df_2$GROUP)
incidence_rate_2 <- incidence_rate(incidence_object_2, 2)

# Extra incidence rate
sample_df_3 <- sample_df[!sample_df$GROUP %in% c(5001, 5148),]
incidence_object_3 <- incidence::incidence(sample_df_3$CASES, 
                                           interval = "weeks", 
                                           group = sample_df_3$GROUP)
incidence_rate_3 <- incidence_rate(incidence_object_3, 2)

test_that("Morans Index errors and warnings are thrown", {
  expect_error(morans_index(incidence_object, 2, FALSE))
  expect_error(morans_index(incidence_rate_2, 2, FALSE))
  expect_warning(morans_index(incidence_rate, 12, TRUE), type = "list")
})

test_that("Morans Index works as expected", {
  expect_type(morans_index(incidence_rate, 1, FALSE), type = "list")
  expect_length(morans_index(incidence_rate, 1, FALSE), n = 5)
  expect_type(morans_index(incidence_rate, 2, TRUE), type = "list")
  expect_type(morans_index(incidence_rate, 12, TRUE), type = "list")
})
