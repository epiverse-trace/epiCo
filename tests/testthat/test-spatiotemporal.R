#### Tests for spatiotemporal module ####

test_that("Neighborhoods errors and warnings are thrown", {
  # error on parameters
  expect_error(neighborhoods(c(5001, 5148, 5206, 5266, 5088, 5440, 5615)))
  expect_warning(neighborhoods(c(
    "0500010", "05148", "05206", "05266", "05088",
    "05440", "05615"
  )))
})

test_that("Neighborhoods are built as expected", {
  # class
  expect_s3_class(
    neighborhoods(c(
      "05001", "05148", "05206", "05266", "05088", "05440",
      "05615"
    )),
    "nb"
  )
  expect_length(
    neighborhoods(c(
      "05001", "05148", "05206", "05266", "05088", "05440",
      "05615"
    ))$neighbours,
    7
  )
  expect_length(
    neighborhoods(c(
      "05001", "0500148", "15206", "05266", "05088", "05440",
      "05615"
    ))$neighbours,
    5
  )
})

##  Incidence objects for morans Index

# Functional incidence object
set.seed(3)
sample_groups <- c(
  "05001", "05148", "05615", "05088", "05266", "05440",
  "05318", "05368", "05659", "91001"
)
sample_data <- sample(sample_groups, 200, replace = TRUE)

sample_df <- data.frame(GROUP = sample_data)
sample_df$CASES <- "2012-03-01"

incidence_object <- incidence::incidence(sample_df$CASES,
  interval = "month",
  group = sample_df$GROUP
)

# Failing incidence object
sample_data_2 <- as.integer(sample(1:50, 200, replace = TRUE))
sample_dates_2 <- as.Date("2018-12-31") + sample_data_2
sample_groups_2 <- sample(c("05001", "05264", "05615", "05607"), 200,
  replace = TRUE
)
sample_df_2 <- data.frame(CASES = sample_dates_2, GROUP = sample_groups_2)
incidence_object_2 <- incidence::incidence(sample_df_2$CASES,
  interval = "weeks",
  group = sample_df_2$GROUP
)

test_that("Morans Index errors and warnings are thrown", {
  expect_error(morans_index(c(20, 53, 90, 63), 2, plot = FALSE))
  expect_error(morans_index(incidence_object_2,
    threshold = 2, plot = FALSE
  ))
  expect_error(morans_index(incidence_object,
    threshold = "2", plot = FALSE
  ))
  expect_error(morans_index(incidence_object,
    threshold = 2, plot = "FALSE"
  ))
  expect_message(morans_index(incidence_object,
    threshold = 12, plot = TRUE
  ), type = "list")
})

test_that("Morans Index works as expected", {
  expect_type(morans_index(incidence_object,
    threshold = 1, plot = FALSE
  ), type = "list")
  expect_length(morans_index(incidence_object,
    threshold = 1, plot = FALSE
  ), n = 2)
  expect_type(morans_index(incidence_object,
    threshold = 2, plot = TRUE
  ), type = "list")
})
