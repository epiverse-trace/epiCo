#### Tests for demographics module ####


test_that("Population pyramid errors are thrown", {
  # parameters
  expect_error(population_pyramid(
    divipola_code = "5001",
    year = 2010
  ))
  expect_error(population_pyramid(
    divipola_code = 5001,
    year = 2002
  ))
  expect_error(population_pyramid(
    divipola_code = 500012,
    year = 2020
  ))
  expect_error(population_pyramid(
    divipola_code = c(5001, 5044),
    year = 2020
  ))
  expect_error(population_pyramid(
    divipola_code = 5001,
    year = 2020,
    range = "5"
  ))
})

test_that("Population pyramid obtaines data", {
  # dimension
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020
    )),
    c(34L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020,
      gender = FALSE
    )),
    c(17L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020,
      total = FALSE
    )),
    c(34L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020,
      gender = FALSE,
      total = FALSE
    )),
    c(17L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020,
      gender = FALSE,
      total = FALSE,
      plot = TRUE
    )),
    c(17L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5,
      year = 2020
    )),
    c(34L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 0,
      year = 2020
    )),
    c(34L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = 5001,
      year = 2020,
      range = 20
    )),
    c(8L, 3L)
  )
})

test_that("Population pyramid is not NA", {
  # dimension
  expect_false(anyNA(population_pyramid(
    divipola_code = 5001,
    year = 2006
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 5001,
    year = 2006,
    total = FALSE,
    gender = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 5001,
    year = 2006,
    plot = TRUE
  )))

  expect_false(anyNA(population_pyramid(
    divipola_code = 73,
    year = 2006
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 73,
    year = 2006,
    total = FALSE,
    gender = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 73,
    year = 2006,
    plot = TRUE
  )))

  expect_false(anyNA(population_pyramid(
    divipola_code = 0,
    year = 2006
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 0,
    year = 2006,
    total = FALSE,
    gender = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 0,
    year = 2006,
    plot = TRUE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = 0,
    year = 2006,
    range = 10
  )))
})

# data for age_risk tests
age_0 <- c(1, 1, 20, 4, 5, 7, 3, 3, 4, 4, 6, 6, 6, 50, 47, 47, 3, 20, 23, 23)
age_1 <- c(1, 1, 20, 4, 5, 7, 3, 3, 4, 4, 6, 6, 6, 50, 47, "47", 3, 35, 35, 3)

gender_0 <- c(
  "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "M", "M",
  "M", "M", "M", "M", "M", "M", "M", "M"
)

pop_pyramid_0 <- population_pyramid(5001, 2020, FALSE)
pop_pyramid_1 <- population_pyramid(5001, 2020, TRUE)

test_that("age risk errors are thrown", {
  expect_error(age_risk(age_1, gender_0, pop_pyramid_0))
  expect_error(age_risk(age_0, gender_0, pop_pyramid_0))
})

test_that("age risk works as expected", {
  expect_length(age_risk(age_0, gender_0, pop_pyramid_1), 3)
  expect_length(age_risk(age_0, population_pyramid = pop_pyramid_1), 2)
  expect_type(age_risk(age_0, gender_0, pop_pyramid_1, TRUE), "list")
  expect_type(
    age_risk(age_0, population_pyramid = pop_pyramid_1, plot = TRUE),
    "list"
  )
})


test_that("describe ethnicity errors are thrown", {
  expect_error(describe_ethnicity(c("a", 4)))
})

test_that("describe etnicity works as expected", {
  expect_type(describe_ethnicity(c(1, 2, 3), "EN"), type = "character")
  expect_type(describe_ethnicity(c(1, 2, 3), "ES"), type = "character")
})

test_that("describe occupation errors are thrown", {
  expect_error(describe_occupation(c("a", 4)))
  expect_error(describe_occupation(4148))
  expect_error(describe_occupation(c(41487, 39), 3))
})

test_that("describe occupation works as expected", {
  expect_type(describe_occupation(c(1111, 4141), output_level = 1),
    type = "list"
  )
  expect_type(describe_occupation(c(1111, 4141), output_level = 2),
    type = "list"
  )
  expect_type(describe_occupation(c(1111, 4141), output_level = 3),
    type = "list"
  )
  expect_type(describe_occupation(c(1111, 23), output_level = 1),
    type = "list"
  )
  expect_type(describe_occupation(c(1111, 23), output_level = 3),
    type = "list"
  )
  expect_type(describe_occupation(c(1111, 23), output_level = 4),
    type = "list"
  )

  expect_length(describe_occupation(c(1111, 4141), output_level = 2), n = 2)
  expect_length(describe_occupation(110, output_level = 2), n = 1)
  expect_length(describe_occupation(111, output_level = 4), n = 1)
  expect_length(describe_occupation(c(23, 11), output_level = 4), n = 2)
})

test_that("occupation plot errors are thrown", {
  expect_error(occupation_plot(isco_codes = "Worker"))
  expect_error(occupation_plot(isco_codes = 999999))
  expect_error(occupation_plot(
    isco_codes = c(1130, 6114, 9311),
    gender = c("F", "F")
  ))
})
