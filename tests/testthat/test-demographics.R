#### Tests for demographics module ####


test_that("Population pyramid errors are thrown", {
  # parameters
  expect_error(population_pyramid(
    divipola_code = 5001,
    year = 2010
  ))
  expect_error(population_pyramid(
    divipola_code = "05001",
    year = 2002
  ))
  expect_error(population_pyramid(
    divipola_code = "500012",
    year = 2020
  ))
  expect_error(population_pyramid(
    divipola_code = c("05001", "05044"),
    year = 2020
  ))
  expect_error(population_pyramid(
    divipola_code = "05001",
    year = 2020,
    range = "5"
  ))
  expect_error(population_pyramid(
    divipola_code = "-05001",
    year = 2020,
    range = "5"
  ))
  expect_error(population_pyramid(
    divipola_code = "50",
    year = 2020,
    range = "5"
  ))
})

test_that("Population pyramid obtaines data", {
  # dimension
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020
    )),
    c(36L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020,
      sex = FALSE
    )),
    c(18L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020,
      total = FALSE
    )),
    c(36L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020,
      sex = FALSE,
      total = FALSE
    )),
    c(18L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020,
      sex = FALSE,
      total = FALSE,
      plot = TRUE
    )$data),
    c(18L, 2L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05",
      year = 2020
    )),
    c(42L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "0",
      year = 2020
    )),
    c(42L, 3L)
  )
  expect_identical(
    dim(population_pyramid(
      divipola_code = "05001",
      year = 2020,
      range = 20
    )),
    c(10L, 3L)
  )
})

test_that("Population pyramid is not NA", {
  # dimension
  expect_false(anyNA(population_pyramid(
    divipola_code = "05001",
    year = 2012
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "05001",
    year = 2012,
    total = FALSE,
    sex = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "05001",
    year = 2012,
    plot = TRUE
  )$data))

  expect_false(anyNA(population_pyramid(
    divipola_code = "73",
    year = 2012
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "73",
    year = 2012,
    total = FALSE,
    sex = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "73",
    year = 2012,
    plot = TRUE
  )$data))

  expect_false(anyNA(population_pyramid(
    divipola_code = "0",
    year = 2012
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "0",
    year = 2012,
    total = FALSE,
    sex = FALSE
  )))
  expect_false(anyNA(population_pyramid(
    divipola_code = "0",
    year = 2012,
    plot = TRUE
  )$data))
  expect_false(anyNA(population_pyramid(
    divipola_code = "0",
    year = 2012,
    range = 10
  )))
})

# data for age_risk tests
age_0 <- c(1, 1, 20, 4, 5, 7, 3, 3, 4, 4, 6, 6, 6, 50, 47, 47, 3, 20, 23, 23)
age_1 <- c(1, 1, 20, 4, 5, 7, 3, 3, 4, 4, 6, 6, 6, 50, 47, "47", 3, 35, 35, 3)

sex_0 <- c(
  "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "M", "M",
  "M", "M", "M", "M", "M", "M", "M", "M"
)

pop_pyramid_0 <- population_pyramid("05001", 2020, FALSE)
pop_pyramid_1 <- population_pyramid("05001", 2020, TRUE, plot = TRUE)

test_that("age risk errors are thrown", {
  expect_error(age_risk(age_1, pop_pyramid_0, sex_0))
  expect_error(age_risk(age_0, pop_pyramid_0, sex_0))
})

test_that("age risk works as expected", {
  expect_length(age_risk(age_0, pop_pyramid_1, sex_0), 3)
  expect_length(age_risk(age_0, population_pyramid = pop_pyramid_1), 2)
  expect_type(age_risk(age_0, pop_pyramid_1, sex_0, TRUE), "list")
  expect_type(
    age_risk(age_0, population_pyramid = pop_pyramid_1, plot = TRUE),
    "list"
  )
})


test_that("describe ethnicity errors are thrown", {
  expect_error(describe_ethnicity(c("a", 4)))
  expect_error(describe_ethnicity(c(1, 2, 4), language = "GE"))
})

test_that("describe etnicity works as expected", {
  expect_type(describe_ethnicity(c(1, 2, 3), "EN"), type = "list")
  expect_type(describe_ethnicity(c(1, 2, 3), "ES"), type = "list")
})

test_that("describe occupation errors are thrown", {
  expect_error(describe_occupation(c("a", 4)))
  expect_error(describe_occupation(4148))
  expect_error(describe_occupation(c(41487, 39), 3))
})

test_that("describe occupation works as expected", {
  expect_type(describe_occupation(c(1110, 4141, 12345)),
    type = "list"
  )
  expect_type(describe_occupation(c(1110, 4141), sex = c("F", "M")),
    type = "list"
  )
  expect_type(
    describe_occupation(c(1110, 4141),
      sex = c("F", "M"),
      plot = "treemap"
    ),
    type = "list"
  )
  expect_type(
    describe_occupation(c(1110, 4141),
      sex = c("F", "M"),
      plot = "circular"
    ),
    type = "list"
  )
  expect_type(
    describe_occupation(c(1110, 4141),
      sex = NULL,
      plot = "circular"
    ),
    type = "list"
  )
  expect_type(describe_occupation(c(1110, 4141), plot = NULL),
    type = "list"
  )

  expect_length(describe_occupation(c(1110, 4141), plot = "treemap"), n = 2)
  expect_identical(
    dim(describe_occupation(c(1110, 4141), plot = NULL)),
    c(3L, 9L)
  )
  expect_length(describe_occupation(c(1110, 4141),
    sex = c("F", "M"),
    plot = "circular"
  ), n = 2)
})

test_that("occupation plot errors are thrown", {
  expect_error(occupation_plot(isco_codes = "Worker"))
  expect_error(occupation_plot(isco_codes = 999999))
  expect_error(occupation_plot(
    isco_codes = c(1130, 6114, 9311),
    sex = c("F", "F")
  ))
})
