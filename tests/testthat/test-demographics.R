#### Tests for demographics module ####


test_that("Population pyramid errors are thrown",{
  # parameters
  expect_error(population_pyramid(divipola_code = "5001", 
                                  year = 2010))
  expect_error(population_pyramid(divipola_code = 5001, 
                                  year = 2002))
  expect_error(population_pyramid(divipola_code = 500012, 
                                  year = 2020))
  expect_error(population_pyramid(divipola_code = c(5001, 5044), 
                                  year = 2020))
  
})

test_that("Population pyramid obtaines data",{
  # dimension
  expect_equal(dim(population_pyramid(divipola_code = 5001, 
                                      year = 2020)), 
               c(202, 3))
  expect_equal(dim(population_pyramid(divipola_code = 5001, 
                                      year = 2020, 
                                      gender = FALSE)), 
               c(101, 2))
  expect_equal(dim(population_pyramid(divipola_code = 5001, 
                                      year = 2020, 
                                      total = FALSE)), 
               c(202, 3))
  expect_equal(dim(population_pyramid(divipola_code = 5001, 
                                      year = 2020, 
                                      gender = FALSE,
                                      total = FALSE)), 
               c(101, 2))
  expect_equal(dim(population_pyramid(divipola_code = 5001, 
                                      year = 2020, 
                                      gender = FALSE,
                                      total = FALSE, 
                                      plot = TRUE)), 
               c(101, 2))
  
  expect_equal(dim(population_pyramid(divipola_code = 5, 
                                      year = 2020)), 
               c(202,3))
  expect_equal(dim(population_pyramid(divipola_code = 0, 
                                      year = 2020)), 
               c(202,3))
})

test_that("Population pyramid is not NA",{
  # dimension
  expect_true(all(!is.na(population_pyramid(divipola_code = 5001, 
                                            year = 2006))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 5001, 
                                            year = 2006, 
                                            total = FALSE, 
                                            gender = FALSE))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 5001, 
                                            year = 2006,
                                            plot = TRUE))))
  
  expect_true(all(!is.na(population_pyramid(divipola_code = 73, 
                                            year = 2006))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 73, 
                                            year = 2006, 
                                            total = FALSE, 
                                            gender = FALSE))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 73, 
                                            year = 2006,
                                            plot = TRUE))))
  
  expect_true(all(!is.na(population_pyramid(divipola_code = 0, 
                                            year = 2006))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 0, 
                                            year = 2006, 
                                            total = FALSE, 
                                            gender = FALSE))))
  expect_true(all(!is.na(population_pyramid(divipola_code = 0, 
                                            year = 2006,
                                            plot = TRUE))))
})

##### AGE RISK
testthat("describe ethnicity errors are thrown",{
  expect_error(describe_ethnicity(c("a", 4)))
})

testthat("describe etnicity works as expected",{
  expect_type(describe_ethnicity(c(1,2,3), "EN"), type = "character")
  expect_type(describe_ethnicity(c(1,2,3), "ES"), type = "character")
})

testthat("describe occupation errors are thrown",{
  expect_error(describe_occupation(c("a", 4)))
  expect_error(describe_occupation(c(4148)))
})

testthat("describe occupation works as expected"){
  expect_type(describe_occupation(c(1111, 4141), output_level = 1), 
              type = "list")
  expect_type(describe_occupation(c(1111, 4141), output_level = 2), 
              type = "list")
  expect_type(describe_occupation(c(1111, 4141), output_level = 3), 
              type = "list")
  expect_type(describe_occupation(c(1111, 23), output_level = 1), 
              type = "list")
  expect_type(describe_occupation(c(1111, 23), output_level = 3), 
              type = "list")
  expect_type(describe_occupation(c(1111, 23), output_level = 4), 
              type = "list")
  expect_length(describe_occupation(c(1111, 4141), output_level = 2), n = 2)
  
}
