## Data import

historic_data <- readRDS("data/incidence_historic_ibague_endemic.rds")

test_that("Endemic channel throws expected erors", {
  expect_error(endemic_channel(incidence_historic = c(1,2,3,4)))
  expect_error(endemic_channel(incidence_historic = historic_data,
                               observations = c(0,0,-1)))
  expect_error(endemic_channel(incidence_historic = historic_data,
                               observations = seq(1,54)))
  # Add expected error for wrong intervals
  
})
