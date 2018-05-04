library(testthat)

context('Default Flow Schedule')

test_that('Check N-D year type default flow schedule'){
  nd <- read.csv('data-raw/n-d.csv', stringsAsFactors = FALSE, colClasses = c('character', rep('numeric', 10)))
  unimpaired_inflow <- 1270
  default_flow_schedule <- get_default_flow_schedule(unimpaired_inflow)
  expect_equal(nd, default_flow_schedule, tolerance = 0.001)
}


