library(testthat)
library(dplyr)

context('Default Flow Schedule')

test_that('Check CL year type default flow schedule', {
  ch <- read.csv('cl.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 0
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(ch, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check CH year type default flow schedule', {
  ch <- read.csv('ch.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 400
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(ch, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check CH-4 year type default flow schedule', {
  ch4 <- read.csv('ch4.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 670
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(ch4, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check CH-5 year type default flow schedule', {
  ch5 <- read.csv('ch5.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 700
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(ch5, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check CH-6 year type default flow schedule', {
  ch6 <- read.csv('ch6.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 750
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(ch6, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check CL year type default flow schedule', {
  dry <- read.csv('dry.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 930
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(dry, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check N-D year type default flow schedule', {
  nd <- read.csv('n-d.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 1270
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(nd, default_flow_schedule[, -12], tolerance = 0.05)
})

test_that('Check N-W year type default flow schedule', {
  nw <- read.csv('n-w.csv', stringsAsFactors = FALSE)
  unimpaired_inflow <- 2501
  allocation <- get_allocation(unimpaired_inflow)
  default_flow_schedule <- get_default_flow_schedule(allocation)
  expect_equal(nw, default_flow_schedule[, -12], tolerance = 0.05)
})

# (unimpaired_inflow <- 3670)
# (allocation_lookup <- get_allocation_lookup(unimpaired_inflow))
# (allocation <- get_allocation(unimpaired_inflow, allocation_lookup))
# (year_type <- get_year_type(allocation))
# (restoration_af <- get_restoration_AF(unimpaired_inflow, allocation))
# (addition_allocation <- get_additional_allocation(allocation, year_type))
# (number_of_days <- get_number_of_days(year_type))
# (diversions <- get_diversions(year_type))
# (friant_exhibitB <- get_friant_flows_exhibitB(year_type))
# (gravelB <- get_gravelly_ford_flows(year_type, friant_exhibitB, diversions))
# (gravelB_losses <- get_R2_losses(gravelB, TRUE))
# (mendota_B <- get_mendota_dam_flows(gravelB, gravelB_losses))
# (confluence_B <- get_confluence_flows(year_type, mendota_B))
#
#
# (friant_flows <- get_friant_default_schedule(year_type, addition_allocation))
# (gravelly_ford_flows <- get_gravelly_ford_flows(year_type, friant_flows, diversions))
# (gravelly_ford_losses <- get_R2_losses(gravelly_ford_flows))
# (mendota_dam_flows <- get_mendota_dam_flows(gravelly_ford_flows, gravelly_ford_losses))
# (confluence_flows <- get_confluence_flows(year_type, mendota_dam_flows))
# default_flow_schedule <- get_default_flow_schedule(unimpaired_inflow)
# daily_default_flow_schedule <- get_daily_default_flow_schedule(default_flow_schedule, year = 2000)
