library(tidyverse)
library(lubridate)
library(readxl)

excel_sheets('data-raw/31GammaFlow_Schedule_Checkv5.xlsx')

flow_default_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A1:O13', sheet = 'YearType.LUP')

flow_increase_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A1:O13', sheet = 'Transformation.LUP')

use_data(flow_default_lookup)

year_type_lookup <- data.frame(
  total_TAF = c(116.866, 187.785, 209.207, 226.760, 236.390, 267.630, 272.985, 279.332, 301.289, 365.256, 473.851, 673.488),
  year_type = c('CL',	'CH',	'CH-1',	'CH-2',	'CH-3',	'CH-4',	'CH-5',	'CH-6',	'Dry', 'N-D',	'N-W', 'Wet'),
  stringsAsFactors = FALSE)

use_data(year_type_lookup)

# from unimpaired inflow create allocation lookup
get_allocation_look_up <- function(unimpaired_inflow) {
  data.frame(
  uiTAF = c(0, 400, 670, 930, 1450, 2500),
  allocation = c(116.866115702479, 187.785123966942,
                 0.223153846 * unimpaired_inflow + 122.76692318,
                 0.1346153846 * unimpaired_inflow + 205.107692322,
                 0.140095238 * unimpaired_inflow + 197.1619047619,
                 673.487603305785))
}

# given unimpaired inflow return allocation
get_allocation <- function(unimpaired_inflow) {

  allocationLU <- get_allocation_look_up(unimpaired_inflow)
  flow_index <- which.min(abs(allocationLU$uiTAF - unimpaired_inflow))
  flow_index <- ifelse(uiTAF[flow_index] > unimpaired_inflow, flow_index - 1, flow_index)

  return(allocationLU$allocation[flow_index])

}

allocation <- get_allocation(1270)

# given allocation return year type
get_year_type <- function(allocation) {
  allocation_index <- which.min(abs(SJRDefaultFlows::year_type_lookup$total_TAF - allocation))
  allocation_index <- ifelse(SJRDefaultFlows::year_type_lookup$total_TAF[allocation_index] > allocation,
                             allocation_index - 1, allocation_index)

  return(SJRDefaultFlows::year_type_lookup$year_type[allocation_index])
}

year_type = 'N-D'

# stoped here
get_default_flows <- function(unimpaired_inflow, flow_cap = FALSE) {

  allocation <- get_allocation(unimpaired_inflow)
  year_type <- get_year_type(allocation)

  days <- SJRDefaultFlows::flow_default_lookup$`# Days`
  total_acre_feet <- sum(SJRDefaultFlows::flow_default_lookup[year_type] * (60 * 60 * 24 * days) / 43560)


}



R2losses <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx',
                       sheet = 'R2 Losses.LUP', range = 'A1:B10', col_names = c('flow', 'r2_losses'))





closestLoc = which(min(abs(w-x)))
closestVal = w[which(min(abs(w-x)))]
