library(tidyverse)
library(lubridate)
library(readxl)

excel_sheets('data-raw/31GammaFlow_Schedule_Checkv5.xlsx')

number_of_days_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A8:C20', sheet = 'Friant')
use_data(number_of_days_lookup)

flow_default_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A1:O13', sheet = 'YearType.LUP')
use_data(flow_default_lookup, overwrite = TRUE)

flow_increase_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx',
                                          range = 'A3:H14', sheet = 'Transformation.LUP',
                                          col_names = c('YearType', '__', 'ExtraFlow1', 'ExtraFlow2',
                                                        'ExtraFlow3', 'ExtraFlow4', 'ExtraFlow5', 'IncreaseAmount'))

use_data(flow_increase_lookup, overwrite = TRUE)

year_type_lookup <- data.frame(
  total_TAF = c(116.866, 187.785, 209.207, 226.760, 236.390, 267.630, 272.985, 279.332, 301.289, 365.256, 473.851, 673.488),
  year_type = c('CL',	'CH',	'CH-1',	'CH-2',	'CH-3',	'CH-4',	'CH-5',	'CH-6',	'Dry', 'N-D',	'N-W', 'Wet'),
  stringsAsFactors = FALSE)

use_data(year_type_lookup)

# from unimpaired inflow create allocation lookup
get_allocation_lookup <- function(unimpaired_inflow) {
  data.frame(
  uiTAF = c(0, 400, 670, 930, 1450, 2500),
  allocation = c(116.866115702479, 187.785123966942,
                 0.223153846 * unimpaired_inflow + 122.76692318,
                 0.1346153846 * unimpaired_inflow + 205.107692322,
                 0.140095238 * unimpaired_inflow + 197.1619047619,
                 673.487603305785))
}

# given unimpaired inflow return allocation
get_allocation <- function(unimpaired_inflow, allocation_lookup) {

  flow_index <- which.min(abs(allocation_lookup$uiTAF - unimpaired_inflow))
  flow_index <- ifelse(allocation_lookup$uiTAF[flow_index] > unimpaired_inflow,
                       flow_index - 1, flow_index)

  return(allocation_lookup$allocation[flow_index])

}

cfs_to_af <- function(cfs) {
  return(cfs * (24 * 60 * 60) / 43560)
}

af_to_cfs <- function(af) {
  return(af * 43560 / (24 * 60 * 60))
}

# given allocation return year type
get_year_type <- function(allocation) {
  allocation_index <- which.min(abs(SJRDefaultFlows::year_type_lookup$total_TAF - allocation))
  allocation_index <- ifelse(SJRDefaultFlows::year_type_lookup$total_TAF[allocation_index] > allocation,
                             allocation_index - 1, allocation_index)

  return(SJRDefaultFlows::year_type_lookup$year_type[allocation_index])
}

get_restoration_AF <- function(unimpaired_inflow, allocation) {
  return(allocation * 1000 - ifelse(unimpaired_inflow < 670, 116866, 116945))
}

get_additional_allocation <- function(allocation, year_type) {

  days <- SJRDefaultFlows::flow_default_lookup$`# Days`
  flow_schedule_cfs <- SJRDefaultFlows::flow_default_lookup[year_type]
  total_acre_feet <- sum(flow_schedule_cfs * (60 * 60 * 24 * days) / 43560)

  return(allocation * 1000 - total_acre_feet)
}

# given year type reurns number of days, replaces flow increase time of year cells K5:O5
get_number_of_days <- function(year_type) {

  year_type_index <- which(SJRDefaultFlows::flow_increase_lookup$YearType == year_type)
  flow_increase_timing_keys <- as.matrix(SJRDefaultFlows::flow_increase_lookup[year_type_index, 3:7])[,1:5]
  day_keys <- which(SJRDefaultFlows::number_of_days_lookup$Key %in% flow_increase_timing_keys)
  days <- sum(SJRDefaultFlows::number_of_days_lookup$`# Days`[day_keys])
  return(days)
}

get_default_schedule <- function(year_type, addition_allocation) {

  year_type_index <- which(SJRDefaultFlows::flow_increase_lookup$YearType == year_type)
  flow_increase_keys <- as.matrix(SJRDefaultFlows::flow_increase_lookup[year_type_index, 3:7])[,1:5]
  days <- SJRDefaultFlows::number_of_days_lookup$`# Days`
  default_flow_cfs <- SJRDefaultFlows::flow_default_lookup[[year_type]]
  default_flow_af <- cfs_to_af(default_flow_cfs) * days
  increase_amount <- SJRDefaultFlows::flow_increase_lookup[[year_type_index, 'IncreaseAmount']]
  to_change <- (increase_amount > default_flow_cfs) & SJRDefaultFlows::number_of_days_lookup$Key %in% flow_increase_keys[[1]]
  changed_af <- (cfs_to_af(increase_amount) * days - default_flow_af) * to_change

  flow_period1 <- ifelse(addition_allocation < to_change[1],
                    cfs_to_af(default_flow_cfs[1]) + af_to_cfs(addition_allocation)/days[1],
                    default_flow_cfs[1] + af_to_cfs(changed_af[1])/days[1])
  remaining_allocation_period1 <- addition_allocation - cfs_to_af((flow_period1 - default_flow_cfs[1])) * days[1]

  schedule_cfs <- c(flow_period1, numeric(11))
  remaining_allocation_af <- c(remaining_allocation_period1, numeric(11))

  for (i in 2:12) {
    remain_alloc <- remaining_allocation_af[i-1]
    schedule_cfs[i] <- ifelse(remain_alloc < changed_af[i],
           default_flow_cfs[i] + af_to_cfs(remain_alloc)/days[i],
           default_flow_cfs[i] + af_to_cfs(changed_af[i])/days[i])

    remaining_allocation_af[i] <- remain_alloc - cfs_to_af(schedule_cfs[i] - default_flow_cfs[i]) * days[i]
  }


}

# =K9-(J10-D10)*(60*60*24*C10)/43560
# =IF(K9<I10,D10+(K9*43560/(C10*24*60*60)),D10+I10*43560/(C10*24*60*60))

# Friant Sheet Test
unimpaired_inflow <- 1270 # cell D3
allocation_lookup <- get_allocation_lookup(unimpaired_inflow)
allocation <- get_allocation(unimpaired_inflow, allocation_lookup) # cell C4
year_type <- get_year_type(allocation) # cell C5
restoration_af <- get_restoration_AF(unimpaired_inflow, allocation) # cell E5
addition_allocation <- get_additional_allocation(allocation, year_type) #cell K3
number_of_days <- get_number_of_days(year_type) # cell I4





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
