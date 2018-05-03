library(tidyverse)
library(lubridate)
library(readxl)

excel_sheets('data-raw/31GammaFlow_Schedule_Checkv5.xlsx')

number_of_days_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A8:C20', sheet = 'Friant')
use_data(number_of_days_lookup)

friant_exhibitB_flow_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A1:O13', sheet = 'YearType.LUP')
use_data(friant_exhibitB_flow_lookup, overwrite = TRUE)

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


exhibitB_diversions_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx', range = 'A2:J14', sheet = 'ExhibitB.LUP') %>%
  select(period = X__1, divers_R1 = `Diversions R1`, divers_R1_crit_yrs = `Divsns. R1 Critical Yrs`,
         mud_ss_gains = `Mud & SS Gains`, mud_ss_gains_crit_yrs = `M&SS Gains - Crit Yr`)

use_data(exhibitB_diversions_lookup, overwrite = TRUE)

get_gravelly_ford_flows <- function(year_type) {

  friant_flows_exB <- SJRDefaultFlows::friant_exhibitB_flow_lookup[[year_type]]

  if(year_type %in% c('Wet', 'N-W', 'N-D', 'Dry')) {
    return(friant_flows_exB - SJRDefaultFlows::exhibitB_diversions_lookup$divers_R1)
  } else {
    return(friant_flows_exB - SJRDefaultFlows::exhibitB_diversions_lookup$divers_R1_crit_yrs)
  }
}

get_R2_losses <- function(gravelly_ford_flows) {
  get_R2_loss <- function(gravelly_ford_flow){
    loss_index <- which.min(abs(SJRDefaultFlows::R2_losses_lookup$flow - gravelly_ford_flow))
    if(SJRDefaultFlows::R2_losses_lookup$flow[loss_index] > gravelly_ford_flow) {loss_index = loss_index - 1}
    return(SJRDefaultFlows::R2_losses_lookup$r2_losses[loss_index])
  }
  return(sapply(gravelly_ford_flows, get_R2_loss))
}

get_mendota_dam_flows <- function(gravelly_ford_flows, gravelly_ford_losses) {
  return(gravelly_ford_flows - gravelly_ford_losses)
}

gravelly_ford_flows <- get_gravelly_ford_flows(year_type)
gravelly_ford_losses <- get_R2_losses(gravelly_ford_flows)
get_mendota_dam_flows(gravelly_ford_flows, gravelly_ford_losses)

R2_losses_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx',
                       sheet = 'R2 Losses.LUP', range = 'A2:B10', col_names = c('flow', 'r2_losses'))
use_data(R2_losses_lookup, overwrite = TRUE)
