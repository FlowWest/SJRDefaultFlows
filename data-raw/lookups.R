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

R2_losses_lookup <- read_excel('data-raw/31GammaFlow_Schedule_Checkv5.xlsx',
                               sheet = 'R2 Losses.LUP', range = 'A2:B10', col_names = c('flow', 'r2_losses'))
use_data(R2_losses_lookup, overwrite = TRUE)


