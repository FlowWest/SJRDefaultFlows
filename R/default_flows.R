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
get_allocation <- function(unimpaired_inflow) {

  allocation_lookup <- get_allocation_lookup(unimpaired_inflow)
  flow_index <- which.min(abs(allocation_lookup$uiTAF - unimpaired_inflow))
  flow_index <- ifelse(allocation_lookup$uiTAF[flow_index] > unimpaired_inflow,
                       flow_index - 1, flow_index)

  return(allocation_lookup$allocation[flow_index])

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

  days <- SJRDefaultFlows::friant_exhibitB_flow_lookup$`# Days`
  flow_schedule_cfs <- SJRDefaultFlows::friant_exhibitB_flow_lookup[year_type]
  total_acre_feet <- sum(flow_schedule_cfs * (60 * 60 * 24 * days) / 43560)

  return(round(allocation * 1000 - total_acre_feet))
}

# given year type reurns number of days, replaces flow increase time of year cells K5:O5
get_number_of_days <- function(year_type) {

  year_type_index <- which(SJRDefaultFlows::flow_increase_lookup$YearType == year_type)
  flow_increase_timing_keys <- as.matrix(SJRDefaultFlows::flow_increase_lookup[year_type_index, 3:7])[,1:5]
  day_keys <- which(SJRDefaultFlows::number_of_days_lookup$Key %in% flow_increase_timing_keys)
  days <- sum(SJRDefaultFlows::number_of_days_lookup$`# Days`[day_keys])
  return(days)
}

get_friant_flows_exhibitB <- function(year_type) {
  exhibitB_flows <- SJRDefaultFlows::friant_exhibitB_flow_lookup[[year_type]]
  return(exhibitB_flows)
}

get_friant_default_schedule <- function(year_type, addition_allocation, capped = FALSE, flow_cap = NULL) {

  year_type_index <- which(SJRDefaultFlows::flow_increase_lookup$YearType == year_type)
  flow_increase_keys <- as.matrix(SJRDefaultFlows::flow_increase_lookup[year_type_index, 3:7])[,1:5]
  if (all(is.na(flow_increase_keys))) {flow_increase_keys <- 0}
  days <- SJRDefaultFlows::number_of_days_lookup$`# Days`
  exhibitB_cfs <- get_friant_flows_exhibitB(year_type)
  exhibitB_af <- cfs_to_af(exhibitB_cfs) * days
  increase_amount <- SJRDefaultFlows::flow_increase_lookup[[year_type_index, 'IncreaseAmount']]
  increase_amount <- ifelse(is.na(increase_amount), 0, increase_amount)

  to_change <- (increase_amount > exhibitB_cfs) & SJRDefaultFlows::number_of_days_lookup$Key %in% flow_increase_keys
  changed_af <- (cfs_to_af(increase_amount) * days - exhibitB_af) * to_change

  flow_period1 <- ifelse(addition_allocation < to_change[1],
                         cfs_to_af(exhibitB_cfs[1]) + af_to_cfs(addition_allocation)/days[1],
                         exhibitB_cfs[1] + af_to_cfs(changed_af[1])/days[1])
  remaining_allocation_period1 <- addition_allocation - cfs_to_af((flow_period1 - exhibitB_cfs[1])) * days[1]

  schedule_cfs <- c(flow_period1, numeric(11))
  remaining_allocation_af <- c(remaining_allocation_period1, numeric(11))

  for (i in 2:12) {
    remain_alloc <- remaining_allocation_af[i-1]
    schedule_cfs[i] <- ifelse(remain_alloc < changed_af[i],
                              exhibitB_cfs[i] + af_to_cfs(remain_alloc)/days[i],
                              exhibitB_cfs[i] + af_to_cfs(changed_af[i])/days[i])

    remaining_allocation_af[i] <- remain_alloc - cfs_to_af(schedule_cfs[i] - exhibitB_cfs[i]) * days[i]
  }

  if (capped) {
    schedule_cfs <- ifelse(schedule_cfs > flow_cap, flow_cap, schedule_cfs)
  }

  return(schedule_cfs)

}

get_diversions <- function(year_type) {
  if(year_type %in% c('Wet', 'N-W', 'N-D', 'Dry')) {
    return(SJRDefaultFlows::exhibitB_diversions_lookup$divers_R1)
  } else {
    return(SJRDefaultFlows::exhibitB_diversions_lookup$divers_R1_crit_yrs)
  }
}

get_gravelly_ford_flows <- function(year_type, friant_flows, diversions) {

  return(friant_flows - diversions)

}

get_R2_losses <- function(gravelly_ford_flows, exhibitB = FALSE) {
  get_R2_loss <- function(gravelly_ford_flow, exB = exhibitB){

    # find loss index
    loss_index <- which.min(abs(SJRDefaultFlows::R2_losses_lookup$flow - gravelly_ford_flow))
    if(loss_index > 1 & SJRDefaultFlows::R2_losses_lookup$flow[loss_index] > gravelly_ford_flow) {loss_index = loss_index - 1}

    if (exB) {
      return(SJRDefaultFlows::R2_losses_lookup$r2_losses[loss_index])
    }

    if (gravelly_ford_flow < 1000) {
      if (SJRDefaultFlows::R2_losses_lookup$r2_losses[loss_index] < gravelly_ford_flow) {
        return(SJRDefaultFlows::R2_losses_lookup$r2_losses[loss_index])
      } else {
        return(gravelly_ford_flow)
      }
    } else {
      return(47.932 * log(gravelly_ford_flow) - 195.9)
    }

}

  return(sapply(gravelly_ford_flows, get_R2_loss))

}

get_mendota_dam_flows <- function(gravelly_ford_flows, R2_losses) {
  return(gravelly_ford_flows - R2_losses)
}

# TODO is this correct?
get_sack_dam_flows <- function(mendota_dam_flows) {
  return((mendota_dam_flows - 10) * .95)
}

get_confluence_flows <- function(year_type, mendota_dam_flows) {
  if(year_type %in% c('Wet', 'N-W', 'N-D', 'Dry')) {
    return(mendota_dam_flows + SJRDefaultFlows::exhibitB_diversions_lookup$mud_ss_gains)
  } else {
    return(mendota_dam_flows + SJRDefaultFlows::exhibitB_diversions_lookup$mud_ss_gains_crit_yrs)
  }
}


get_default_flow_schedule <- function(allocation) {

  year_type <- get_year_type(allocation) # cell C5
  # restoration_af <- get_restoration_AF(unimpaired_inflow, allocation) # cell E5
  addition_allocation <- get_additional_allocation(allocation, year_type) #cell K3
  number_of_days <- get_number_of_days(year_type) # cell I4
  friant_flows <- get_friant_default_schedule(year_type, addition_allocation)
  diversions <- get_diversions(year_type)
  gravelly_ford_flows <- get_gravelly_ford_flows(year_type, friant_flows, diversions)
  gravelly_ford_losses <- get_R2_losses(gravelly_ford_flows)
  mendota_dam_flows <- get_mendota_dam_flows(gravelly_ford_flows, gravelly_ford_losses)
  confluence_flows <- get_confluence_flows(year_type, mendota_dam_flows)
  sack_dam_flows <- get_sack_dam_flows(mendota_dam_flows)

  #exhibit B
  friant_exhibitB <- get_friant_flows_exhibitB(year_type)
  gravelly_ford_exhibitB <- get_gravelly_ford_flows(year_type, friant_exhibitB, diversions)
  R2_losses_exhibitB <- get_R2_losses(gravelly_ford_exhibitB, exhibitB = TRUE)
  mendota_dam_exhibitB <- get_mendota_dam_flows(gravelly_ford_exhibitB, R2_losses_exhibitB)
  confluence_exhibitB <- get_confluence_flows(year_type, mendota_dam_exhibitB)

  flows <- data.frame(
    period = SJRDefaultFlows::number_of_days_lookup$Period,
    days = SJRDefaultFlows::number_of_days_lookup$`# Days`,
    friant_exhibitB = friant_exhibitB,
    gravelly_ford_exhibitB = gravelly_ford_exhibitB,
    mendota_dam_exhibitB = mendota_dam_exhibitB,
    confluence_exhibitB = confluence_exhibitB,
    friant_release = friant_flows,
    gravelly_ford_target = gravelly_ford_flows,
    SJRRP_flows_at_gravelly_ford = gravelly_ford_flows - 5,
    mendota_dam = mendota_dam_flows,
    confluence = confluence_flows,
    sack_dam = sack_dam_flows, stringsAsFactors = FALSE)

}

get_daily_default_flow_schedule <- function(default_flow_schedule, year){

  period <- default_flow_schedule$period
  days <- default_flow_schedule$days
  friant_exhibitB <- default_flow_schedule$friant_exhibitB
  gravelly_ford_exhibitB <- default_flow_schedule$gravelly_ford_exhibitB
  mendota_dam_exhibitB <- default_flow_schedule$mendota_dam_exhibitB
  confluence_exhibitB <- default_flow_schedule$confluence_exhibitB
  friant_release <- default_flow_schedule$friant_release
  gravelly_ford_target <- default_flow_schedule$gravelly_ford_target
  SJRRP_flows_at_gravelly_ford <- default_flow_schedule$SJRRP_flows_at_gravelly_ford
  mendota_dam <- default_flow_schedule$mendota_dam
  confluence <- default_flow_schedule$confluence
  sack_dam <- default_flow_schedule$sack_dam
  start_month_days <- unlist(strsplit(period, ' - '))[c(TRUE, FALSE)]
  start_month_days[7] <- 'Sep 1'
  year <- c(rep(year, 11), year + 1)
  start_dates <- as.Date(paste(year, start_month_days), format = '%Y %b %d')

  daily_default_flow_schedule <- data.frame()

  for (i in 1:12) {
    temp <- data.frame(
      date = seq(start_dates[i], length = days[i], by = 'day'),
      period = rep(period[i], days[i]),
      friant_exhibitB = rep(friant_exhibitB[i], days[i]),
      gravelly_ford_exhibitB = rep(gravelly_ford_exhibitB[i], days[i]),
      mendota_dam_exhibitB = rep(mendota_dam_exhibitB[i], days[i]),
      confluence_exhibitB = rep(confluence_exhibitB[i], days[i]),
      friant_release = rep(friant_release[i], days[i]),
      gravelly_ford_target = rep(gravelly_ford_target[i], days[i]),
      SJRRP_flows_at_gravelly_ford = rep(SJRRP_flows_at_gravelly_ford[i], days[i]),
      mendota_dam = rep(mendota_dam[i], days[i]),
      confluence = rep(confluence[i], days[i]),
      sack_dam = rep(sack_dam[i], days[i]))

    daily_default_flow_schedule <- rbind(daily_default_flow_schedule, temp)
  }
  return(daily_default_flow_schedule)
}


