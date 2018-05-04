library(testthat)
# Friant Sheet Test
unimpaired_inflow <- 1270 # cell D3
allocation_lookup <- get_allocation_lookup(unimpaired_inflow)
allocation <- get_allocation(unimpaired_inflow, allocation_lookup) # cell C4
year_type <- get_year_type(allocation) # cell C5
restoration_af <- get_restoration_AF(unimpaired_inflow, allocation) # cell E5
addition_allocation <- get_additional_allocation(allocation, year_type) #cell K3
number_of_days <- get_number_of_days(year_type) # cell I4
friant_flows <- get_friant_default_schedule(year_type, addition_allocation)
gravelly_ford_flows <- get_gravelly_ford_flows(year_type, friant_flows)
gravelly_ford_losses <- get_R2_losses(gravelly_ford_flows)
mendota_dam_flows <- get_mendota_dam_flows(gravelly_ford_flows, gravelly_ford_losses)
confluence_flows <- get_confluence_flows(year_type, mendota_dam_flows)


cbind(SJRDefaultFlows::exhibitB_diversions_lookup$period, friant_flows,gravelly_ford_flows, mendota_dam_flows,confluence_flows)
