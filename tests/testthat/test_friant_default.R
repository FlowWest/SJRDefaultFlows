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
get_gravelly_flows(year_type)
