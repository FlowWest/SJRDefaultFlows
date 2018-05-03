
cfs_to_af <- function(cfs) {
  return(cfs * (24 * 60 * 60) / 43560)
}

af_to_cfs <- function(af) {
  return(af * 43560 / (24 * 60 * 60))
}
