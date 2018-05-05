
# test CH year type with 400 TAF unimpared inflow
ch <- read.csv('data-raw/ch.csv', stringsAsFactors = FALSE, colClasses = c('character', rep('numeric', 10)))
use_data(ch)

# test N-D year type 1250 TAF unimpared inflow
nd <- read.csv('data-raw/n-d.csv', stringsAsFactors = FALSE, colClasses = c('character', rep('numeric', 10)))
use_data(nd)

# test N-W year type with 2501 TAF unimpared inflow
nw <- read.csv('data-raw/n-w.csv', stringsAsFactors = FALSE, colClasses = c('character', rep('numeric', 10)))
use_data(nw)
