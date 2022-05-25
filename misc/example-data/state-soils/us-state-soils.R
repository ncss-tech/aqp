## US State Soils
## 2020-10-08
##

# look-up abbreviations
library(datasets)
data('state')

x <- read.csv('state_soils.csv', stringsAsFactors = FALSE)

# abbreviated names
x$abbreviated <- state.abb[match(x$state, state.name)]

# fix PR and VI
x$abbreviated[x$state == 'Puerto Rico'] <- 'PR'
x$abbreviated[x$state == 'Virgin Islands'] <- 'VI'


us.state.soils <- x[, c('state', 'abbreviated', 'series')]

save(us.state.soils, file = '../../../data/us.state.soils.rda')


