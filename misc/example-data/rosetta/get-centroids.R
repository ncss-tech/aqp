##
## get class centroids from Rosetta manual
##

library(rvest)
library(xml2)

x <- read_html('https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-class-average-hydraulic-parameters/')

# get the main table
ht <- html_table(x, fill=TRUE, header = TRUE)

str(ht, 1)

# the third entry seems to be correct
v <- ht[[3]]

# extract mean and SD entries
v.mean <- v[, c(1, 3, 5, 7, 9)]
v.sd <- v[, c(4, 6, 8, 10)]

# names
names(v.mean) <- c('texture', 'theta_r', 'theta_s', 'alpha', 'npar')
names(v.sd) <- c('theta_r_sd', 'theta_s_sd', 'alpha_sd', 'npar_sd')

# OK, SD still needs to be cleaned-up
str(v.mean)
str(v.sd)

# strip '()' and convert to numeric
v.sd <- data.frame(
  lapply(v.sd, function(i) {
    as.numeric(gsub(pattern='[()]', replacement = '', x = i))
  })
)

# all together now
z <- cbind(v.mean, v.sd)

# save to CSV for manual editing
write.csv(z, file='centroids.csv', row.names = FALSE)

## stop ! manual editing of centroids required

