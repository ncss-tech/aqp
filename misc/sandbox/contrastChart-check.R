library(aqp)

# works as expected
contrastChart('10YR 3/4', hues = c('10YR'), style = 'hue')

# these were not working prior to 2021-01-06
# factor level bungling when full set of levels not present 
contrastChart(m = '10YR 3/4', hues = c('10YR'), style = 'hue', thresh = 12, returnData = TRUE)
contrastChart(m = '10YR 3/4', hues = c('10YR'), style = 'hue', thresh = 10, returnData = TRUE)

# interesting: this is what happens when you specify an in-between chip
contrastChart('10YR 6/7', hues = c('10YR'), style = 'hue', thresh = 5, returnData = TRUE)
