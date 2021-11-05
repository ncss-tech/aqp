library(aqp)

## idea 1: simulate some opinions of soil color, using discrete sampling and pre-specified proportions
n <- 8
hh <- sample(c('10YR', '7.5YR', '2.5Y'), size = n, replace = TRUE, prob = c(0.7, 0.2, 0.1))
vv <- sample(3:6, size = n, replace = TRUE)
cc <- sample(3:6, size = n, replace = TRUE)

# opinions of soil color
m2 <- sprintf('%s %s/%s', hh, vv, cc)

## idea 2: use a more advanced sampling approach, based on dE00 and fixed set of hues
m2 <- simulateColor(
  method = 'dE00', 
  n = 8, 
  parameters = list(m = '10YR 4/4', thresh = 10, hues = c('10YR', '7.5YR', '2.5Y'))
)

# unpack list
m2 <- m2[[1]]

# check
colorChart(m2)
colorChart(m2, annotate = TRUE, annotate.type = 'percentage', size = FALSE)
previewColors(parseMunsell(m2))


## idea 3
# manually specify opinions
m2 <- c('10YR 4/4', '7.5YR 4/4', '10YR 3/4', '10YR 4/3', '2.5Y 3/3')



# reference soil color
n <- length(m2)
m1 <- rep('10YR 4/4', times = n)


# compute color contrast metrics for sorting of opinions via dE00
cc <- colorContrast(m1, m2)

# re-order opinions based on dE00
o <- order(cc$dE00)
m2 <- m2[o]

# graphical summary
par(mar=c(4, 2, 0.25, 0.25), bg='black', fg='white')
colorContrastPlot(m1, m2, labels = c('reference', 'opinions'), d.cex = 0.8, col.cex = 0.8)

contrastChart('10YR 4/4', hues = c('10YR', '7.5YR', '2.5YR', '5YR', '2.5Y', '5Y'), thresh = 5)





