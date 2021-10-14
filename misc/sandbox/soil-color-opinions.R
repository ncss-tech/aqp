library(aqp)

# simulate some opinions of soil color
n <- 8
hh <- sample(c('10YR', '7.5YR', '2.5Y'), size = n, replace = TRUE, prob = c(0.7, 0.2, 0.1))
vv <- sample(3:6, size = n, replace = TRUE)
cc <- sample(3:6, size = n, replace = TRUE)

# opinions of soil color
m2 <- sprintf('%s %s/%s', hh, vv, cc)

# manually specify opinions
m2 <- c('10YR 4/4', '7.5YR 4/4', '10YR 3/4', '10YR 4/3', '2.5Y 3/3')
n <- length(m2)


# reference soil color
m1 <- rep('10YR 4/4', times=n)


# compute color contrast metrics for sorting of opinions via dE00
cc <- colorContrast(m1, m2)

# # re-order opinions
# o <- order(cc$dE00)
# m2 <- m2[o]

# graphical summary
par(mar=c(4, 2, 0.25, 0.25), bg='black', fg='white')
colorContrastPlot(m1, m2, labels = c('reference', 'opinions'), d.cex = 0.8, col.cex = 0.8)
