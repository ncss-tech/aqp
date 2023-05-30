devtools::load_all()


## problematic shuffling to accommodate overlapping labels

plotColorMixture(
  x = c('5G 5/10', '5Y 3/3'),
  w = c(1, 1),
  mixingMethod = 'exact'
)


plotColorMixture(c('10YR 5/3', '10YR 3/2', '5R 2/2'))
plotColorMixture(c('10YR 5/3', '10YR 3/2', '5R 2/2'), label.cex = 0.65)


plotColorMixture(c('5B 5/10', '5Y 8/8'), label.cex = 0.65, showMixedSpec = TRUE, mixingMethod = 'reference')
plotColorMixture(c('5B 5/10', '5Y 8/8'), label.cex = 0.65, showMixedSpec = TRUE, mixingMethod = 'reference', n = 3)

