context("methods for plotting slab() output")

library(lattice, quietly = TRUE)
library(grid, quietly = TRUE)


# load sample data, upgrade to SoilProfileCollection
data(sp1)
depths(sp1) <- id ~ top + bottom

# aggregate entire collection with two different segment sizes
a <- slab(sp1, fm = ~ prop)
b <- slab(sp1, fm = ~ prop, slab.structure=5)

# stack into long format
ab <- make.groups(a, b)
ab$which <- factor(ab$which, levels=c('a','b'),
                   labels=c('1-cm Interval', '5-cm Interval'))


test_that("lattice helper functions work as expected", {
  
  # plot median and IQR
  # custom plotting function for uncertainty viz.
  fig <- xyplot(top ~ p.q50 | which, data=ab, ylab='Depth',
         xlab='median bounded by 25th and 75th percentiles',
         lower=ab$p.q25, upper=ab$p.q75, ylim=c(250,-5),
         panel=panel.depth_function,
         prepanel=prepanel.depth_function,
         cf=ab$contributing_fraction,
         alpha=0.5,
         layout=c(2,1), scales=list(x=list(alternating=1))
  )
  
  expect_true(inherits(fig, 'trellis'))
  
  
})



