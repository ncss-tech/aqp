





## TODO:
# * store key attributes in slab() output
# * pre-compute most of the manually-specified parameters
# * lattice AND ggplot2 output
# * enhanced formula for upper / lower?

slabPlot <- function(x, fm, lower = NULL, upper = NULL, ylab = 'Depth (cm)', xlab = 'median bounded by 5th and 95th percentiles', alpha = 0.5, ...) {
  
  
  .lwr <- lower
  .upr <- upper
  .cf <- x[['contributing_fraction']]
  
  
  xyplot(
    fm, 
    data = x, 
    lower = .lwr, 
    upper = .upr, 
    cf = .cf,
    alpha = alpha,
    ylim = c(105,-5),
    panel = panel.depth_function,
    prepanel = prepanel.depth_function,
    ylab = ylab,
    xlab = xlab,
    par.settings = tactile.theme(superpose.line = list(lwd = 2)),
    strip = strip.custom(bg = grey(0.85)),
    ...
    
  )
  
  
   
}




library(aqp)
library(lattice)
library(latticeExtra)
library(tactile)


n <- 20

set.seed(1010101)
x <- combine(
  lapply(letters[1:n], random_profile, method = 'LPP', SPC = TRUE)
)

hzdesgnname(x) <- 'name'

x1 <- trunc(x[1:(n/2), ], z1 = 0, z2 = 30)
x2 <- x[(n/2)+1:n, ]


spc.list <- list(x1, x2)
args.list <- list(list(color = 'p1'), list(color = 'p1'))

plotMultipleSPC(
  spc.list, 
  group.labels = c('fixed depth', 'NRCS'), 
  max.depth = 105, 
  args = args.list, 
  merged.legend = 'p1'
)


site(x1)$group <- 'fixed depth'
site(x2)$group <- 'NRCS'
g <- combine(x1, x2)

g$group <- factor(g$group)



a <- slab(g, fm = group ~ p1 + p2 + p3)

slabPlot(a, fm = top ~ p.q50 | variable, groups = group, lower = a$p.q25, upper = a$p.q75)

