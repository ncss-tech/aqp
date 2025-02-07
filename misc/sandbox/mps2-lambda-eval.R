# install mpsline2 from CRAN

library(aqp)
library(soilDB)
library(sharpshootR)
library(mpspline2)
library(reshape2)
library(lattice)


# x <- fetchKSSL(series = 'clarksville')
# 
# plotSPC(x[2, ], color = 'clay')
# 
# x <- x[2, ]
# x$p1 <- x$clay
# x$top <- x$hzn_top
# x$bottom <- x$hzn_bot
# profile_id(x) <- 'clarksville'
# horizonDepths(x) <- c('top', 'bottom')


# # # make some example data
# ids <- LETTERS[1:3]
# 
# set.seed(10101)
# x <- lapply(
#   ids,
#   random_profile,
#   n = c(6, 7, 8),
#   n_prop = 1,
#   method = 'LPP',
#   SPC = TRUE,
#   lpp.a = 5,
#   lpp.b = 30,
#   lpp.d = 10,
#   lpp.e = 5,
#   lpp.u = 25
# )
# 
# x <- combine(x)
# horizons(x)$hzd <- 0
# site(x)$group <- profile_id(x)



## example data

x <- list(
  id = 'P1',
  depths = c(5, 25, 33, 100, 150),
  name = c('A', 'Bt1', 'Bt2', 'BC', 'Cr'),
  p1 = c(5, 25, 35, 10, 8),
  soil_color = c('10YR 2/2', '10YR 3/3', '10YR 4/4', '10YR 4/6', '5G 6/2'),
  hzd = c('clear', 'clear', 'abrupt', 'gradual', NA)
)

x <- quickSPC(x)
x$hzd <- hzDistinctnessCodeToOffset(x$hzd)
site(x)$group <- 'A'


## wait for this to be fixed
# https://github.com/obrl-soil/mpspline2/issues/9
# x$p1 <- scale(x$p1)



## PAWS example
# 
# x <- list(
#   id = 'P1',
#   depths = c(5, 25, 33, 100, 150),
#   name = c('A', 'Bt1', 'Bt2', 'BC', 'Cr'),
#   awc_r = c(0.11, 0.15, 0.18, 0.08, 0.05),
#   soil_color = c('10YR 2/2', '10YR 3/3', '10YR 4/4', '10YR 4/6', '5G 6/2'),
#   hzd = c('clear', 'clear', 'abrupt', 'gradual', NA)
# )
# 
# 
# x <- quickSPC(x)
# x$hzd <- hzDistinctnessCodeToOffset(x$hzd)
# 
# x$p1 <- (x$bottom - x$top) * x$awc_r




# fake site data
site(x)$fake_site_attr <- 1:length(x)


.cols <- hcl.colors(50, 'spectral')

# check source data
par(mar = c(0, 0, 3, 1))
plotSPC(x, color = 'p1', col.palette = .cols, hz.distinctness.offset = 'hzd')

# iterate over possible lambda values
# 0.1 is default
s <- c(0.1, 0.25, 0.3, 0.5, 1)
m <- lapply(s, function(i) {
  .spc <- spc2mpspline(x, var_name = 'p1', lam = i, method = 'est_1cm')
  profile_id(.spc) <- sprintf("%s-0%s", profile_id(.spc), i)
  return(.spc)
})

z <- combine(m)
z$p1 <- z$p1_spline

xx <- combine(z, x)

# site(xx)$id <- profile_id(xx)


par(mar = c(0, 0, 3, 1))

plotSPC(xx, color = 'p1', col.palette = .cols, divide.hz = FALSE, width = 0.35, name = NA, lwd = 0, hz.distinctness.offset = 'hzd')


o <- order(xx$p1_rmse)
plotSPC(xx, color = 'p1', col.palette = .cols, divide.hz = FALSE, width = 0.35, plot.order = o, name = NA, lwd = 0, hz.distinctness.offset = 'hzd')


.cols <- c(hcl.colors(n = 5), 'firebrick')
tps <- tactile::tactile.theme(superpose.line = list(lwd = 2, col = .cols))

site(xx)$lambda <- factor(c('original', s))


# compare depth-functions by method, no aggregation
xyplot(
  cbind(top, bottom) ~ p1 | group, 
  id = factor(xx$id), 
  groups = lambda,
  data = as(xx, 'data.frame'),
  par.settings = tps,
  auto.key = list(columns = 1, lines = TRUE, points = FALSE, title = 'lambda', space = 'right', cex = 0.8),
  strip = strip.custom(bg = grey(0.85)),
  ylim = c(160, -5), 
  ylab = 'Depth (cm)',
  # main = '',
  as.table = TRUE, 
  panel = panel.depth_function,
  scales = list(alternating = 1, y = list(tick.number = 15)),
  asp = 2
)


# note: can only be used on single-profile groups
slabInterval <- function(i, .f = sum) {
  .ss <- as.numeric(strsplit(i, '-', fixed = TRUE)[[1]])
  
  .a <- slab(xx, lambda ~ p1,  slab.structure = .ss, slab.fun = .f, na.rm = TRUE)
  .a <- dcast(.a, lambda ~ variable, value.var = 'value')
  names(.a)[2] <- i
  
  return(.a)
}

l <- lapply(
  c('0-150', '25-50', '75-100', '0-25', '0-75'), 
  slabInterval
)

a <- Reduce(merge, l)


knitr::kable(a, row.names = FALSE, digits = 0, caption = 'Sum of values over depth interval.')



# xyplot(cbind(top, bottom) ~ p1 | id, id=xx$id,
#        data=as(xx, 'data.frame'),
#        par.settings=tps,
#        auto.key=list(columns=4, lines=TRUE, points=FALSE),
#        strip=strip.custom(bg=grey(0.85)),
#        ylim=c(150, -5), as.table=TRUE, panel=panel.depth_function,
#        layout = c(6, 1),
#        scales = list(alternating = 1)
# )
# 
# 
# 
# 
# 
