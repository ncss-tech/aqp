library(aqp)
library(soilDB)
library(mgcv)
library(rms)
library(latticeExtra)


x <- fetchKSSL(c('drummer', 'ava', 'zook'))

x$taxonname <- toupper(x$taxonname)

plotSPC(x, color = 'clay')

x$mintop <- profileApply(x, FUN = function(i) min(i$hzn_top))

x <- subset(x,  mintop == 0 )
x <- trunc(x, 0, 100)
s <- dice(x)



groupedProfilePlot(s, groups = 'taxonname', color = 'clay', divide.hz = FALSE, name = NA, print.id = FALSE)


h <- as(s, 'data.frame')
h$taxonname <- factor(h$taxonname)


# # ... how to incorporate error structue?
# m <- gam(clay ~ s(hzn_top, by = taxonname), data = h)
# plot(m)


# slab
a <- slab(x, fm = taxonname ~ estimated_ph_h2o)

a <- a[, c('top', 'taxonname', 'p.q50', 'p.q25', 'p.q75')]
names(a) <- c('hzn_top', 'taxonname', 'linear.predictors', 'lower', 'upper')



# RMS approach, RCS -> fewer DF used
dd <- datadist(h[, c('hzn_top', 'estimated_ph_h2o', 'taxonname')])
options(datadist = "dd")

# works: uncertainty estimates are way off
m.ols <- Gls(estimated_ph_h2o ~ rcs(hzn_top, 10) * taxonname, data = h)

# getting the syntax right is tough
m.gls <- Gls(estimated_ph_h2o ~ rcs(hzn_top, 10) * taxonname, data = h, correlation = corCAR1(form = ~ hzn_top | pedon_key))

nd <- expand.grid(hzn_top = 1:100, taxonname = levels(h$taxonname))

p.ols <- predict(m.ols, newdata = nd, conf.int = 0.95)
p.gls <- predict(m.gls, newdata = nd, conf.int = 0.95)

p.ols <- cbind(nd, as.data.frame(p.ols))
p.gls <- cbind(nd, as.data.frame(p.gls))


g <- make.groups(
  'OLS 95% Conf.' = p.ols, 
  'GLS + corCAR1 95% Conf.' = p.gls,
  'aqp::slab q25,q50,q75' = a
)

head(g)


xyplot(
  hzn_top ~ linear.predictors | which, groups = taxonname, data = g,
  xlab = 'pH 1:1 H2O', ylab = 'Depth (cm)',
  asp = 1.5,
  lower = g$lower, upper = g$upper,
  sync.colors = TRUE, alpha = 0.33,
  ylim = c(105, -5),
  scales = list(alternating = 1),
  panel = panel.depth_function,
  par.settings = list(superpose.line = list(lwd = 2, col = 2:4), layout.heights = list(strip = 1.45)),
  prepanel = prepanel.depth_function,
  auto.key = list(columns = 3, lines = TRUE, points = FALSE),
  strip = strip.custom(bg = grey(0.85)),
  par.strip.text = list(cex = 0.8),
)  + latticeExtra::as.layer(xyplot(hzn_top ~ estimated_ph_h2o, groups = taxonname, data = h, cex = 0.125, par.settings = list(superpose.symbol = list(col = 2:4, alpha = 0.33))))








