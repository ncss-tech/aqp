# install mpsline2 from CRAN

library(aqp)
library(sharpshootR)
library(mpspline2)
library(reshape2)
library(lattice)

# make some example data
ids <- LETTERS[1]

set.seed(10101)
x <- lapply(ids, random_profile, n = c(6, 7, 8), n_prop = 1, method = 'LPP', SPC = TRUE, lpp.a = 5, lpp.b = 25, lpp.d = 10, lpp.e = 5, lpp.u = 25)
x <- combine(x)

# fake site data
site(x)$fake_site_attr <- 1:length(x)

# check source data
par(mar=c(0,0,3,1))
plotSPC(x, color='p1', col.palette = hcl.colors(10, 'viridis'))

s <- seq(0, 1, by = 0.25)
m <- lapply(s, function(i) {
  .spc <- spc2mpspline(x, var_name = 'p1', lam = i, method = 'est_1cm')
  profile_id(.spc) <- sprintf("%s-0%s", profile_id(.spc), i)
  return(.spc)
})

z <- combine(m)
z$p1 <- z$p1_spline

xx <- combine(z, x)

par(mar = c(0, 0, 3, 1))
plotSPC(xx, color = 'p1', col.palette = hcl.colors(10, 'viridis'), divide.hz = FALSE, width = 0.35)


o <- order(xx$p1_rmse)
plotSPC(xx, color = 'p1', col.palette = hcl.colors(10, 'viridis'), divide.hz = FALSE, width = 0.35, plot.order = o)


tps <- tactile::tactile.theme(superpose.line = list(lwd = 2))

# compare depth-functions by method, no aggregation
xyplot(cbind(top, bottom) ~ p1, id=xx$id, groups = factor(id),
       data=as(xx, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(125, -5), as.table=TRUE, panel=panel.depth_function,
       scales = list(alternating = 1),
       asp = 1
)

xyplot(cbind(top, bottom) ~ p1 | id, id=xx$id,
       data=as(xx, 'data.frame'),
       par.settings=tps,
       auto.key=list(columns=4, lines=TRUE, points=FALSE),
       strip=strip.custom(bg=grey(0.85)),
       ylim=c(125, -5), as.table=TRUE, panel=panel.depth_function,
       layout = c(6, 1),
       scales = list(alternating = 1)
)


