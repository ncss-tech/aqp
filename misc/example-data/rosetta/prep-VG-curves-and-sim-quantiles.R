library(aqp)
library(soilDB)
library(latticeExtra)

data("ROSETTA.centroids")

# iterate over horizons and generate VG model curve
res <- lapply(1:nrow(ROSETTA.centroids), function(i) {
  m <- KSSL_VG_model(VG_params = ROSETTA.centroids[i, ], phi_min = 10^-3, phi_max=10^6)$VG_curve
  # copy generalized hz label
  m$hz <- ROSETTA.centroids$hz[i]
  # copy ID
  m$texture_class <- ROSETTA.centroids$texture[i]
  return(m)
})

# copy over lab sample number as ID
res <- do.call('rbind', res)

# check: OK
str(res)


## there is no need to pre-make these curves, move to a new function
## prepareCurves or something like that


# visual check: OK
xyplot(
  phi ~ theta | texture_class, data=res, 
  type=c('l', 'g'), 
  scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), 
  yscale.components=yscale.components.logpower, 
  ylab=expression(Suction~~(kPa)), 
  xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3)), 
  par.settings = list(superpose.line=list(col='RoyalBlue', lwd=2)), 
  strip=strip.custom(bg=grey(0.85)), 
  as.table=TRUE
)


xyplot(
  phi ~ theta, groups=texture_class, data=res, 
  type=c('l', 'g'), 
  scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), 
  yscale.components=yscale.components.logpower, 
  ylab=expression(Suction~~(kPa)), 
  xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3)), 
  par.settings = list(superpose.line=list(col=viridis::viridis(12), lwd=2)), 
  strip=strip.custom(bg=grey(0.85)), 
  auto.key=list(columns=4, lines=TRUE, points=FALSE, cex=0.8)
)








d <- split(x, x$texture)

.sim <- function(i, n=1000) {
  
  # simulate all parameters
  theta_r <- rnorm(mean=i$theta_r, sd = i$theta_r_sd, n=n)
  theta_s <- rnorm(mean=i$theta_s, sd = i$theta_s_sd, n=n)
  alpha <- rnorm(mean=i$alpha, sd = i$alpha_sd, n=n)
  npar <- rnorm(mean=i$npar, sd = i$npar_sd, n=n)
  
  # collect
  vgp <- data.frame(theta_r, theta_s, alpha, npar)
  
  # run through model
  res <- lapply(1:nrow(vgp), function(j) {
    KSSL_VG_model(vgp[j, ], phi_min = 10^-3, phi_max=10^6)$VG_curve
  } )
  
  # stack
  res <- do.call('rbind', res)
  
  # bin according to regular intervals of theta
  s <- seq(from=0.05, to=0.5, length.out = 25)
  res$g <- cut(res$theta, breaks = s, include.lowest = TRUE, right = TRUE)
  
  # quantiles by bin
  res.q <- tapply(res$phi, res$g, quantile, probs=c(0.05, 0.5, 0.95), simplify = TRUE)
  res.q <- do.call('rbind', res.q)
  
  # format for plotting
  
  
  return(res)
}

dd <- lapply(d, .sim)
dd <- do.call('rbind', dd)

str(dd)



