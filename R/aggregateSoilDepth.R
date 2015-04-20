
aggregateSoilDepth <- function(x, groups, crit.prob=0.9, name='hzname', p='Cr|R|Cd', ...) {
  
  # sanity checks:
  # 1. does the group label variable exist in @site?
  if(! groups %in% siteNames(x))
    stop('`groups` should specify a site-level attribute')
  
  # mark soil vs. non-soil horizons
  x$soil.flag <- rep('soil', times=nrow(x))
  x$soil.flag[grep(p, horizons(x)[[name]])] <- 'not-soil'
  
  # convert to factor
  x$soil.flag <- factor(x$soil.flag, levels=c('soil', 'not-soil'))
  
  # compute slice-wise probabilities that sum to contributing fraction
  fm <- as.formula(paste0(groups, ' ~ soil.flag'))
  a.ml.soil <- slab(x, fm, cpm=2, ...)
  
  ## note: this is important when aggregating over several depth classes
  # compute adjusted soil flag probability: cf * Pr(soil)
  a.ml.soil$adj.soil.flag <- with(a.ml.soil, contributing_fraction * soil)
  
  # convert probability of contact into probability of soil
  crit.prob <- 1 - crit.prob
  
  # extract depth at specific probability of contact
  depth.prob <- ddply(a.ml.soil, groups, .fun=function(i) {
    max(i$top[which(i$adj.soil.flag >= crit.prob)])
  })
  
  # add soil top boundary and fix names
  names(depth.prob)[2] <- 'soil.bottom'
  depth.prob$soil.top <- 0
  
  # re-order and return
  return(depth.prob[, c(groups, 'soil.top', 'soil.bottom')])
}

