slicedHSD <- function(i, var, group, conf = 0.95) {
  
  # note: accessing sliced SPC out of function scope
  # local copy of current slice, keeping only horizons
  x <- horizons(s[, i])
  # move group from site -> hz
  x[[group]] <- denormalize(s[, i], group)
  
  # template for NA-results / error condition
  d.error <- data.frame(
    top = i - 1,
    bottom = i,
    diff = NA,
    lwr = NA,
    upr = NA,
    p.adj = NA
  )
  
  # model formula
  fm <- as.formula(sprintf('%s ~ %s', var, group))
  
  # catch errors
  mod <- try(
    aov(fm, data = x, silent = TRUE)
  )
  
  if(inherits(mod, 'try-error')) {
    return(d.error)
  }
  
  # does this need error trapping?
  res <- TukeyHSD(mod, conf.level = conf)
  
  # results: only works with A/B style testing of two groups
  d <- data.frame(
    top = i - 1,
    bottom = i,
    as.data.frame(res[[1]])
  )
  
  return(d)
}




