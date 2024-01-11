library(aqp)


# for later: simulate realistic soil morphology

p <- matrix(
  c(0), 
  nrow = 30, 
  ncol = 10
)

for(i in 1:nrow(p)) {
  # adjust probabilities with each iteration
  p[i, ] <- sample(hz.levels, size = 10, replace = TRUE, prob = c(1, 1, 0.5, 0.25, 0.3, 0.1, 0.1))
}




## ideas on how to encode source data:
# * 1-letter codes representing sampling area (dz x dz)
# * horizon designations, would need to borrow from quickSPC() parser
# * 


s <- list(
  'p1' = c(
    'AAAAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'BBBBBAAA',
    'BBBBBBAA',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'CCCBBBBB',
    'CCCCBBBB',
    'CCCCCBBB',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'RRRCCRRR',
    'RRRRCRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRZ',  
    'ZZZZZZZZ'
  ),
  'p2' = c(
    'AAAAAAAA',
    'AXXAAAAA',
    'XXXAAAAA',
    'AAAAAAAA',
    'BBBBBAAA',
    'BBAABBAA',
    'BBAABBBB',
    'AAAABBBB',
    'BBBABBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'CCCBBBBB',
    'CCCCBBBB',
    'CCCCCBBB',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCCCCCC',
    'RRCCCRRR',
    'CCCCCRRR',
    'CCCRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',  
    'RRRRRRRR'
  ),
  'p3' = c(
    'OOOOOOOO',
    'OOOOOOOO',
    'OOOOOOOO',
    'OOOOAAAA',
    'AAOAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'BBBABAAA',
    'BBAABBAA',
    'BBAABBBB',
    'AAAABBBB',
    'BBBABBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'CCCBBBBB',
    'CCCCBBBB',
    'CCCCCBBB',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',  
    'RRRRRRRR'
  ),
  'p4' = c(
    'OOOOOOOO',
    'OOOOAAAA',
    'AAOAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'BBBABAAA',
    'BBAABBAA',
    'BBAABBBB',
    'AAAABBBB',
    'BBAABBBB',
    'AAAABBBB',
    'BBAABBBB',
    'AAAABBBB',
    'BBBABBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'BBBBBBBB',
    'CCCBBBBB',
    'CCCCBBBB',
    'CCCCCBBB',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',  
    'RRRRRRRR'
  ),
  'p5' = c(
    'OOOOOOOO',
    'OOOOAAAA',
    'AAOAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'AAAAAAAA',
    'BBBABAAA',
    'BBAABBAA',
    'BBAABBBB',
    'AAAABBBB',
    'BBAABBBB',
    'AAAABBBB',
    'BBAABBBB',
    'AAAABBBB',
    'BBBZZBBB',
    'ZZZZZBBB',
    'BBBZXBBB',
    'BBBBBBBB',
    'XXXXXBBB',
    'XXXZZBBB',
    'XXXXXBBB',
    'CCCCCBBB',
    'CCCCCCCC',
    'CCCCCCCC',
    'CCCRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',
    'RRRRRRRR',  
    'RRRRRRRR'
  )
)




griddedHZ <- function(x, ll, dw = 10, dz = 10) {
  
  # split horizon templates
  l <- lapply(x, strsplit, fixed = TRUE, split = '')
  
  # convert to matrix
  # rows: depth intervals (z)
  # columns: width intervals (q)
  m <- do.call('rbind', sapply(l, '['))
  
  # integer representation of factor levels
  h <- matrix(
    as.integer(factor(m, levels = ll)), 
    ncol = ncol(m), 
    nrow = nrow(m), 
    byrow = FALSE
  )
  
  # width sequence
  w <- seq(0, ncol(h)) * dw
  
  # depth sequence
  z <- seq(0, nrow(h)) * dz
  
  # width sequence, scaled for screen
  wS <- aqp:::.rescaleRange(w, 0, 0.5)
  
  # results
  res <- list(
    hz = h,
    w = w,
    wS = wS,
    z = z
  )
  
  return(res)
}


hz.levels <- c('O', 'A', 'B', 'C', 'R', 'X', 'Z')

.nm <- names(s)
.n <- length(s)

x <- list()
for(i in seq_along(s)) {
  
  .g <- griddedHZ(s[[i]], ll = hz.levels)
  .g$id <- .nm[i]
  .g$seq <- i
  x[[i]] <- .g
  
}


.zlim <- range(seq_along(hz.levels))
.cols <- hcl.colors(n = length(hz.levels), palette = 'Zissou1')


par(mar = c(3, 1, 3, 2), xpd = NA)

plot(0, 0, type = 'n', xlim = c(0, .n+1), ylim = c(max(.z), 0), axes = FALSE, xlab = '', ylab = '')


.trash <- lapply(x, function(i) {
  image(
    x = i$wS + (i$seq - 0.25), 
    y = i$z, 
    z = t(i$hz), 
    add = TRUE, 
    col = .cols, 
    zlim = .zlim, 
    useRaster = TRUE,
    oldstyle = TRUE
  )
})

text(x = 1:.n, y = 0, labels = sapply(x, '[[', 'id'), pos = 3)

axis(side = 1, at = 1:.n, cex.axis = 0.8)
axis(side = 4, las = 1, line = -2, cex.axis = 0.75)
legend('topleft', legend = hz.levels, pch = 15, pt.cex = 1.5, col = .cols, bty = 'n')


