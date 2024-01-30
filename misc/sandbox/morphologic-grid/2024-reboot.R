library(aqp)


## ideas on how to encode source data:
# * 1-letter codes representing sampling area (dz x dz)
# * horizon designations, would need to borrow from quickSPC() parser
# * 

# 
# s <- list(
#   'p1' = c(
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'BBBBBAAA',
#     'BBBBBBAA',
#     'BBBBBBBB',
#     'B***BBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'CCCBBBBB',
#     'CCCCBBBB',
#     'CCCCCBBB',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'RRRCCRRR',
#     'RRRRCRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRZ',  
#     'ZZZZZZZZ'
#   ),
#   'p2' = c(
#     'AAAAAAAA',
#     'AXXAAAAA',
#     'XXXAAAAA',
#     'AAAAAAAA',
#     'BBBBBAAA',
#     'BBAABBAA',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBBABBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'CCCBBBBB',
#     'CCCCBBBB',
#     'CCCCCBBB',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'RRCCCRRR',
#     'CCCCCRRR',
#     'CCCRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',  
#     'RRRRRRRR'
#   ),
#   'p3' = c(
#     'OOOOOOOO',
#     'OOOOOOOO',
#     'OOOOOOOO',
#     'OOOOAAAA',
#     'AAOAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'BBBABAAA',
#     'BBAABBAA',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBBABBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'CCCBBBBB',
#     'CCCCBBBB',
#     'CCCCCBBB',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',  
#     'RRRRRRRR'
#   ),
#   'p4' = c(
#     'OOOOOOOO',
#     'OOOOAAAA',
#     'AAOAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'BBBABAAA',
#     'BBAABBAA',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBBABBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'BBBBBBBB',
#     'CCCBBBBB',
#     'CCCCBBBB',
#     'CCCCCBBB',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',  
#     'RRRRRRRR'
#   ),
#   'p5' = c(
#     'OOOOOOOO',
#     'OOOOAAAA',
#     'AAOAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'AAAAAAAA',
#     'BBBABAAA',
#     'BBAABBAA',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBAABBBB',
#     'AAAABBBB',
#     'BBBZZBBB',
#     'ZZZZZBBB',
#     'BBBZXBBB',
#     'BBBBBBBB',
#     'XXXXXBBB',
#     'XXXZZBBB',
#     'XXXXXBBB',
#     'CCCCCBBB',
#     'CCCCCCCC',
#     'CCCCCCCC',
#     'CCCRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',
#     'RRRRRRRR',  
#     'RRRRRRRR'
#   )
# )


## simulate source data for testing



# hz: horizon and depth-function templates
# plot: graphical sanity check on results
initHzDepthFunctions <- function(hz, plot = TRUE) {
  
  # generate a depth function for each horizon template
  p <- lapply(seq_along(1:nrow(hz)), function(i) {
    zapsmall(
      # work at depth interval mid points
      dnorm(z.i + (dz / 2), mean = hz$m[i], sd = hz$sd[i])
    )
  })
  
  # combine define functions 
  p <- do.call('cbind', p)
  
  # normalize probability
  p <- zapsmall(sweep(p, MARGIN = 1, STATS = rowSums(p), FUN = '/'))
  
  if(plot) {
    # mask 0 via NA, in temp copy
    pp <- p
    pp[pp < 1e-5] <- NA
    
    matplot(
      x = z.i, 
      y = pp, 
      type = 'b', 
      pch = 1, 
      cex = 0.5,
      xlab = 'Depth (cm)',
      ylab = 'Pr(H | depth)',
      axes = FALSE
    )
    
    axis(side = 1, at = pretty(z.i, n = 12), cex.axis = 0.75)
    axis(side = 2, at = seq(0, 1, by = 0.1), cex.axis = 0.75, las = 1)
  }
  
  
  
  return(p)
}

# hz: horizon depth function probability definitions
sim2d <- function(hz, sort = FALSE) {
  
  # sample horizon codes within each depth interval (z)
  x <- lapply(z, function(i, .sort = sort) {
    
    # adjust probabilities based on simulated depth-functions
    .s <- sample(hz$hz.code, size = w.n, replace = TRUE, prob = p[i, ])
    
    # optionally sort of an alternative representation
    if(.sort) {
      .s <- sort(.s)
    }
    
    # collapse to string representation of hz designation codes
    .s <- paste(.s, collapse = '')
    return(.s)
  } 
  )
  
  # convert list -> vector
  x <- do.call('c', x)
  
  return(x)
}



griddedHZ <- function(x, ll, dw, dz) {
  
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
  wS <- aqp:::.rescaleRange(w, 0, 0.66)
  
  # results
  res <- list(
    hz = h,
    w = w,
    wS = wS,
    z = z
  )
  
  return(res)
}


## TODO: abstract to more intuitive depth scale vs. character pixels

## boundary conditions

# vertical dimension
z.min <- 0
z.max <- 200
dz <- 5

# width dimension
w.min <- 0
w.max <- 100
dw <- 10

## vertical sequence
# top of cell, depth units
z.i <- seq(from = z.min, to = z.max - dz, by = dz)
# position in matrix 
z <- seq_along(z.i)
z.n <- length(z)

## width sequence
# left side of cell, depth units
w.i <- seq(from = w.min, to = w.max - dw, by = dw)
# position in matrix
w <- seq_along(w.i)
w.n <- length(w)


## horizonation depth function templates
## TODO: enforce expected transitions and terminal states (R)

hz <- data.frame(
  hz.code = c('A',  'B', 'C',   'D',  'E',  'F'),
  hz.name = c('Oi', 'A', 'Bw', 'BC',  'C',  'R'),
  m =       c(0,     15,  50,    65,  125,  200),
  sd =      c(3,     6,   15,    10,   15,   30)
)

# build depth-function matrix
par(mar = c(3, 4.5, 3, 1))
p <- initHzDepthFunctions(hz)


## simulation
n.profiles <- 8
pIDs <- sprintf("p%02d", 1:n.profiles)

s <- lapply(1:n.profiles, function(i) {
  sim2d(hz = hz, sort = TRUE)
})

names(s) <- pIDs


.zlim <- range(seq_along(hz$hz.code))
.ylim <- c(max(z) * dz, 0)
.xlim <- c(0, n.profiles + 1)

.cols <- hcl.colors(n = length(hz$hz.code), palette = 'Zissou1')


x <- list()
for(i in seq_along(s)) {
  
  .g <- griddedHZ(s[[i]], ll = hz$hz.code, dw = dw, dz = dz)
  .g$id <- pIDs[i]
  .g$seq <- i
  x[[i]] <- .g
  
}



par(mar = c(3, 1, 3, 2), xpd = NA)

plot(0, 0, type = 'n', xlim = .xlim, ylim = .ylim, axes = FALSE, xlab = '', ylab = '')


.trash <- lapply(x, function(i) {
  image(
    x = i$wS + (i$seq - 0.33), 
    y = i$z, 
    z = t(i$hz), 
    add = TRUE, 
    col = .cols, 
    zlim = .zlim, 
    useRaster = TRUE,
    oldstyle = TRUE
  )
})

text(x = 1:n.profiles, y = 0, labels = sapply(x, '[[', 'id'), pos = 3)

axis(side = 1, at = 1:n.profiles, cex.axis = 0.8)
axis(side = 4, las = 1, line = -2, cex.axis = 0.75)
legend('topleft', legend = hz$hz.code, pch = 15, pt.cex = 1.5, col = .cols, bty = 'n')

## demo ASCI representation
cat(s[[1]], sep = '\n')



