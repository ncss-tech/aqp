## inspired by Shawn Salley


library(aqp)
library(corrplot)
library(viridisLite)

# make n profiles of fake data
# compute pair-wise distances
# return full distance matrix
.makeData <- function(n) {
  x <- lapply(1:n, random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = TRUE)
  x <- combine(x)
  
  v <-  c('p1', 'p2', 'p3', 'p4')
  d <- NCSP(x, vars = v, k = 0, rescaleResult = TRUE)
  m <- as.matrix(d)
  dimnames(m) <- list(profile_id(x), profile_id(x))
  
  return(m)
}

# coorplot() wrapper, for plotting distances along a common color ramp
.plotData <- function(m) {
  par(bg = 'black', fg = 'white')
  corrplot(
    m, 
    col = mako(25), 
    is.corr = FALSE, 
    col.lim = c(0, 1), 
    method = "color", 
    order = "hclust",
    type = "upper", 
    tl.pos = "n",
    cl.pos = "n",
    mar = c(0.1, 0, 0, 0.8)
  ) 
}


# random number of profiles per figure, 10 figures
n.profiles <- sample(5:50, size = 10)

# manual specification
n.profiles <- c(35, 6, 100, 20, 15, 8, 45, 10, 67, 3)

# sorting is interesting
n.profiles <- sort(n.profiles)

# simulate data and prepare distance matrices
l <- lapply(n.profiles, .makeData)

par(mfrow=c(2, 5), bg = 'black', fg = 'white')
.junk <- lapply(l, .plotData)



