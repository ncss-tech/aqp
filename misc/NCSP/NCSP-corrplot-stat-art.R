## inspired by Shawn Salley


library(aqp)
library(corrplot)


# make n profiles of fake data
.makeData <- function(n) {
  x <- lapply(1:n, random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = TRUE)
  x <- combine(x)
  
  return(x)
}

# compute pair-wise distances
# return full distance matrix
.doNCSP <- function(i) {
  
  v <-  c('p1', 'p2', 'p3', 'p4')
  d <- NCSP(i, vars = v, k = 0, rescaleResult = TRUE)
  m <- as.matrix(d)
  dimnames(m) <- list(profile_id(i), profile_id(i))
  
  return(m)
}

# coorplot() wrapper, for plotting distances along a common color ramp
.plotData <- function(m) {
  par(bg = 'black', fg = 'white')
  corrplot(
    m, 
    col = hcl.colors(n = 25, palette = 'Mako'), 
    is.corr = FALSE, 
    col.lim = c(0, 1), 
    method = "color", 
    order = "original",
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
.profiles <- lapply(n.profiles, .makeData)

# compute distances
.dist <- lapply(.profiles, .doNCSP)

par(mfrow=c(2, 5), bg = 'black', fg = 'white')
.junk <- lapply(.dist, .plotData)

# compute information content
.info <- lapply(.profiles, profileInformationIndex, vars =  c('p1', 'p2', 'p3', 'p4'), method = 'sum', baseline = FALSE)

sapply(.info, sum)


