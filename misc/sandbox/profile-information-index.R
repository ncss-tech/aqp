library(aqp)
library(soilDB)
library(data.table)
library(lattice)
library(tactile)

## TODO:
# * what is the baseline? 
# * compressed vs. raw
# * compressed values vs. compressed (mean or constant level factors)


a <- rnorm(n = 100)
b <- rep(mean(a), times = length(a))
a <- format(a, digits = 4)
b <- format(b, digits = 4)

length(memCompress(b, type = 'none')) / length(memCompress(a, type = 'gzip'))


.PI <- function(x, vars, d = 4) {
  h <- horizons(x)
  
  # select variables
  # numeric data truncated to 4 significant digits
  h <- lapply(h[, vars], function(i) {
    
    if(all(is.na(i))) {
      return(NA)
    }
    
    
    if(is.numeric(i)) {
      
      # baseline is mean(i)
      v <- as.character(signif(na.omit(i), digits = 6))
      b <- as.character(signif(rep(mean(i, na.rm = TRUE), times = length(v)), digits = 6))
      
      v <- memCompress(v, type = 'gzip')
      b <- memCompress(b, type = 'gzip')
      
    } else {
      
      # treating all categorical variables as nominal for now
      v <- as.character(na.omit(i))
      
      # baseline is the most frequent
      b <- names(sort(table(v), decreasing = TRUE))[1]
      b <- rep(b, times = length(v))
      
      v <- memCompress(v, type = 'gzip')
      b <- memCompress(b, type = 'gzip')
      
    }
    
    res <- length(v) / length(b)
    return(res)
  })
  
  h <- unlist(h)
  
  if(all(is.na(h))) {
    return(NA)
  }
  
  res <- mean(h, na.rm = TRUE) - 1
  
  return(res)
}



# .profileInformation <- function(x, vars, d = 4) {
#   h <- horizons(x)
#   
#   # select variables
#   # numeric data truncated to 4 significant digits
#   h <- lapply(h[, vars], function(i) {
#     if(is.numeric(i)) {
#       res <- format(na.omit(i), digits = d)
#     } else {
#       res <- as.character(na.omit(i))
#     }
#     return(res)
#   })
#   
#   # compressed column data
#   mc <- lapply(h, memCompress, type = 'gzip')
#   
#   # raw column data
#   mn <- lapply(h, memCompress, type = 'none')
#   
#   
#   res <- mean(sapply(mn, length)) / mean(sapply(mc, length))
#   
#   return(res)
# }

s <- c('holland', 'sierra', 'musick', 'hanford', 'grangeville', 'delhi', 'amador', 'cecil', 'leon', 'lucy', 'clarksville', 'zook', 'clear lake', 'yolo', 'calhi', 'corralitos', 'sacramento', 'dodgeland')
x <- fetchOSD(s)


vars <- c('top', 'bottom', 'hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH', 'pH_class', 'distinctness', 'topography')


x$pi <- profileApply(x, FUN = .PI, simplify = TRUE, vars = vars)

par(mar = c(3, 0, 0, 1))
plotSPC(x, width = 0.3, name.style = 'center-center', plot.order = order(x$pi), cex.names = 0.75, shrink = TRUE)
axis(side = 1, at = 1:length(x), labels = format(x$pi, digits = 2)[order(x$pi)], cex.axis = 0.66, las = 1)


sc <- data.table::fread('https://github.com/ncss-tech/SoilWeb-data/raw/main/files/SC-database.csv.gz')
sc <- as.data.frame(sc)

sc.sub <- subset(sc, subset = taxgrtgroup %in% c('haploxeralfs', 'palexeralfs', 'xerorthents'))

s <- sc.sub$soilseriesname
s <- split(s, makeChunks(s, size = 20))

x <- lapply(
  s,
  FUN = function(i) {
    fetchOSD(i)
  })

x <- combine(x)

vars <- c('top', 'bottom', 'hue', 'value', 'chroma', 'texture_class', 'cf_class', 'pH_class', 'distinctness', 'topography')


x$pi <- profileApply(x, FUN = .PI, simplify = TRUE, vars = vars)
x$nhz <- profileApply(x, FUN = nrow, simplify = TRUE)

x$greatgroup <- factor(x$greatgroup, levels = c('palexeralfs', 'haploxeralfs', 'xerorthents'))

bwplot(greatgroup ~ pi, data = site(x), par.settings = tactile.theme(), varwidth = TRUE, notch = TRUE, xlab = 'Profile Information Index')

bwplot(greatgroup ~ nhz, data = site(x), par.settings = tactile.theme(), varwidth = TRUE, notch = TRUE, xlab = 'Number of Horizons')

bwplot(pi ~ factor(nhz) | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons')


xyplot(pi ~ nhz | greatgroup, data = site(x), par.settings = tactile.theme(), ylab = 'Profile Information Index', xlab = 'Number of Horizons', type = c('g', 'p', 'r'))

bwplot(greatgroup ~ pi | factor(nhz), data = site(x), par.settings = tactile.theme(), xlab = 'Profile Information Index', as.table = TRUE)


cor(x$nhz, x$pi)




z1 <- lapply(letters[1:10], random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25)
z1 <- combine(z1)

z2 <- lapply(letters[11:20], random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=1, lpp.d=1, lpp.e=1, lpp.u=25)
z2 <- combine(z2)

site(z1)$g <- 'high'
site(z2)$g <- 'low'

z <- combine(z1, z2)

z$pi <- profileApply(z, FUN = .PI, simplify = TRUE, vars = c('top', 'bottom', 'p1'))
z$nhz <- profileApply(z, FUN = nrow, simplify = TRUE)

z$pi
z$nhz


par(mar = c(0, 0, 3, 0))
groupedProfilePlot(z, groups = 'g', color = 'p1')


tapply(z$pi, z$g, summary)



z1 <- lapply(1:50, random_profile, n = 5, exact = TRUE, n_prop = 1, SPC = TRUE, method = 'LPP', lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25, min_thick = 2, max_thick = 50)
z1 <- combine(z1)

z1$pi <- profileApply(z1, FUN = .PI, simplify = TRUE, vars = c('top', 'bottom', 'p1'))

par(mar = c(3, 0, 0, 0))
plotSPC(z1, color = 'p1', plot.order = order(z1$pi), print.id = FALSE, width = 0.35, divide.hz = FALSE)
axis(side = 1, at = 1:length(z1), labels = format(z1$pi[order(z1$pi)], digits = 2), cex.axis = 0.66)




