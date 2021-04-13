library(aqp)
library(stringr)

mixIt <- function(x, y) {
  mix <- try(mixMunsell(c(x,y), mixingMethod = 'exact')$munsell)
  if(inherits(mix, 'try-error')) {
    return(NA)
  } else {
    return(mix)
  }
}

mixIt <- Vectorize(mixIt)


mixtureGrid <- function(x) {
  
  # safely mix all permutations
  m <- outer(X = x, Y = x, FUN = mixIt)
  
  # diagonal without color
  mc <- m
  diag(mc) <- NA
  
  # create hex representation of color matrix
  m.colors <- parseMunsell(mc)
  
  # how many duplicates?
  # sort(table(m))
  
  # parameters required to make image
  n <- length(x)
  s <- 1:n
  cols <- parseMunsell(x)
  
  res <- list(
    x = x,
    m = m,
    m.colors = m.colors,
    n = n,
    s = s,
    cols = cols
  )
  
  return(res)
}

plotMixtureGrid <- function(g, fig.title = '') {

  par(mar = c(4.5, 4.5, 1.4, 1), bg = 'black', fg = 'white', xpd = NA)
  image(x = g$s, y = g$s, z = matrix(1:g$n^2, ncol = g$n), col = g$m.colors, axes = FALSE, ylab = '', xlab = '')
  
  abline(h = g$s - 0.5, v = g$s - 0.5, col = 'black')
  abline(h = g$n + 0.5, v = g$n + 0.5, col = 'black')
  
  text(row(g$m), col(g$m), labels = gsub(' ', '\n', g$m), cex = 0.45)
  
  axis(side = 1, at = g$s, labels = g$x, tick = FALSE, cex.axis = 0.75, las = 2, col.axis = 'white', line = 0.5, font.axis = 2)
  axis(side = 2, at = g$s, labels = g$x, tick = FALSE, las = 1, cex.axis = 0.75, col.axis = 'white', line = 0.5, font.axis = 2)
  
  points(x = g$s, y = rep(0.125, times = g$n), pch= 15, col = g$cols, cex = 2)
  points(y = g$s, x = rep(0.125, times = g$n), pch= 15, col = g$cols, cex = 2)
  
  title(fig.title, col.main = 'white', font.main = 4, line = 0.5)
  
}




# iterate over 10YR chips
# note that chips for 2/3 -> 2/8 aren't defined (not in the color book)
x <- expand.grid(hue = "10YR", value = seq(2, 8, by = 2), chroma = seq(2, 8, by = 2))
x <- x[order(x$value, x$chroma), ]
x <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)


## TODO: profile a single call to mixMunsell -> notice that most of the time is spent outside of distance calc 


# Euclidean distance via sweep / colSums: 85 seconds 
# Gower distance via gower package: 25 seconds (mixingMethod = 'reference')
# (mixingMethod = 'exact')
system.time(g <- mixtureGrid(x))
plotMixtureGrid(g)



plotColorMixture(c('10YR 8/6', '10YR 2/2'))

mixMunsell(c('10YR 2/4', '10YR 2/4'))
mixMunsell(c('10YR 2/2', '10YR 2/2'))
mixMunsell(c('10YR 6/2', '10YR 2/2'))
mixMunsell(c('10YR 6/2', '10YR 2/2'), mixingMethod = 'exact')


plotColorMixture(c('10YR 6/2', '10YR 2/2'))
plotColorMixture(c('5B 6/6', '10Y 8/4'))

plotColorMixture(c('5B 5/10', '5Y 8/8'))

# iterate over all hues @ 6/8
# result is a character matrix of Munsell chips
x <- sprintf("%s 6/8", huePosition(returnHues = TRUE))
g <- mixtureGrid(x)

png(filename = 'spilled-paint2.png', width = 1500, height = 1515, res = 120, type = 'cairo', antialias = 'subpixel')

plotMixtureGrid(g, fig.title = 'Clown Barf (Exact)')

dev.off()

# 
# # color clump image
# m.hue <- str_split_fixed(m, pattern = ' ', 2)[, 1]
# f <- factor(m.hue, levels = huePosition(returnHues = TRUE))
# m.int <- m
# m.int[] <- as.numeric(f)
# # f.cols <- parseMunsell(levels(f))
# 
# par(mar = c(4.5, 4.5, 1.4, 1), bg = 'black', fg = 'white', xpd = NA)
# image(x = s, y = s, z = matrix(1:n^2, ncol = n), col = m.int, axes = FALSE, ylab = '', xlab = '')
# 
# 
# 
# 
