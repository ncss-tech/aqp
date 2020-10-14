library(aqp)
library(ragg)

# x <- c('5Y 6/8', '10YR 6/8', '2.5YR 6/8', '5R 6/8', '5G 6/8')


f <- function(x, y) {
  mix <- try(mixMunsell(c(x,y))$munsell)
  if(inherits(mix, 'try-error')) {
    return(NA)
  } else {
    col <- parseMunsell(mix)
    return(col)
  }
}

x <- sprintf("%s 6/8", huePosition(returnHues = TRUE))
m <- outer(X = x, Y = x, FUN = Vectorize(f))


# diagnoal without color?
diag(m) <- NA

n <- length(x)
s <- 1:n
cols <- parseMunsell(x)


png(filename = 'spilled-paint.png', width = 900, height = 915, res = 120, type = 'cairo', antialias = 'subpixel')

par(mar = c(4, 4.5, 1.25, 1), bg = 'black', fg = 'white', xpd = NA)
image(x = s, y = s, z = matrix(1:n^2, ncol = n), col = m, axes = FALSE, ylab = '', xlab = '')

abline(h = s - 0.5, v = s - 0.5, col = 'black')
abline(h = n + 0.5, v = n + 0.5, col = 'black')

axis(side = 1, at = s, labels = x, tick = FALSE, cex.axis = 0.66, las = 2, col.axis = 'white', line = 0.5)
axis(side = 2, at = s, labels = x, tick = FALSE, las = 1, cex.axis = 0.66, col.axis = 'white', line = 0.5)

points(x = s, y = rep(0, times = n), pch= 15, col = cols, cex = 2)
points(y = s, x = rep(0, times = n), pch= 15, col = cols, cex = 2)

title('Spilled Paint', col.main = 'white', font.main = 4, line = 0.5)

dev.off()
