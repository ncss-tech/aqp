library(aqp)

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



# diag(m) <- NA

n <- length(x)
s <- 1:n
cols <- parseMunsell(x)

par(mar = c(4, 4.5, 2, 1), bg = 'black', fg = 'white', xpd = NA)
image(x = s, y = s, z = matrix(1:n^2, ncol = n), col = m, axes = FALSE, ylab = '', xlab = '')
abline(h = s - 0.5, v = s - 0.5, col = 'black')
abline(h = n + 0.5, v = n + 0.5, col = 'black')
axis(side = 1, at = s, labels = x, tick = FALSE, cex.axis = 0.66, las = 2, col.axis = 'white', line = 0)
axis(side = 2, at = s, labels = x, tick = FALSE, las = 1, cex.axis = 0.66, col.axis = 'white', line = 0.5)

points(x = s, y = rep(0, times = n), pch= 15, col = cols, cex = 2)
points(y = s, x = rep(0, times = n), pch= 15, col = cols, cex = 2)

title('Spilled Paint', col.main = 'white', font.main = 4)
