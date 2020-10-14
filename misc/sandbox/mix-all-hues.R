library(aqp)
library(stringr)

mixIt <- function(x, y) {
  mix <- try(mixMunsell(c(x,y))$munsell)
  if(inherits(mix, 'try-error')) {
    return(NA)
  } else {
    return(mix)
  }
}

# iterate over all hues @ 6/8
# result is a character matrix of Munsell chips
x <- sprintf("%s 6/8", huePosition(returnHues = TRUE))
m <- outer(X = x, Y = x, FUN = Vectorize(mixIt))


# diagonal without color
mc <- m
diag(mc) <- NA

# create hex representation of color matrix
m.colors <- parseMunsell(mc)



# how many duplicates?
sort(table(m))

# parameters required to make image
n <- length(x)
s <- 1:n
cols <- parseMunsell(x)


png(filename = 'spilled-paint.png', width = 1500, height = 1515, res = 120, type = 'cairo', antialias = 'subpixel')

par(mar = c(4.5, 4.5, 1.4, 1), bg = 'black', fg = 'white', xpd = NA)
image(x = s, y = s, z = matrix(1:n^2, ncol = n), col = m.colors, axes = FALSE, ylab = '', xlab = '')

abline(h = s - 0.5, v = s - 0.5, col = 'black')
abline(h = n + 0.5, v = n + 0.5, col = 'black')

text(row(m), col(m), labels = gsub(' ', '\n', m), cex = 0.45)

axis(side = 1, at = s, labels = x, tick = FALSE, cex.axis = 0.75, las = 2, col.axis = 'white', line = 0.5, font.axis = 2)
axis(side = 2, at = s, labels = x, tick = FALSE, las = 1, cex.axis = 0.75, col.axis = 'white', line = 0.5, font.axis = 2)

points(x = s, y = rep(0.125, times = n), pch= 15, col = cols, cex = 2)
points(y = s, x = rep(0.125, times = n), pch= 15, col = cols, cex = 2)

title('Clown Barf', col.main = 'white', font.main = 4, line = 0.5)

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
