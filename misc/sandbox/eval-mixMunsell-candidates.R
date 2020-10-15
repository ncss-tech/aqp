library(aqp)

## TODO: think about adding an argument to mixMunsell that would clamp to a specific hue


# should the most likely mixture be 10YR?
m <- c('10YR 8/6', '10YR 2/2')
x <- mixMunsell(m, n = 20)

# it isn't
x[1, ]

# how close are the top 20 candidates?

# compute dE00 between 1st and rest of matches
z <- expand.grid(m1 = x$munsell[1], m2 = x$munsell)
d <- colorContrast(z[[1]], z[[2]])


n <- nrow(x)
s <- 1:n
cols <- parseMunsell(x$munsell)

par(mar = c(4.5, 4.5, 2, 1), bg = 'black', fg = 'white')
plot(s, x$distance, type = 's', las = 1, ylab = 'Spectral Distance', xlab = 'Match Candidate', col.axis = 'white', col.lab = 'white')
points(s, x$distance, pch = 15, col = cols, cex= 4)
text(s, x$distance, gsub(' ', '\n', x$munsell), cex = 0.5, col = invertLabelColor(cols))
title(sprintf("Mixture: %s + %s", m[1], m[2]), col.main = 'white')

# par(mar = c(4.5, 4.5, 1, 1), bg = 'black', fg = 'white')
plot(s, d$dE00, type = 's', las = 1, ylab = expression(~Delta*E['00']), xlab = 'Match Candidate', col.axis = 'white', col.lab = 'white')
points(s, d$dE00, pch = 15, col = cols, cex= 4)
text(s, d$dE00, gsub(' ', '\n', x$munsell), cex = 0.5, col = invertLabelColor(cols))
title(sprintf("Mixture: %s + %s", m[1], m[2]), col.main = 'white')

plot(x$distance, d$dE00, xlab = 'Spectral Distance', ylab = expression(~Delta*E['00']), col.axis = 'white', col.lab = 'white', pch = 15, col = cols, cex= 4.5)
text(x$distance, d$dE00, gsub(' ', '\n', x$munsell), cex = 0.66, col = invertLabelColor(cols))
title(sprintf("Mixture: %s + %s", m[1], m[2]), col.main = 'white')

