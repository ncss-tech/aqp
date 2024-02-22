

# philentropy package
# https://search.r-project.org/CRAN/refmans/philentropy/html/00Index.html

# https://en.wikipedia.org/wiki/Bhattacharyya_distance

library(philentropy)

s <- seq(-3, 6, length.out = 100)
x <- dnorm(s, mean = 0, sd = 1)
y <- dnorm(s, mean = 1, sd = 1)
z <- dnorm(s, mean = -0.1, sd = 1)

plot(s, x, type = 'l', xlim = c(-3, 6))
lines(s, y, col = 2)
lines(s, z, col = 3)

# absolute value depends on number of elements in x, y
bhattacharyya(P = x, Q = y, testNA = FALSE, unit = "log2", epsilon = 0.00001)
bhattacharyya(P = x, Q = z, testNA = FALSE, unit = "log2", epsilon = 0.00001)


# can we use this to describe meaningful differences between GHL probability density curves?
# what about depth functions?
# what about spectra?

# TODO: try it out on a couple soil-related things?
library(aqp)


data("munsell.spectra.wide")


x <- munsell.spectra.wide[, '10YR 3/3']
y <- munsell.spectra.wide[, '5YR 4/6']

# useful?
bhattacharyya(P = x, Q = y, testNA = FALSE, unit = "log2", epsilon = 0.00001)


