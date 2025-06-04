##
##
##

library(purrr)
devtools::load_all()

data("munsell.spectra.wide")

nm <- names(munsell.spectra.wide)[-1]
s <- sample(nm, size = 100)

z <- do.call(
  'rbind',
  map(s, .progress = TRUE, .f = function(i) {
    spec2Munsell(munsell.spectra.wide[, i])
  })
)

z$m <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

# compare
cc <- colorContrast(
  m1 = s,
  m2 = z$m
)

hist(cc$dE00, breaks = 25)


## TODO: investigate these
# 2026-06-04: much lower dE00 than before, after truncating reflectance to max of 1
x <- cc[order(cc$dE00, decreasing = TRUE)[1:10], ]

x


#
.m <- '7.5RP 9/9'
plot(munsell.spectra.wide[, 1], munsell.spectra.wide[, .m], type = 'b', las = 1, main = .m, ylim = c(0, 1.2))
spec2Munsell(munsell.spectra.wide[, .m])

#
.m <- '7.5RP 9/9'
plot(munsell.spectra.wide[, 1], munsell.spectra.wide[, .m], type = 'b', las = 1, main = .m, ylim = c(0, 1.2))
lines(munsell.spectra.wide[, 1], munsell.spectra.wide[, '7.5RP 8/6'], type = 'b', col = 2)
lines(munsell.spectra.wide[, 1], munsell.spectra.wide[, '7.5RP 9/6'], type = 'b', col = 2)
spec2Munsell(munsell.spectra.wide[, .m])
