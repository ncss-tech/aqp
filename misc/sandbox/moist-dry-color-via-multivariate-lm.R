library(aqp)
library(soilDB)
library(farver)

o <- fetchOSD(c('pierre', 'zook', 'cecil', 'amador', 'miami', 'drummer'))


m <- munsell2rgb(o$hue, o$value, o$chroma, returnLAB = TRUE)
d <- munsell2rgb(o$dry_hue, o$dry_value, o$dry_chroma, returnLAB = TRUE)

x <- cbind(m, d)
names(x) <- c('L.m', 'A.m', 'B.m', 'L.d', 'A.d', 'B.d')

mod <- lm(cbind(L.m, A.m, B.m) ~ L.d + A.d + B.d, data = x)


p <- predict(mod)
col2Munsell(p, space = 'CIELAB')

# compare_colour(m[1, ], p[1, ], from_space = 'lab', method = 'cie2000')
