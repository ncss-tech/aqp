library(aqp)
library(microbenchmark)
library(daff)

load(system.file("data/munsell.rda", package="aqp")[1])


x <- munsell2rgb(munsell$hue, munsell$value, munsell$chroma, return_triplets = TRUE)
y <- munsell2rgb2(munsell$hue, munsell$value, munsell$chroma, return_triplets = TRUE)

all.equal(x, y)
d <- diff_data(x, y)
render_diff(d)



microbenchmark(
  join = munsell2rgb(munsell$hue, munsell$value, munsell$chroma),
  merge = munsell2rgb2(munsell$hue, munsell$value, munsell$chroma)
  )


