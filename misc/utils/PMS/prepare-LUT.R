library(aqp)


## TODO: the following creates the Pantone -> Munsell LUT
##       this isn't the same as the Munsell -> Pantone LUT (entries currently missing)


# https://github.com/ncss-tech/aqp/issues/124

# sourced from:
# https://raw.githubusercontent.com/ajesma/Pantoner/gh-pages/csv/pantone-coated.csv
# https://github.com/ajesma/Pantoner/raw/gh-pages/csv/pantone-uncoated.csv
x.coated <- read.csv('pantone-coated.csv', stringsAsFactors = FALSE)
x.uncoated <- read.csv('pantone-uncoated.csv', stringsAsFactors = FALSE)

# OK
head(x.coated)
head(x.uncoated)

# stack, names can be used 
x <- rbind(x.coated, x.uncoated)
str(x)
head(x)

# convert
m <- rgb2munsell(t(col2rgb(x$hex)) / 255)

# merge to make the combined lookup table
z <- cbind(x, m)
str(z)

# re-name
names(z) <- c('code', 'hex', 'hue', 'value', 'chroma', 'dE00')

# condense Munsell notation
z$munsell <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)

# subset
z <- z[, c('code', 'hex', 'munsell', 'dE00')]
str(z)

# re-name and save
pms.munsell.lut <- z

# save
save(pms.munsell.lut, file = '../../../data/pms.munsell.lut.rda')
