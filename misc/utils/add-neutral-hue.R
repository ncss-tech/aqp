library(aqp)
data(munsell)

# get a copy of the original table
m <- munsell

# add records for neutral hues
hue <- 'N'
value <- c(2, 3, 4, 5, 6, 7, 8)
chroma <- 0

N <- data.frame(hue=hue, value=value, chroma=chroma, stringsAsFactors = FALSE)

# estimate colors via grey()
greys <- as.data.frame(t(col2rgb(grey(value/10))) / 255)
names(greys) <- c('r', 'g', 'b')

N <- cbind(N, greys)

# check
with(N, plot(value ~ chroma, col=rgb(r, g, b), pch=15, cex=5, axes=FALSE))

# add at bottom
munsell <- rbind(m, N)

# save
save(munsell, file='munsell.rda')
