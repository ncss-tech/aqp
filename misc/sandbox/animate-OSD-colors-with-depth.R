library(aqp)
library(farver)
library(ggplot2)
library(gganimate)
library(lattice)

## use entire OSD color DB
## convert colors, slice
## keep only unique combinations of colors / slice
## to big? iterate over slices
## animate transitions in CIELAB or MDS(L,A,B) by slice


tf <- tempfile()
download.file('https://github.com/ncss-tech/soil-color-book/raw/master/data/osd_colors-5cm.csv.gz', destfile = tf)
x.1 <- read.csv(tf, stringsAsFactors = FALSE)

tf <- tempfile()
download.file('https://github.com/ncss-tech/soil-color-book/raw/master/data/osd_colors-25cm.csv.gz', destfile = tf)
x.2 <- read.csv(tf, stringsAsFactors = FALSE)

tf <- tempfile()
download.file('https://github.com/ncss-tech/soil-color-book/raw/master/data/osd_colors-50cm.csv.gz', destfile = tf)
x.3 <- read.csv(tf, stringsAsFactors = FALSE)

tf <- tempfile()
download.file('https://github.com/ncss-tech/soil-color-book/raw/master/data/osd_colors-100cm.csv.gz', destfile = tf)
x.4 <- read.csv(tf, stringsAsFactors = FALSE)

x <- rbind(x.1, x.2, x.4)

str(x)
head(x)

x.rgb <- data.frame(slice=x$top, col=munsell2rgb(x$m_hue, x$m_value, x$m_chroma, return_triplets = TRUE))
x.rgb <- unique(na.omit(x.rgb))

x.lab <- convertColor(x.rgb[, -1], from = 'sRGB', to = 'Lab', from.ref.white = 'D65', to.ref.white = 'D65', clip = FALSE)


d <- compare_colour(x.lab, x.lab, from_space = 'lab', to_space = 'lab', method='CIE2000')

mds <- cmdscale(d)
mds <- data.frame(slice=factor(x.rgb$slice), mds, col=rgb(x.rgb[, -1], maxColorValue = 1), stringsAsFactors = FALSE)

par(mar=c(1,1,1,1))
plot(X2 ~ X1, data=mds, type='n', axes=FALSE, xlab='', ylab='')
points(X2 ~ X1, data=mds, pch=15, col=mds$col)

# points(X2 ~ X1, data=mds, pch=15, col=mds$col, subset=slice == 5)
# points(X2 ~ X1, data=mds, pch=15, col=mds$col, subset=slice == 25)

## why are the colors arranged in linear bands?

# ... not quite right...

ggplot(mds, aes(X1, X2, colour = col)) +
  geom_point(pch=15, show.legend = FALSE) +
  scale_colour_identity() +
  # Here comes the gganimate specific bits
  labs(title = 'slice: {closest_state}') +
  transition_states(slice) +
  ease_aes('sine-in-out')


