library(aqp)
library(reshape2)
library(latticeExtra)

## http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt
# missing odd chroma
x <- read.table('SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt.gz', skip=13, header=TRUE, stringsAsFactors = FALSE, sep=',')

# long format simpler to work with
m <- melt(x, id.vars = 'Name')

# remove leading 'X' from wavelength
m$variable <- as.character(m$variable)
m$v <- as.numeric(gsub(pattern = 'X', replacement = '', x = m$variable))

# subset columns and re-name
m <- m[, c('Name', 'v', 'value')]
names(m) <- c('munsell', 'wavelength', 'reflectance')

# this is clever
# split into Munsell pieces
d <- strcapture(
  '([[[:digit:][:alpha:].]+)([[:digit:]]+)/([[:digit:]]+)', 
  m$munsell, 
  proto = data.frame(hue=character(), value=integer(), chroma=integer(), stringsAsFactors = FALSE)
)

# check: some funky ones in there:
# 10Y8.
# 7.5Y8.
table(d$hue)

# OK
table(d$value)
table(d$chroma)


# re-assemble
m.rel <- data.frame(
  munsell=sprintf("%s %s/%s", d$hue, d$value, d$chroma),
  hue=d$hue, 
  value=d$value, 
  chroma=d$chroma, 
  wavelength=m$wavelength, 
  reflectance=m$reflectance, 
  stringsAsFactors = FALSE
  )

# sort
m.rel <- m.rel[order(m.rel$hue, m.rel$value, m.rel$chroma, m.rel$wavelength), ]


# subset
idx <- which(m.rel$munsell %in% c('10YR 3/4', '5YR 4/6'))
s <- m.rel[idx, ]

idx <- which(m.rel$munsell %in% c('10YR 3/1', '10YR 3/2', '10YR 3/4', '10YR 3/6'))
s <- m.rel[idx, ]


cols <- parseMunsell(levels(factor(s$munsell)))
tps <- list(superpose.line=list(col=cols, lwd=5))

xyplot(reflectance ~ wavelength, groups=munsell, data=s, 
       type=c('l', 'g'),
       scales=list(tick.number=10),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right'),
       par.settings=tps
)



## investigate slices -- can interpolate reflectance vs chroma (by wavelengths) for odd chroma

idx <- which(m.rel$hue %in% c('10YR') & m.rel$value == 4)
s <- m.rel[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales=list(tick.number=10),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right')
)




