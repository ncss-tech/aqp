library(reshape2)

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

# save
saveRDS(m.rel, file = 'simplified-Munsell-spectra.rds')

