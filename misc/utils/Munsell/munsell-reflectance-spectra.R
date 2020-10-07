library(aqp)
library(soilDB)
library(reshape2)
library(latticeExtra)

## http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt

# https://github.com/ncss-tech/aqp/issues/101

# address in the ST forum as well:
# https://soiltxnmyforum.cals.vt.edu/forum/read.php?3,1984,1987#msg-1987


# calculation here
# https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf

# weighted geometric mean
# https://en.wikipedia.org/wiki/Weighted_geometric_mean
wgm <- function(v, w) {
  r <- sum(w * log(v)) / sum(w)
  r <- exp(r)
  return(r)
}

wgm(v = c(0.5, 0.8), w = c(0.5, 0.5))



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


spectralMixture <- function(m1, m2, w1 = 0.5, w2 = 0.5) {
  
  ## TODO: load reference spectra from local data file
  
  ## TODO: generalize to more than 2 colors
  
  # subset reference spectra for colors
  idx <- which(m.rel$munsell %in% c(m1, m2))
  s <- m.rel[idx, ]
  s$color <- parseMunsell(s$munsell)
  
  # long -> wide
  s.wide <- dcast(s, munsell ~ wavelength, value.var = 'reflectance')
  
  # spectra as vectors
  # columns are wavelength
  s.1 <- s.wide[1, -1]
  s.2 <- s.wide[2, -1]
  
  # prepare weights
  wts <- c(w1, w2)
  
  # empty vector for mixture
  mixed <- vector(mode = 'numeric', length = length(s.1[1, ]))
  
  # iterate over wavelength (columns in first spectra)
  for(i in seq_along(s.1)) {
    
    # prepare values
    vals <- c(
      s.1[1, i],
      s.2[1, i]
    )
    
    # mix via weighted geometric mean
    mixed[i] <- wgm( v = vals, w = wts )
  }
  
  
  ## TODO: maybe store in this form
  # make a matrix of reference spectra
  reference <- dcast(m.rel, wavelength ~ munsell, value.var = 'reflectance')
  
  # subtract the mixture spectra, element-wise, from reference library
  # note we are removing the wavelength column
  m.diff <- sweep(reference[, -1], MARGIN = 1, STATS = mixed, FUN = '-')
  
  # euclidean distance is sufficient
  # D = sqrt(sum(reference - mixed))
  m.dist <- sqrt(colSums(m.diff^2))
  
  # get the spectra of the closest munsell chip
  m.match <- sort(m.dist)[1]
  
  # compile into data.frame
  res <- data.frame(
    munsell = names(m.match),
    distance = m.match,
    stringsAsFactors = FALSE
  )
  
  
  # sub set reference library for plotting: source colors and the mixture
  idx <- which(m.rel$munsell %in% c(m1, m2, res$munsell))
  s <- m.rel[idx, ]
  s$color <- parseMunsell(s$munsell)
  
  
  ## use a unique ID
  ## this will break when the mixed color = m1 or m2
  s$munsell <- factor(s$munsell, levels = c(m1, m2, res$munsell))
  
  cols <- parseMunsell(c(m1, m2, res$munsell))
  tps <- list(superpose.line=list(col=cols, lwd=5, lty = c(1, 1, 4)))
  
  print(
  xyplot(reflectance ~ wavelength, groups=munsell, data=s, 
         type=c('l', 'g'),
         scales=list(tick.number=10),
         auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right'),
         par.settings=tps
  )
  )

  return(res)
}


spectralMixture('10YR 4/6', '5YR 2/2')
spectralMixture('10YR 4/6', '5YR 2/2', w1 = 0.8, w2 = 0.2)

spectralMixture('10YR 4/6', '2.5Y 5/4')

spectralMixture('10YR 6/6', '5P 5/4')

spectralMixture('10YR 4/4', '5GY 5/4')



d <- cbind(
  parseMunsell(c('10YR 4/6', '5YR 2/2'), convertColors=FALSE),
  parseMunsell(c('10YR 4/6', '5YR 2/2'), return_triplets=TRUE, returnLAB=TRUE),
  pct=c(0.5, 0.5),
  col=parseMunsell(c('10YR 4/6', '5YR 2/2'), convertColors=TRUE)
)

estimateColorMixture(d, backTransform = TRUE)



## investigate slices -- can interpolate reflectance vs chroma (by wavelengths) for odd chroma

idx <- which(m.rel$hue %in% c('7.5YR') & m.rel$value == 4)
s <- m.rel[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales=list(tick.number=10),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right')
)




