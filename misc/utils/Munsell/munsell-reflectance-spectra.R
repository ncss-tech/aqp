library(aqp)
library(reshape2)
library(latticeExtra)

## http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt

# https://github.com/ncss-tech/aqp/issues/101

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

## very slow, but implements basic idea
mixIt <- function(m1, m2, w1 = 1, w2 = 1, mult = 3) {
  # subset
  idx <- which(m.rel$munsell %in% c(m1, m2))
  s <- m.rel[idx, ]
  s$color <- parseMunsell(s$munsell)
  
  
  # cols <- parseMunsell(levels(factor(s$munsell)))
  # tps <- list(superpose.line=list(col=cols, lwd=5))
  # 
  # print(
  #   xyplot(reflectance ~ wavelength, groups=munsell, data=s, 
  #          type=c('l', 'g'),
  #          scales=list(tick.number=10),
  #          auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right'),
  #          par.settings=tps
  #   )
  # )
  # 
  
  # long -> wide
  s.wide <- dcast(s, munsell ~ wavelength, value.var = 'reflectance')
  
  # mix colors by multiplying spectra (simulate subtractive mixture)
  mixed <- (s.wide[1, -1] * w1) * (s.wide[2, -1] * w2) * mult
  
  z <- split(m.rel, m.rel$munsell)
  
  zz <- lapply(z, function(i) {
    i.dist <- sqrt(sum((i$reflectance - mixed)^2))
    
    res <- data.frame(
      munsell = i$munsell[1],
      distance = i.dist,
      stringsAsFactors = FALSE
    )
    
    return(res)
  })
  
  zz <- do.call('rbind', zz)
  
  new.color <- zz$munsell[order(zz$distance)[1]]
  
  idx <- which(m.rel$munsell %in% c(m1, m2, new.color))
  s <- m.rel[idx, ]
  s$color <- parseMunsell(s$munsell)
  
  s$munsell <- factor(s$munsell, levels = c(m1, m2, new.color))
  
  cols <- parseMunsell(c(m1, m2, new.color))
  tps <- list(superpose.line=list(col=cols, lwd=5, lty = c(1, 1, 4)))
  
  print(
  xyplot(reflectance ~ wavelength, groups=munsell, data=s, 
         type=c('l', 'g'),
         scales=list(tick.number=10),
         auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right'),
         par.settings=tps
  )
  )
  # return(mixed)
}


mixIt('10YR 4/6', '10G 2/2')
mixIt('10YR 4/6', '10G 2/2', mult = 8)




## investigate slices -- can interpolate reflectance vs chroma (by wavelengths) for odd chroma

idx <- which(m.rel$hue %in% c('7.5YR') & m.rel$value == 4)
s <- m.rel[idx, ]

xyplot(reflectance ~ chroma | factor(wavelength), data=s, 
       type='b', as.table=TRUE,
       scales=list(tick.number=10),
       auto.key=list(lines=TRUE, points=FALSE, cex=1, space='right')
)




