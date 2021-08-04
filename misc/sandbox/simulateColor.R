library(aqp)

data(loafercreek, package = 'soilDB')

# generalize horizon names using REGEX rules
n <- c('Oi', 'A', 'BA','Bt1','Bt2','Bt3','Cr','R')
p <- c('O', '^A$|Ad|Ap|AB','BA$|Bw',
       'Bt1$|^B$','^Bt$|^Bt2$','^Bt3|^Bt4|CBt$|BCt$|2Bt|2CB$|^C$','Cr','R')
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# remove non-matching generalized horizon names
loafercreek$genhz[loafercreek$genhz == 'not-used'] <- NA
loafercreek$genhz <- factor(loafercreek$genhz)

a <- aggregateColor(loafercreek, 'genhz', k = 8)


.simulateColor <- function(a, n = 10) {
 x <- a[['scaled.data']]
 
 res <- lapply(x, function(i) {
   sample(i[['munsell']], size = n, replace = TRUE, prob = i[['weight']])
 })
 
 return(res)
}



.simulateColorFromDE00 <- function(m, n = 10, thresh, hues) {
  # load Munsell LUT
  # safe for CRAN check
  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])
  
  # extract just requested hues
  # along with standard value/chroma pairs found on a typical color book page
  chroma.subset <- c(1, 2, 3, 4, 6, 8)
  x <- munsell[which(munsell$value %in% 3:8 & munsell$chroma %in% chroma.subset & munsell$hue %in% hues), ]
  
  # convert into hex notation for plotting
  x$color <- munsell2rgb(x$hue, x$value, x$chroma)
  x$munsell <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
  
  # re-level hues according to color contrast guidance
  hh <- unique(x$hue)
  ll <- hh[order(huePosition(hh))]
  x$hue <- factor(x$hue, levels=ll)
  
  # setup query color table
  m <- data.frame(
    queryColor = m, 
    parseMunsell(m, convertColors = FALSE),
    stringsAsFactors = FALSE
  )
  m$value <- as.integer(m$value)
  m$chroma <- as.integer(m$chroma)
  
  # compute all pair-wise contrast classes and dE00
  cc <- colorContrast(x$munsell, rep(m$queryColor, times = nrow(x)))
  
  # join for plotting
  z <- merge(x, cc, by.x='munsell', by.y='m1', all.x=TRUE, sort=FALSE)
  
  # dE00 thresholding
  z <- z[which(z$dE00 < thresh), ]
  
  s <- 1 / (1 + (z$dE00))
  p <- s / sum(s)
  
  res <- sample(z$munsell, replace = TRUE, size = n, prob = p)
  
  return(res)
  
}

cols <- .simulateColor(a)

cols <- .simulateColorFromDE00(m = '10YR 4/6', n = 100, thresh = 6, hues = c('2.5Y','10YR', '7.5YR'))

previewColors(parseMunsell(cols), method = 'MDS')





