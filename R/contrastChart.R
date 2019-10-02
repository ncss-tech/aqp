
# m: color to compare e.g. '10YR 4/3'
# hues: vector of Munsell hue pages to display 
contrastChart <- function(m, hues) {
  
  # load Munsell LUT
  # safe for CRAN check
  munsell <- NULL
  load(system.file("data/munsell.rda", package = "aqp")[1])
  
  # extract just those hues we are working with, standard value/chroma pairs
  x <- munsell[which(munsell$value %in% 3:8 & munsell$chroma %in% c(1,2,3,4,6,8) & munsell$hue %in% hues), ]
  
  # convert into hex notation for plotting
  x$color <- munsell2rgb(x$hue, x$value, x$chroma)
  x$munsell <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
  
  # re-level hues according to color contrast guidance
  hh <- unique(x$hue)
  ll <- hh[order(huePosition(hh))]
  x$hue <- factor(x$hue, levels=ll)
  
  # setup query color table
  m <- data.frame(queryColor=m, parseMunsell(m, convertColors = FALSE), stringsAsFactors = FALSE)
  m$value <- as.integer(m$value)
  m$chroma <- as.integer(m$chroma)
  
  # compute all pair-wise constrast classes and dE00
  cc <- colorContrast(x$munsell, rep(m$queryColor, times=nrow(x)))
  
  # join for plotting
  z <- merge(x, cc, by.x='munsell', by.y='m1', all.x=TRUE)
  
  # make plot
  pp <- xyplot(value ~ chroma | hue, data=z,
               main=sprintf('Color Contrast Chart: %s', m$queryColor),
               asp=1, xlab='Chroma', ylab='Value',
               xlim=c(0.75, 8.25), ylim=c(2.75, 8.25), 
               scales=list(alternating=1, tick.number=8, relation='free', y=list(rot=0)),
               as.table=TRUE, strip=strip.custom(bg='grey'),
               subscripts=TRUE,
               panel=function(xx, yy, subscripts, ...) {
                 # prep data for this panel
                 d <- z[subscripts, ]
                 d$cc <- as.character(d$cc)
                 d$dE00 <- format(d$dE00, digits = 2)
                 
                 # remove query color contrast and dE00
                 idx <- which(d$munsell == m$queryColor)
                 d$cc[idx] <- ''
                 d$dE00[idx] <- ''
                 
                 # # grid system
                 # panel.abline(h = 3:8, v=1:8, col=grey(0.85), lty=1)
                 
                 # offsets, may require additional tinkering
                 bd.side <- 0.3
                 bd.bottom <- 0.2
                 bd.top <- 0.4
                 bd.annot <- 0.05
                 
                 # color chips
                 # border encodes query chip
                 chip.border <- rep('black', times=nrow(d))
                 chip.lwd <- rep(1, times=nrow(d))
                 
                 # update border colors and thickness for the query color
                 border.idx <- which(d$hue == m$hue & d$value == m$value & d$chroma == m$chroma)
                 chip.border[border.idx] <- 'red'
                 chip.lwd[border.idx] <- 3
                 
                 panel.rect(
                   xleft=xx - bd.side, 
                   ybottom=yy - bd.bottom, 
                   xright=xx + bd.side, 
                   ytop=yy + bd.top, 
                   col=d$color,
                   border=chip.border,
                   lwd=chip.lwd
                 )
                 
                 # annotate contrast class
                 panel.text(
                   xx, 
                   yy - (bd.bottom + 0.08), 
                   as.character(d$cc), 
                   cex=0.6,
                   font=3
                 )
                 
                 # annotate dE00
                 panel.text(
                   xx, 
                   yy - (bd.bottom + 0.25), 
                   d$dE00, 
                   cex=0.66
                 )
               }
  )
  
  return(pp)
  
}




