# library(aqp)

# example data
x <- c(
  'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
  'P2:Ap|AA|E|BhsBhs|Bw1Bw1|CCCCC',
  'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
  'P4:AA|EEE|BhsBhsBhsBhs|BwBw|CCCCC',
  'P5:AAAA|ACACACACAC|CCCCCCCCCCC|CdCdCd'
)

s <- quickSPC(x)

# single match for most profiles
.ex <- grepl('Bt3|Bw', s$name)
s$e <- .ex

a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)

op <- par(no.readonly = TRUE)
par(mar = c(0, 0, 3, 0))
plotSPC(
  s, color = 'e', col.label = 'expression', col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

# highlight selected horizons above and below with brackets
addBracket(
  depths(s, hzID = FALSE)[a, ], 
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)

addBracket(
  depths(s, hzID = FALSE)[b, ], 
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)


# specify numer
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, offset = 1)
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, offset = 1)

plotSPC(
  s, color = 'e', col.label = 'expression', col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

# highlight selected horizons above and below with brackets
addBracket(
  depths(s, hzID = FALSE)[a, ], 
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)

addBracket(
  depths(s, hzID = FALSE)[b, ], 
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)




# multiple matches
.ex <- grepl('B', s$name)
s$e <- .ex

# default
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE)
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE)

par(mfcol = c(1, 2))
plotSPC(
  s,  col.label = 'expression', color = 'e', 
  col.palette = c('grey', 'royalblue'), 
  name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

addBracket(
  depths(s, hzID = FALSE)[a, ], 
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)

addBracket(
  depths(s, hzID = FALSE)[b, ], 
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)

mtext('single = FALSE',  side = 1, line = -1.5, at = 0, adj = -0.5)


# interpret multiple reference hz as a single reference hz
a <- hzAbove(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)
b <- hzBelow(s, .ex, SPC = FALSE, simplify = TRUE, single = TRUE)


plotSPC(
  s, col.label = 'expression', color = 'e', 
  col.palette = c('grey', 'royalblue'), name = 'name', 
  hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75
)

addBracket(
  depths(s, hzID = FALSE)[a, ], 
  offset = -0.3, col = 'darkgreen', tick.length = 0, lwd = 3
)

addBracket(
  depths(s, hzID = FALSE)[b, ],
  offset = -0.35, col = 'firebrick', tick.length = 0, lwd = 3
)

mtext('single = TRUE',  side = 1, line = -1.5, at = 0, adj = -0.5)


# demonstrate SPC = TRUE, single = TRUE
plotSPC(
  s, col.label = 'expression', color = 'e', 
  col.palette = c('grey', 'royalblue'), name = 'name', 
  hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75, max.depth = 250
)

a <- hzAbove(s, .ex, SPC = TRUE, single = TRUE)

plotSPC(
  a, name = 'name', hz.depths = TRUE, depth.axis = FALSE, 
  name.style = 'center-center', cex.names = 0.75, max.depth = 250
)
title('selected profiles/horizons')

par(op)




