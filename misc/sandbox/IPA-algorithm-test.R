
## testing IPA concept
# https://iopscience.iop.org/article/10.1088/1748-9326/abbb00/pdf
# https://www.publish.csiro.au/sr/pdf/SR9760291

## is that latest version correct?


library(aqp)
library(soilDB)

x <- fetchKSSL('musick')

x$IPA <- profileApply(x, simplify = TRUE, function(i) {
  
  var(i$sand, na.rm = TRUE) / mean(i$sand, na.rm = TRUE)
  
})

plotSPC(x, color = 'sand')

o <- order(x$IPA)
plotSPC(x, color = 'sand', plot.order = o)
text(1:length(x), 350, round(x$IPA, 2)[o])





