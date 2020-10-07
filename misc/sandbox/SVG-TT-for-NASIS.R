library(soiltexture)


svg(filename = 'TT.svg', width = 8, height = 8, pointsize = 12, family = 'sans', bg = NA)

# empty soil texture triangle
TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",
  main= "",
  tri.sum.tst=FALSE,
  cex.lab=0.75,
  cex.axis=0.75,
  frame.bg.col='white',
  class.lab.col='black',
  lwd.axis=1.5,
  arrows.show=TRUE,
  new.mar = c(3, 0, 0, 0)
)

dev.off()

