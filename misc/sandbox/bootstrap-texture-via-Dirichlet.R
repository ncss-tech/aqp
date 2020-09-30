library(aqp)
library(soilDB)
library(compositions)
library(soiltexture)

data("loafercreek")

table(loafercreek$genhz)

h <- horizons(loafercreek)
h <- h[which(h$genhz == 'Bt1'), ]

ssc <- dplyr::select(
  h,
  SAND = sand,
  SILT = silt,
  CLAY = clay
)

ssc <- na.omit(ssc)

ssc <- ssc[sample(1:nrow(ssc), size = 15), ]

# convert to a closed / proportional composition object
z <- acomp(ssc, total = 100)

# fit 3-term alpha parameters of Dirichlet distribution
D <- fitDirichlet(z)

# safely compute compositional mean / variance-covariance
mean.comp <- meanCol(z)
var.comp <- compositions::var(z, robust = FALSE, method = 'pearson')

s <- rDirichlet.acomp(250, D$alpha)

# convert back to format that is suitable for plotting on the TT
s <- as.data.frame(unclass(s) * 100)
names(s) <- names(mean.comp)


TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",    # use "our" texture triangle
  main= "Soil Textures",          # title
  tri.sum.tst=FALSE,            # do not test for exact sum(sand, silt, clay) == 100
  cex.lab=0.75,                 # scaling of label text
  cex.axis=0.75,                # scaling of axis
  frame.bg.col='white',         # background color
  class.lab.col='black',        # color for texture class labels
  lwd.axis=1.5,                    # line thickness for axis
  arrows.show=TRUE
)

# full samples, these will extend beyond original class limits
TT.points(tri.data = ssc, geo = TT, col='darkgreen', pch = 15, cex = 1, tri.sum.tst = FALSE)
TT.points(tri.data = s, geo = TT, col='royalblue', pch = 0, cex = 1, lwd = 1)


# not useful
textureTriangleSummary(ssc, sim = TRUE, sim.n = 1000)

# bootstrapped version
textureTriangleSummary(s)




