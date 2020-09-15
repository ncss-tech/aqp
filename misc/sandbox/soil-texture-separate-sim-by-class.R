library(aqp)
library(compositions)

library(soiltexture)
library(scales)

library(DBI)
library(RSQLite)

# connect
db <- dbConnect(RSQLite::SQLite(), 'E:/NASIS-KSSL-LDM/LDM/LDM-compact.sqlite')

# list tables
dbListTables(db)

# list fields in physical table
dbListFields(db, 'physical')

qq <- "
SELECT
sand_total AS sand, silt_total AS silt, clay_total AS clay
FROM layer
JOIN physical ON layer.labsampnum = physical.labsampnum
;"

# run query
x <- dbGetQuery(db, qq)

# close connection
dbDisconnect(db)


# extract components of texture, removing rows with missing data
ssc <- x[, c('sand', 'silt', 'clay')]
ssc <- na.omit(ssc)

# adjust names for plotting with TT.plot()
# names must be SAND, SILT, CLAY
names(ssc) <- toupper(names(ssc))

# test of bogus data
ssc$sum <- rowSums(ssc[, c('SAND', 'SILT', 'CLAY')])
# > 5% deviation from 100%
idx <- which(abs(ssc$sum - 100) > 5)

# check errors: 467 ~ 0.1%
(length(idx) / nrow(ssc)) * 100

# remove errors
z <- ssc[-idx, ]


# negative proportions: 37 cases
idx <- which(
  z$SAND < 0 | z$SILT < 0 | z$CLAY < 0
)

z <- z[-idx, ]

# texture class
z$class <- ssc_to_texcl(sand = z$SAND, clay = z$CLAY)
table(z$class)

cl <- split(z, z$class)

i <- cl[['l']]

i.comp <- acomp(i[, 1:3], total = 100)
m.comp <- meanCol(i.comp)
v.comp <- compositions::var(i.comp, robust = FALSE, method = 'pearson')

s <- rnorm.acomp(n = 1000, mean = m.comp, var = v.comp)
ssc.i <- as.data.frame(unclass(s) * 100)

ssc.i$texture <- ssc_to_texcl(ssc.i$SAND, ssc.i$CLAY)

ssc.i.trunc <- ssc.i[as.character(ssc.i$texture) == as.character(i$class[1]), ]


zz <- soiltexture$values[soiltexture$values$texcl == as.character(i$class[1]), ]
names(zz) <- c('SAND', 'SILT', 'CLAY')


TT <- TT.plot(
  class.sys= "USDA-NCSS.TT",    # use "our" texture triangle
  tri.data=ssc.i,                 # data.frame with sand, silt, clay values
  main= "Soil Textures",          # title
  tri.sum.tst=FALSE,            # do not test for exact sum(sand, silt, clay) == 100
  cex.lab=0.75,                 # scaling of label text
  cex.axis=0.75,                # scaling of axis
  cex=0.5,                      # scaling of point symbols
  col=alpha('royalblue', 0.125),  # color of point symbols, with transparency
  frame.bg.col='white',         # background color
  class.lab.col='black',        # color for texture class labels
  lwd.axis=1.5,                    # line thickness for axis
  arrows.show=TRUE
)

TT.points(tri.data = ssc.i.trunc, geo = TT, col=alpha('firebrick', 0.25), pch = 15, cex = 0.5)


TT.points(tri.data = zz, geo = TT, col='black', pch = '.', cex = 0.01)


## what?
texcl_to_ssc('s', sample = 100)


# zz <- ssc.i
# names(zz) <- tolower(names(zz))
# 
# textureTriangleSummary(zz[, 1:3], sim = TRUE)


# 
# # plot data
# # note that there are many arguments used to ajust style
# TT.plot(
#   class.sys= "USDA-NCSS.TT",    # use "our" texture triangle
#   tri.data=z,                 # data.frame with sand, silt, clay values
#   main= "Soil Textures",          # title
#   tri.sum.tst=FALSE,            # do not test for exact sum(sand, silt, clay) == 100
#   cex.lab=0.75,                 # scaling of label text
#   cex.axis=0.75,                # scaling of axis
#   cex=0.5,                      # scaling of point symbols
#   col=alpha('royalblue', 0.125),  # color of point symbols, with transparency
#   frame.bg.col='white',         # background color
#   class.lab.col='black',        # color for texture class labels
#   lwd.axis=1.5,                    # line thickness for axis
#   arrows.show=TRUE
# )
# 
# 
