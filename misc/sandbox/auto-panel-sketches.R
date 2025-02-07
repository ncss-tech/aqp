library(soilDB)
library(aqp)

x <- fetchNASIS()

x <- HzDepthLogicSubset(x)

sdc <- getSoilDepthClass(x)
site(x) <- sdc

z <- subset(x, depth > 40)
z <- subset(z, !is.na(soil_color))

z$texcl <- factor(z$texcl, levels = SoilTextureLevels())

# z <- combine(lapply(1:216, random_profile, SPC = TRUE))



# suggestions for even numbers
.s <- 1:length(z)
.fidx <- which(length(z) %% .s == 0)
.s[.fidx]


.size <- 72
z$.chunk <- makeChunks(seq_along(z), size = .size)
.chunkIds <- unique(z$.chunk)
.n <- length(.chunkIds)

## TODO: this doesn't work for thematic sketches, as each panel gets its own legend

par(mar = c(0, 0, 0, 0), mfrow = c(.n, 1))

for(i in .chunkIds) {
  .idx <- which(z$.chunk == i)
  plotSPC(
    z[.idx, ], 
    print.id = FALSE, 
    name = NA, 
    lwd = 0.1,
    divide.hz = FALSE, 
    width = 0.4, 
    max.depth = 150, 
    depth.axis = FALSE,
    n = .size,
    # color = 'texcl'
    # color = 'p1'
  )
}

