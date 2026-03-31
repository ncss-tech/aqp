

par(mar = c(0, 0, 0, 0), mfrow = c(2, 1))

.labs <- formatMunsell('N', 1:9, chroma = 0)
.cols <- parseMunsell(.labs)

soilPalette(.cols, lab = .labs)

.labs <- formatMunsell('10YR', 1:9, chroma = 1)
.cols <- parseMunsell(.labs)

soilPalette(.cols, lab = .labs)

