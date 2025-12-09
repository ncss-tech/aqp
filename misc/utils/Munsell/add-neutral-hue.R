##
##
##


## 2025-12-05
## no longer using this approximation
## TODO: compare this with direct measurement of color chips

stop()

# library(aqp)
# data(munsell)
# 
# # get a copy of the original table
# m <- munsell
# 
# # add records for neutral hues
# hue <- 'N'
# value <- c(2, 2.5, 3, 4, 5, 6, 7, 8, 8.5, 9, 9.5)
# chroma <- 0
# 
# N <- data.frame(
#   hue = hue, 
#   value = value, 
#   chroma = chroma
# )
# 
# # estimate colors via grey()
# greys <- as.data.frame(t(col2rgb(grey(value/10))) / 255)
# names(greys) <- c('r', 'g', 'b')
# 
# N <- cbind(N, greys)
# 
# # check
# with(N, plot(value ~ chroma, col = rgb(r, g, b), pch = 15, cex = 5, axes = FALSE))
# 
# # add at bottom
# munsell <- rbind(m, N)
# 
# # save
# save(munsell, file = 'munsell.rda')
