devtools::load_all()

library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)
library(dendextend)
library(bench)



# start fresh
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

# eval dissimilarity:
# using Ex-Ca:Mg and CEC at pH 7
# with no depth-weighting (k=0)
# to a maximum depth of 40 cm
d1 <- profile_compare(sp4, vars = c('Ca', 'CEC_7'), k = 0, max_d = 40)

d2 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40)

d3 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, weights = c(1, 5))

d4 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, rescaleResult = TRUE)

par(mfrow = c(2, 1), mar = c(0, 0, 3, 0))

plotProfileDendrogram(sp4, diana(d1), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'Ca', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

plotProfileDendrogram(sp4, diana(d2), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'Ca', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

tanglegram(
  dendextend::rotate(hclust(d1), order = profile_id(sp4)), 
  dendextend::rotate(hclust(d2), order = profile_id(sp4))
  )


dev.off()

# rescaled D
plotProfileDendrogram(sp4, diana(d4), width = 0.3, color = 'Ca', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)



par(mfrow = c(2, 1), mar = c(0, 0, 3, 0))

plotProfileDendrogram(sp4, diana(d2), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'CEC_7', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

plotProfileDendrogram(sp4, diana(d3), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'CEC_7', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)


tanglegram(
  dendextend::rotate(as.hclust(diana(d2)), order = profile_id(sp4)), 
  dendextend::rotate(as.hclust(diana(d3)), order = profile_id(sp4))
)




## single missing values
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

d1 <- profile_compare(sp4, vars = c('Ca', 'K'), k = 0, max_d = 40)

d2 <- NCSP(sp4, vars = c('Ca', 'K'), k = 0, maxDepth = 40)


par(mfrow = c(2, 1), mar = c(0, 0, 3, 0))

plotProfileDendrogram(sp4, diana(d1), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'K', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

plotProfileDendrogram(sp4, diana(d2), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'K', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

tanglegram(
  dendextend::rotate(hclust(d1), order = profile_id(sp4)), 
  dendextend::rotate(hclust(d2), order = profile_id(sp4))
)



## single  values
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

d1 <- profile_compare(sp4, vars = c('Ca', 'Ca'), k = 0, max_d = 40)

d2 <- NCSP(sp4, vars = c('Ca'), k = 0, maxDepth = 40)


par(mfrow = c(2, 1), mar = c(0, 0, 3, 0))

plotProfileDendrogram(sp4, diana(d1), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'Ca', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

plotProfileDendrogram(sp4, diana(d2), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'Ca', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

tanglegram(
  dendextend::rotate(hclust(d1), order = profile_id(sp4)), 
  dendextend::rotate(hclust(d2), order = profile_id(sp4))
)



## profile corruption / dice()
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

sp4$top[2] <- sp4$top[1]

d5 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, weights = c(1, 5))

## this won't work, colusa is missing
# plotProfileDendrogram(sp4, diana(d4), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'CEC_7', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

# keep track of subset profiles
attributes(d5)



## test color
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')

# get these soil series
s <- fetchOSD(s.list)

# manually convert Munsell -> sRGB
.lab <- munsell2rgb(s$hue, s$value, s$chroma, returnLAB = TRUE)
s$L <- .lab$L
s$A <- .lab$A
s$B <- .lab$B


d1 <- NCSP(s, vars = c('L', 'A', 'B'), k = 0, maxDepth = 150, rescaleResult = TRUE)

d2 <- NCSP(s, vars = c('L', 'A', 'B'), k = 0, isColor = TRUE, maxDepth = 150, rescaleResult = TRUE)

# subset
s.sub <- subset(s, profile_id(s) %in% attr(d1, 'Labels'))

tanglegram(
  dendextend::rotate(hclust(d1), order = profile_id(s.sub)), 
  dendextend::rotate(hclust(d2), order = profile_id(s.sub))
)


par(mfrow = c(2, 1), mar = c(0, 0, 3, 0))

plotProfileDendrogram(s.sub, dendextend::rotate(hclust(d1), order = profile_id(s.sub)), scaling.factor = 0.005, width = 0.3, color = 'soil_color', name.style = 'center-center', plot.depth.axis = FALSE, rotateToProfileID = TRUE, name = NA)

plotProfileDendrogram(s.sub, dendextend::rotate(hclust(d2), order = profile_id(s.sub)), scaling.factor = 0.005, width = 0.3, color = 'soil_color', name.style = 'center-center', plot.depth.axis = FALSE, rotateToProfileID = TRUE, name = NA)




x <- lapply(1:1000, random_profile, n = c(6, 7, 8), n_prop = 5, method = 'LPP', SPC = TRUE)
x <- combine(x)

v <-  c('p1', 'p2', 'p3', 'p4')

# ~ 12 seconds
system.time(d1 <- profile_compare(x, vars = v, k = 0, max_d = 100))

# ~ 13 seconds
system.time(d2 <- NCSP(x, vars = v, k = 0, maxDepth = 100))

# bench::mark(
#   # pc = profile_compare(x, vars = v, k = 0, max_d = 100),
#   NCSP = NCSP(x, vars = v, k = 0, maxDepth = 100)
# )








## testing


m <- data.frame(
  L = c(20, 25, 10),
  A = c(5, 10, 15),
  B = c(8, 16, -5)
)

row.names(m) <- letters[1:nrow(m)]

sm <- rep(TRUE, times = nrow(m))


farver::compare_colour(m, m, from_space = 'lab', to_space = 'lab', method = 'CIE2000', white_from = 'D65')


(d1 <- aqp:::.NCSP_distanceCalc(m, sm = sm, isColor = FALSE))

(d2 <- aqp:::.NCSP_distanceCalc(m, sm = sm, isColor = TRUE))


attributes(d1)
attributes(d2)



##



vars <- c('ex_Ca_to_Mg', 'CEC_7')
m <- dice(sp4, fm = 30 ~ ex_Ca_to_Mg + CEC_7, SPC = FALSE)
m <- m[, vars, drop = FALSE]

# variable weights
w <- c(1, 10)

# create soilmatrix row
sm <- rep(TRUE, times = nrow(m))
sm[which(is.na(m[, 1]))] <- FALSE

# add NA not related to soil/non-soil eval
# all characteristics -> NA distances
# m[1, 1:2] <- NA

# single characteristic -> distance still possible, but limited
m[1, 1] <- NA

(d <- aqp:::.NCSP_distanceCalc(m, sm = sm))
(dw <- aqp:::.NCSP_distanceCalc(m, sm = sm, w = w))

cor(d, dw)
hist(d - dw)


plot(as.phylo(hclust(d)))
