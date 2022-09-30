devtools::load_all()

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

d3 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, var.wt = c(1, 5))

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




## profile corruption / dice()
data(sp4)
depths(sp4) <- id ~ top + bottom
hzdesgnname(sp4) <- 'name'

sp4$top[2] <- sp4$top[1]

d5 <- NCSP(sp4, vars = c('Ca', 'CEC_7'), k = 0, maxDepth = 40, var.wt = c(1, 5))

## this won't work, colusa is missing
# plotProfileDendrogram(sp4, diana(d4), scaling.factor = 1, y.offset = 5, width = 0.3, color = 'CEC_7', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE, rotateToProfileID = TRUE)

# keep track of subset profiles
attributes(d5)



##


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
