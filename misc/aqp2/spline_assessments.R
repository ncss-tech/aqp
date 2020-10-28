# Investigations into spline precision over [0,100] centimeter interval
# andrew g brown 
library(aqp)

# CONSTANTS for linear increase to max and power functions
const_A <- 1
const_B <- -0.5
const_ZMAX <- 50

# VARIABLES
# input options: linpeak, clinpeak, lindeak, clindeak
#                powinc, cpowinc, powdec, cpowdec
input_var <- "powdec" 
spline_var <- paste0(input_var, '_spline')

# TEST GEOMETRIES

# Constant Interval
ctop <- seq(0, 99, 10)
cbottom <- c(ctop[-1], 100)

# Increasing Interval
itop <- c(0, 1, 3, 5, 10, 15, 30, 50, 70, 90)
ibottom <- c(itop[-1], 100)

# Random Interval
set.seed(123)
rtop <- c(0, sort(round(runif(9, 0, 99))))
rbottom <- c(rtop[-1], 100)

# TEST DATASETS

# Linear Increase
lininc <- seq(1,100)

# Linear Decrease
lindec <- rev(lininc) 

# Power Function 
pow <- function(z, a, b) a * z ^ b

# TODO: Simulated Natural Breaks (based on one or more properties/rates of change)

# Depth-peaked Function
linearPeak <- function(z, zmax, FUN = function(x) x) {
  d <- abs(zmax - z)
  d2 <- 1 - (d / max(d))
  FUN(d2)
}

# linear peak
linpeak <- linearPeak(lininc, const_ZMAX)
plot(linpeak, -lininc)

# cumulative linear peak (inflection at const_ZMAX)
clinpeak <- cumsum(linpeak)
plot(clinpeak, -lininc)

# linear peak (decreasing)
lindeak <- 1 - linpeak

# cumulative linear peak (decreasing; inflection at const_ZMAX)
clindeak <- cumsum(lindeak)
plot(clindeak, -lininc)

# pow 
powinc <- pow(lininc, const_A, const_B)
plot(powinc, -lininc)

cpowinc <- cumsum(powinc)
plot(cpowinc, -lininc)

powdec <- pow(lindec, const_A, const_B)
plot(powdec, -lininc)

cpowdec <- cumsum(powdec)
plot(cpowdec, -lininc)

# one realization, using a set of parameters (pow A and B, const_ZMAX)
#  the decrease functions are reversed to be consistent with top and bottom depth
#  hit this with an array of different lambdas
rez <- data.frame(id = 1, 
           top = lininc - 1, bottom = lininc,
           linpeak, clinpeak, 
           lindeak = rev(lindeak), clindeak = rev(clindeak), 
           powinc, cpowinc, 
           powdec = rev(powdec), cpowdec = rev(cpowdec))

depths(rez) <- id ~ top + bottom

plot(rez, color = input_var, divide.hz=FALSE)

horizons(rez)$name <- ""
hzdesgnname(rez) <- "name"

# default lambda = 0.1
test1 <- spc2mpspline(rez, input_var)
plot(test1[[input_var]] ~ test1[[spline_var]])
abline(0,1)

# 0.01 is closer
test2 <- spc2mpspline(rez, input_var, lam = 0.01)
plot(test2[[input_var]] ~ test2[[spline_var]])
abline(0,1)

# 1 is farther
test3 <- spc2mpspline(rez, input_var, lam = 1)
plot(test3[[input_var]] ~ test3[[spline_var]])
abline(0,1)

# 0 just to see
test4 <- spc2mpspline(rez, input_var, lam = 0)
plot(test4[[input_var]] ~ test4[[spline_var]])
abline(0,1)

# now that we have investigated how the splines work on the raw data, we will try some aggregation.

group_hz <- function(object, new_top, new_bottom) {
  hzd <- horizonDepths(object)
  mid <- ((object[[hzd[2]]] - object[[hzd[1]]]) / 2) + object[[hzd[1]]]
  name <- apply(do.call('rbind', lapply(1:length(new_top), function(i) {
    mid >= new_top[i] & mid <= new_bottom[i]
  })), 2, which)
  return(name)
}

simplifySPC <- function(object, new_top, new_bottom) {
  hzd <- horizonDepths(object)
  spc <- data.frame(id = paste0(profile_id(object), "_simplify"),
                    top = new_top, bottom = new_bottom,
                    layerid = unique(group_hz(object, new_top, new_bottom)))
  spc[[idname(object)]] <- spc$id
  spc[[hzd[1]]] <- spc$top
  spc[[hzd[2]]] <- spc$bottom
  spc <- aqp:::.data.frame.j(spc, c(idname(object), hzd, "layerid"), aqp_df_class(object))
  depths(spc) <- id ~ top + bottom
  return(spc)
}

## CONSTANT DEPTH
crez <- simplifySPC(rez, ctop, cbottom)
crez[[input_var]] <- aggregate(rez[[input_var]], by = list(group_hz(rez, ctop, cbottom)), mean)$x

# same data, averaged over constant 10cm intervals
plot(crez, color = input_var, divide.hz=FALSE)

horizons(crez)$name <- ""
hzdesgnname(crez) <- "name"

# compare
hzc <- spc2mpspline(crez, input_var)
hzc2 <- spc2mpspline(crez, input_var, lam=0.01)
hzc3 <- spc2mpspline(crez, input_var, lam=1)
hzc4 <- spc2mpspline(crez, input_var, lam=0)

plot(rez[[input_var]], -lininc)
lines(hzc[[input_var]], -lininc)
lines(hzc[[spline_var]], -lininc, lty=2, col="blue")

## INCREASING INTERVAL WITH DEPTH
irez <- simplifySPC(rez, itop, ibottom)
irez[[input_var]] <- aggregate(rez[[input_var]], by = list(group_hz(rez, itop, ibottom)), mean)$x

plot(irez, color = input_var, divide.hz=FALSE)

horizons(irez)$name <- ""
hzdesgnname(irez) <- "name"

hzi <- spc2mpspline(irez, input_var, lam=0.1)
hzi2 <- spc2mpspline(irez, input_var, lam=0.01)
hzi3 <- spc2mpspline(irez, input_var, lam=1)
hzi4 <- spc2mpspline(irez, input_var, lam=0)

plot(rez[[input_var]], -lininc)
lines(hzi[[input_var]], -lininc)
lines(hzi[[spline_var]], -lininc, lty=2, col="blue")

## RANDOM DEPTH
rrez <- simplifySPC(rez, rtop, rbottom)
rrez[[input_var]] <- aggregate(rez[[input_var]], by = list(group_hz(rez, rtop, rbottom)), mean)$x

plot(rrez, color = input_var, divide.hz=FALSE)

horizons(rrez)$name <- ""
hzdesgnname(rrez) <- "name"

hzr <- spc2mpspline(rrez, input_var)
hzr2 <- spc2mpspline(rrez, input_var, lam=0.01)
hzr3 <- spc2mpspline(rrez, input_var, lam=1)
hzr4 <- spc2mpspline(rrez, input_var, lam=0)

plot(rez[[input_var]], -lininc)
lines(hzr[[input_var]], -lininc)
lines(hzr[[spline_var]], -lininc, lty=2, col="blue")

DT::datatable(data.frame(rbind(site(test1),site(test2), site(test3),
                 site(hzc), site(hzc2), site(hzc3),
                 site(hzi), site(hzi2), site(hzi3),
                 site(hzr), site(hzr2), site(hzr3)),  
                 lambda = rep(c(0.1, 0.01, 1),4),
                 geom = do.call('c', lapply(c("1cm","10cm","Increasing","Random"), rep, 3)),
                 source = c(rep("raw, synthetic",3), rep("aggregate",9))))


