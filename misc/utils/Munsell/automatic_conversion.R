
## 2019-12-17: results nearly identical to manual conversion

# 
# automatic conversion
# 
# ... do not need to rescale Y to (0,1)

# 
# munsell data comes with a lookup table in xyY colorspace
# 
# url: http://www.cis.rit.edu/mcsl/online/munsell.php
# 
# note:
# Munsell chroma, CIE x, y, and Y. The chromaticity coordinates were calculated using illuminant C and the CIE 1931 2 degree observer.
# 
# optionally only read in common colors:
# all <- read.table("soil_colors", header=F, col.names=c("H","V","C","x","y","Y"))
# 
all <- read.table("munsell-all.dat.gz", header=TRUE)


X <- (all$x * all$Y ) / all$y
Y <- all$Y
Z <- ( (1- all$x - all$y) * all$Y )  / all$y

# combine to form matrix for simple manipulation
mun_XYZ_C <- matrix(c(X,Y,Z), ncol=3)

# test for y == 0
# X,Y,Z should then be set to 0
mun_XYZ_C[which(all$y==0),] <- c(0,0,0)


## old version
# M_adapt_C_to_D65 <- matrix(c(0.990490, -0.012269, -0.003515, -0.007115, 1.015427, 0.006679, -0.011434, -0.002878, 0.919313), ncol=3, byrow=TRUE)

## this has been revised as of Jan 2008 
## new version:
M_adapt_C_to_D65 <- matrix(c(0.990448, -0.012371, -0.003564, -0.007168, 1.015594, 0.006770, -0.011615, -0.002928, 0.918157), ncol=3, byrow=TRUE)


# 
# to perform the chromatic adaption: convert from C -> D65 using Bradford method
# 
mun_XYZ_D65 <- mun_XYZ_C %*% M_adapt_C_to_D65



# convert to sRGB with colorspace function:
mun_sRGB_D65.auto <- as(XYZ(mun_XYZ_D65), 'sRGB')@coords



# 
# save the results back to the table:
# 
all$r <- mun_sRGB_D65.auto[,1]
all$g <- mun_sRGB_D65.auto[,2]
all$b <- mun_sRGB_D65.auto[,3]



# the auto-converted
plot(as(XYZ(mun_XYZ_D65), 'LUV'), cex=0.5)

# done
