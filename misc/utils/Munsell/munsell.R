## updated Jan 2008
# chromatic adaption functions changed subtly

## updated May 2010
# new spline-function interpolation of x,y components
# yields better odd-numbered Chroma, and Chroma of 1

## updated 2019-12-17
# automatic conversion now (almost) identical to manual


# load some libs
library(colorspace)
library(lattice)
library(plyr)

# nearly identical to manual conversion
# source('automatic_conversion.R')

# load functions to 
# apply the equations from 
# http://www.brucelindbloom.com/Math.html
source('color_conversion_functions.R')

# 
# munsell data comes with a lookup table in xyY colorspace
# url: http://www.cis.rit.edu/mcsl/online/munsell.php
# 
# note:
# Munsell chroma, CIE x, y, and Y. The chromaticity coordinates were calculated using illuminant C and the CIE 1931 2 degree observer.
all.colors <- read.table("munsell-all.dat.gz", header=TRUE)


# note: the data from the Munsell group contains Y values 
# that are in the range of approx: 0-100
# 
# these need to be rescaled to the range of 0-1, 
# but not using the existing min/max values
# instead, set the max Y value at 100

# scale Y values: we do this for the later conversion to RGB
# no need if we are using XYZ() function from colorspace
all.colors$Y <- all.colors$Y/100.0

## 5/17/2010
# interpolate odd chromas, down to chroma == 1
# discard value < 1
all.sub <- subset(all.colors, V >= 1)
all.interpolated <- ddply(all.sub, .(H, V), .fun=interpolate_munsell_chroma)

# convert xyY (C) to XYZ (D65)
all.XYZ <- xyY2XYZ(all.interpolated)

# convert with function XYZ2rgb()
# this returns sRGB
all.rgb <- XYZ2rgb(all.XYZ)

# combine results with original data:
all <- data.frame(all.interpolated, all.rgb)

# make a color object for plotting
plot_cols <- rgb(all$R, all$G, all$B)



# lattice version, by hue, just soil colors- from yellow to red
p1 <- xyplot(V ~ C | factor(H, levels=c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R')),
main="UnCommon Soil Colors", 
data=all, subset=H %in% c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R') & V > 1, 
as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
panel=function(x, y, subscripts, ...) 
{
panel.xyplot(x, y, pch=15, cex=1.5, col=plot_cols[subscripts])
}
)

# print(p1)




# one more plot, this time with even and odd chromas
p2 <- xyplot(V ~ C | factor(H, levels=c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R')),
main="Common Soil Colors", 
data=all, subset=H %in% c('2.5Y', '10YR', '7.5YR', '5YR', '2.5YR', '10R') & V >= 1 & V <= 8 & C <= 8, 
as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
panel=function(x, y, subscripts, ...) 
{
panel.xyplot(x, y, pch=15, cex=3.5, col=plot_cols[subscripts])
}
)


## save hard copy
# Cairo(file='soil_colors.png', type='png', bg='white', width=800, height=600) 
# Cairo(file='soil_colors.pdf', type='pdf', bg='white', width=10.5, height=8, units='in') 

pdf(file='soil_colors.pdf', width=10.5, height=8)
# a more neutral panel bg col:
strip.background <- trellis.par.get("strip.background")
trellis.par.set(strip.background = list(col = grey(7:1/8))) 
print(p2)

dev.off()





## save combined colors:
write.table(subset(all, select=c('H','V','C','R','G','B')), "soil_colors-all-RGB.v5", quote=TRUE, col.names=FALSE, sep=",")


