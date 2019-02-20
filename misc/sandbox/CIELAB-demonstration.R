library(aqp)
library(addTextLabels)


data("munsell")
data("soil_minerals")

x <- munsell[which(munsell$value == 4 & munsell$chroma == 6), ]

x.1 <- subset(munsell, subset=hue == '10YR' & value == 2 & chroma  == 2)
x.2 <- subset(munsell, subset=hue == '7.5YR' & value == 3 & chroma  == 4)


# https://stackoverflow.com/questions/7611169/intelligent-point-label-placement-in-r
# remotes::install_github("JosephCrispell/addTextLabels")

plot(B ~ A, data=x, type='n', las=1)
grid()
abline(h=0, v=0, lty=3)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(x$A, x$B, sprintf("%s %s/%s", x$hue, x$value, x$chroma), cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")


# par(bg=grey(0.95), mar=c(4.5,4.5,3,1))
par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=x, type='n', las=1, main='Munsell Colors in CIELAB\nvalue = 4 | chroma = 6')
grid(col='black')
abline(h=0, v=0, lty=1)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(x$A, x$B, x$hue, cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

points(B ~ A, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(B ~ A, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)
addTextLabels(x.1$A, x.1$B, sprintf("%s %s/%s", x.1$hue, x.1$value, x.1$chroma), cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")
addTextLabels(x.2$A, x.2$B, sprintf("%s %s/%s", x.2$hue, x.2$value, x.2$chroma), cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")


par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=x, type='n', las=1, main='Munsell Colors in CIELAB\nvalue = 4 | chroma = 6')
grid(col='black')
abline(h=0, v=0, lty=1)
points(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
addTextLabels(x$A, x$B, x$hue, cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

m.rgb <- t(col2rgb(parseMunsell(soil_minerals$color))) / 255
m.lab <- convertColor(m.rgb, from='sRGB', to='Lab', from.ref.white = 'D65', clip = FALSE)
m.lab <- as.data.frame(m.lab)
names(m.lab) <- c('L', 'A', 'B')
m <- data.frame(m.lab, mineral=soil_minerals$mineral, munsell=soil_minerals$color, col=parseMunsell(soil_minerals$color), stringsAsFactors = FALSE)

points(B ~ A, data=m, col=m$col, pch=15, cex=3)
addTextLabels(m$A, m$B, m$mineral, cex=0.6, col.background=rgb(0,0,0, 0.1), col.label="black")


par(bg='white', mar=c(4.5,4.5,3,1))
plot(B ~ A, data=m, type='n', las=1, main='Common Soil Pigments', xlab='CIELAB A-Coordinate', ylab='CIELAB B-Coordinate')
grid(col='black')
points(B ~ A, data=m, bg=m$col, pch=22, cex=6)
addTextLabels(m$A, m$B, m$mineral, cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")

par(bg='white', mar=c(4.5,4.5,3,1))
plot(L ~ A, data=m, type='n', las=1, main='Common Soil Pigments', xlab='CIELAB A-Coordinate', ylab='CIELAB L-Coordinate')
grid(col='black')
points(L ~ A, data=m, bg=m$col, pch=22, cex=6)
addTextLabels(m$A, m$L, m$mineral, cex=0.7, col.background=rgb(0,0,0, 0.1), col.label="black")




x <- munsell[which(munsell$value == 3), ]
plot(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)

x <- munsell
plot(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=1)


x <- subset(munsell, subset=hue %in% c('7.5YR', '10YR') & value %in% 2:8 & chroma %in% 2:8)
plot(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=1)
plot(L ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=1)



plot(B ~ A, data=munsell, type='n')
points(B ~ A, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(B ~ A, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)

plot(B ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
points(B ~ A, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(B ~ A, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)

plot(L ~ A, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
points(L ~ A, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(L ~ A, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)

plot(L ~ B, data=x, col=rgb(x$r, x$g, x$b, maxColorValue = 1), pch=15, cex=2)
points(L ~ B, data=x.1, col=rgb(x.1$r, x.1$g, x.1$b, maxColorValue = 1), pch=15, cex=4)
points(L ~ B, data=x.2, col=rgb(x.2$r, x.2$g, x.2$b, maxColorValue = 1), pch=15, cex=4)

