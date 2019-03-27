library(colorspace)
library(aqp)
library(soilDB)

x <- parseMunsell(c('10YR 3/4', '10YR 6/2', '5YR 3/3'))
  
data("loafercreek")

x <- data.frame(color=loafercreek$moist_soil_color, g=loafercreek$genhz)
x <- na.omit(x)
x <- split(x$color, x$g)
  
swatchplot(x)




