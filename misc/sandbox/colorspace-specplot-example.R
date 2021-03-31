library(aqp)
library(colorspace)


# https://www.jstatsoft.org/article/view/v096i01


h <- huePosition(returnHues = TRUE)
m <- sprintf("%s 8/8", h)

cols <- parseMunsell(m)

specplot(cols, rgb = TRUE)
hclplot(cols)



