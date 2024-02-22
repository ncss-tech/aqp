
x <- lapply(1:40, random_profile, method = 'LPP', lpp.a=5, lpp.b=10, lpp.d=5, lpp.e=5, lpp.u=25, n_prop = 1, SPC = TRUE, HzDistinctSim = TRUE)
x <- combine(x)

x$hzd <- hzDistinctnessCodeToOffset(x$HzDistinctCode)


# https://stephango.com/flexoki
# .cols <- c('#A02F6F', '#AF3029', '#BC5215', '#AD8301', '#66800B', '#24837B', '#205EA6', '#5E409D')
.cols <- c('#CE5D97', '#D14D41', '#DA702C', '#D0A215', '#879A39', '#3AA99F', '#4385BE', '#8B7EC8')


par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(x, col.palette = .cols, color = 'p1', width = 0.4, hz.distinctness.offset = 'hzd', lwd = 0.5, print.id = FALSE, depth.axis = list(cex = 0.9, line = -3, style = 'tra'))
title('Flexoki Light', col.main = 'white', line = -2)



library(soilDB)

x <- fetchOSD(c('AVA', 'AUBURN'), extended = TRUE)

x$climate.annual
