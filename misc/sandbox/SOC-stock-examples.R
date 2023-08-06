# library(aqp)
library(soilDB)
library(lattice)
library(tactile)

s <- c('PIERRE', 'ZOOK', 'CHIPLEY', 'DRUMMER', 'CECIL', 'PELLA', 'SABLE')

x <- fetchKSSL(series = s, simplifyColors = TRUE, returnMorphologicData = TRUE)
x <- x$SPC


x <- HzDepthLogicSubset(x)
length(x)
table(x$taxonname)

x$taxonname <- toupper(x$taxonname)
table(x$taxonname)



# ignoring rock fragments for now
# SOC stock by horizon = thick (cm) * Db 1/3 bar (g/cm^3) * (soil fraction) * conversion factor (10)
x$thick <- x$hzn_bot - x$hzn_top
x$soc_kg_sq.m <- x$thick * x$db_13b * (x$estimated_oc / 100) * 10


x$soc.complete.pct <- evalMissingData(x, vars = 'soc_kg_sq.m')


z <- subset(x, soc.complete.pct > 0.9)

table(z$taxonname)

par(mar = c(0, 0, 3, 0))
plotSPC(z, color = 'soc_kg_sq.m', width = 0.35, label = 'taxonname', name.style = 'center-center')
groupedProfilePlot(z, groups = 'taxonname', color = 'soc_kg_sq.m', width = 0.35, name.style = 'center-center', group.name.offset = -10, max.depth = 150)

groupedProfilePlot(z, groups = 'taxonname', color = 'moist_soil_color', width = 0.35, name.style = 'center-center', group.name.offset = -10, max.depth = 150)



## cecil, pierre, zook
pk <- c('14550', '14197', '22888')

x <- fetchKSSL(pedon_key = pk, simplifyColors = TRUE, returnMorphologicData = TRUE)
x <- x$SPC

x$taxonname <- toupper(x$taxonname)

# SOC stock by horizon = thick (cm) * Db 1/3 bar (g/cm^3) * (soil fraction) * conversion factor (10)
x$thick <- x$hzn_bot - x$hzn_top
x$soc_kg_sq.m <- x$thick * x$db_13b * (x$estimated_oc / 100) * 10

x$SOC_stock <- profileApply(x, function(i) {
  sum(i$soc_kg_sq.m, na.rm = TRUE)
})

par(mar = c(0, 0, 3, 1))
groupedProfilePlot(x, groups = 'taxonname', color = 'soc_kg_sq.m', width = 0.35, name.style = 'center-center', group.name.offset = -15, max.depth = 200, cex.names = 0.8)


horizons(x)$hz.sd <- 10


z <- perturb(p = x, n = 10, thickness.attr = 'hz.sd', min.thickness = 5)

# truncate at 100cm !?
z <- trunc(z, 0, 100)

z$thick <- z$hzn_bot - z$hzn_top
z$soc_kg_sq.m <- z$thick * z$db_13b * (z$estimated_oc / 100) * 10

z$SOC_stock <- profileApply(z, function(i) {
  sum(i$soc_kg_sq.m, na.rm = TRUE)
})

par(mar = c(0, 0, 3, 1))
groupedProfilePlot(z, groups = 'taxonname', color = 'soc_kg_sq.m', width = 0.35, name.style = 'center-center', group.name.offset = -5, cex.names = 0.8, print.id = FALSE)

par(mar = c(0, 0, 3, 1))
groupedProfilePlot(z, groups = 'taxonname', color = 'moist_soil_color', width = 0.35, name.style = 'center-center', group.name.offset = -5, cex.names = 0.66, print.id = FALSE)

d <- site(z)

bwplot(taxonname ~ SOC_stock, data = d, par.settings = tactile.theme())
tapply(d$SOC_stock, d$taxonname, quantile)



a <- alignTransect(z$SOC_stock, x.min = 1, x.max = length(z), fix = TRUE, thresh = 0.4, method = 'E', q = 2)

par(mar = c(2, 0, 3, 1))
plotSPC(z, width = 0.25, name.style = 'center-center', 
        cex.names = 0.75, label = 'taxonname', color = 'moist_soil_color',
        relative.pos = a$relative.pos, plot.order = a$order)

axis(1, at = a$relative.pos, labels = round(a$grad), line = -1.5)


z$cumulative.soc_kg_sq.m <- profileApply(z, function(i) {
  cumsum(i$soc_kg_sq.m)
})


par(mar = c(2.5, 0, 3, 1))
groupedProfilePlot(z, groups = 'taxonname', color = 'cumulative.soc_kg_sq.m', width = 0.35, name.style = 'center-center', group.name.offset = -5, cex.names = 0.75, print.id = FALSE, col.label = 'Cumulative SOC Stock (kg C / m^2)', cex.depth.axis = 1, axis.line.offset = -4)

.lsp <- get('last_spc_plot', envir = aqp.env)

.lab <- round(z$SOC_stock)[.lsp$plot.order]

text(x = 1:length(z), y = 105, labels = .lab, cex = 0.85)
mtext('SOC Stock (kg / m^2)', side = 1, line = 0.75, font = 2, cex = 0.85)


par(mar = c(2.5, 0, 0, 1))
groupedProfilePlot(z, groups = 'taxonname', color = 'moist_soil_color', width = 0.35, name.style = 'center-center', group.name.offset = -5, cex.names = 0.75, print.id = FALSE, col.label = 'Cumulative SOC Stock (kg C / m^2)', cex.depth.axis = 1, axis.line.offset = -4)

.lsp <- get('last_spc_plot', envir = aqp.env)
.lab <- round(z$SOC_stock)[.lsp$plot.order]

text(x = 1:length(z), y = 105, labels = .lab, cex = 0.85)
mtext('SOC Stock (kg / m^2)', side = 1, line = 0.75, font = 2, cex = 0.85)


par(mar = c(2.5, 0, 0, 1))
groupedProfilePlot(z, groups = 'taxonname', name = NA, color = 'moist_soil_color', width = 0.4, name.style = 'center-center', group.name.offset = -5, cex.names = 0.75, print.id = FALSE, col.label = 'Cumulative SOC Stock (kg C / m^2)', cex.depth.axis = 1, axis.line.offset = -4, break.style = 'arrow', arrow.offset = -2, group.line.lty = 1, group.line.lwd = 1)

.lsp <- get('last_spc_plot', envir = aqp.env)
.lab <- round(z$SOC_stock)[.lsp$plot.order]

text(x = 1:length(z), y = 105, labels = .lab, cex = 0.85)
mtext('SOC Stock (kg / m^2)', side = 1, line = 0.75, font = 2, cex = 0.85)




agg <- slab(z, fm = taxonname ~ cumulative.soc_kg_sq.m)

xyplot(top ~ p.q50 | taxonname, data=agg, ylab='Depth',
       xlab='median bounded by 25th and 75th percentiles',
       lower=agg$p.q5, upper=agg$p.q95, ylim=c(110,-5),
       panel=panel.depth_function,
       prepanel=prepanel.depth_function,
       cf=agg$contributing_fraction,
       alpha=0.5,
       layout=c(3,1), scales=list(x=list(alternating=1))
)













