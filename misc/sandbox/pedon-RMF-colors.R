library(aqp)
library(RSQLite)


db <- dbConnect(RSQLite::SQLite(), "E:/NASIS-KSSL-LDM/NASIS-data.sqlite")

q.rmf <- "SELECT DISTINCT
phiid, labsampnum, rdxfeatpct, rdxfeatsize, rdxfeatcntrst, rdxfeathardness, rdxfeatshape, rdxfeatkind, rdxfeatlocation, rdxfeatboundary,
colorpct, colorhue, colorvalue, colorchroma, colormoistst
FROM
phorizon 
JOIN phrdxfeatures ON phorizon.phiid = phrdxfeatures.phiidref
JOIN phredoxfcolor ON phredoxfcolor.phrdxfiidref = phrdxfeatures.phrdxfiid
LEFT JOIN phsample ON phorizon.phiid = phsample.phiidref
ORDER BY phiid;"


x <- dbGetQuery(db, q.rmf)

str(x)
head(x, 20)

x <- subset(x, subset=colormoistst == 'Moist' & !is.na(colorhue) & !is.na(colorvalue) & !is.na(colorchroma))



x$col <- munsell2rgb(x$colorhue, x$colorvalue, x$colorchroma)

table(x$rdxfeatkind)

table(x$rdxfeatcntrst)



cols <- x$col[which(x$rdxfeatkind == 'Reduced matrix')]
previewColors(cols)

cols <- x$col[which(x$rdxfeatkind == 'Iron depletions')]
previewColors(cols)

cols <- x$col[which(x$rdxfeatkind == 'Plinthite nodules')]
previewColors(cols)

cols <- x$col[which(x$rdxfeatkind == 'Masses of reduced iron (Fe+2) accumulation')]
previewColors(cols)

cols <- x$col[which(x$rdxfeatkind == 'Masses of oxidized iron (Fe+3) accumulation')]
previewColors(cols)

cols <- x$col[which(x$rdxfeatkind == 'Masses of manganese accumulation')]
previewColors(cols)



cq <- colorQuantiles(cols)
plotColorQuantiles(cq)

