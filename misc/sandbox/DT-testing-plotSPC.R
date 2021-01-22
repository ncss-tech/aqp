library(aqp)
library(soilDB)


osds <- fetchOSD(c('leon', 'musick', 'clarksville', 'sierra', 'pardee', 'amador', 'lucy', 'dylan', 'tristan', 'pierre', 'drummer', 'zook'))

# encode horizon boundarydistinctness via vertical offset
osds$hd <- hzDistinctnessCodeToOffset(
  osds$distinctness, 
  codes=c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
)

# encode horizon boundary topography via vertical offset
osds$hzto <- hzTopographyCodeToOffset(
  osds$topography, 
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

# also encode horizon boundary topography las line type
osds$hzto.lty <- hzTopographyCodeToLineType(
  osds$topography,
  codes = c('smooth', 'wavy', 'irregular', 'broken')
)

site(osds)$code <- 'OSD'

osds$bnd.code <- sprintf(
  "%s%s",
  substr(osds$distinctness, 1, 1),
  substr(osds$topography, 1, 1)
)

osds$bnd.code <- gsub('NANA', '', osds$bnd.code)

# ok
par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66) 


## DT testing

library(data.table)
aqp_df_class(osds) <- 'data.table'
osds <- rebuildSPC(osds)

plotSPC(osds)

plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.66, cex.names = 0.66) 

