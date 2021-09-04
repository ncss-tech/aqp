library(aqp)
library(soilDB)
library(treemapify)
library(ggplot2)


## this is a nice alternative to aggregateColorPlot() from sharpshootR

x <- fetchKSSL(c('zook', 'drummer', 'pierre', 'lucy'), returnMorphologicData = TRUE, simplifyColors = TRUE)
s <- x$SPC

table(s$taxonname)
s$taxonname <- factor(toupper(s$taxonname))

agg <- aggregateColor(s, groups = 'taxonname', col = 'moist_soil_color', k = 12)
sharpshootR::aggregateColorPlot(agg)

m <- paste0(s$m_hue, ' ', s$m_value, '/', s$m_chroma)
colorChart(m)

a <- do.call('rbind', agg$scaled.data)
a.unique <- unique(a[, c('munsell', 'moist_soil_color')])
a.cols <- a.unique$moist_soil_color
names(a.cols) <- a.unique$munsell

ggplot(data = a) + 
  geom_treemap(aes(area = weight, fill = munsell)) +
  geom_treemap_text(aes(area = weight, label = munsell, colour = I(invertLabelColor(moist_soil_color)))) +
  facet_wrap(~ .id, strip.position = "bottom") + 
  scale_fill_manual(
    guide = 'none',
    values = a.cols
  ) +
  coord_equal() +
  theme_bw() + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(fill = NA, colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

