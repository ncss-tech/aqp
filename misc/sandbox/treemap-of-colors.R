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



### 
x <- fetchKSSL(series='clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

# genhz
s$genhz <- generalize.hz(s$hzn_desgn, c('A', 'E', 'Bt', '2Bt', '3Bt'), pat=c('A', 'E', '^Bt', '2B', '3B'), non.matching.code = NA)
s$genhz <- factor(s$genhz, levels = guessGenHzLevels(s, "genhz")$levels)

table(s$genhz, useNA = 'always')

table(s$genhz, s$hzn_desgn, useNA = 'always')

m <- paste0(s$m_hue, ' ', s$m_value, '/', s$m_chroma)
g <- s$genhz

colorChart(m, g = g, chip.cex = 2)


agg <- aggregateColor(s, "genhz", col = 'moist_soil_color', k = 8)

a <- do.call('rbind', agg$scaled.data)
a.unique <- unique(a[, c('munsell', 'moist_soil_color')])
a.cols <- a.unique$moist_soil_color
names(a.cols) <- a.unique$munsell

ggplot(data = a) + 
  geom_treemap(aes(area = weight, fill = munsell)) +
  geom_treemap_text(aes(area = weight, label = munsell, colour = I(invertLabelColor(moist_soil_color)))) +
  facet_wrap(~ .id, strip.position = "top") + 
  scale_fill_manual(
    guide = 'none',
    values = a.cols
  ) +
  coord_equal() +
  theme_bw() + 
  labs(title = 'Clarksville Moist Color RIC, KSSL Pedons') + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(fill = NA, colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )








