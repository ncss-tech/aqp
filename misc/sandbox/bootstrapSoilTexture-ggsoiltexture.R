library(aqp)
library(ggsoiltexture)
library(ggplot2)
library(dplyr)
library(lattice)

x <- texcl_to_ssc(rep('sil', times = 10), sample = TRUE)
names(x) <- toupper(names(x))


samples <- bootstrapSoilTexture(x, n = 100, method = "d")

y <- samples$samples

g <- make.groups(original = x, sim = y)
names(g) <- tolower(names(g))




ggsoiltexture(g, show_grid = TRUE, class = "USDA") + 
  geom_point(aes(color = which), size = 2) + 
  scale_colour_brewer(palette = 'Set1') +
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom") + 
  labs(title = 'Original Simulation')
