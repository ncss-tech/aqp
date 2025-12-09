# requires for aqp stuff
library(aqp)
library(compositions)

# this great package
library(ggsoiltexture)

# hmm ... looks like I need to load these directly
# maybe these can be in package SUGGESTS?
library(dplyr)
library(ggplot2)
library(ggrepel)



# simulate 100 [sand, silt, clay] values using vector of soil texture classes
# arms waving: "many assumptions, diffuse clusters, etc."
simData <- function(tx) {
  
  # sample [sand, silt, clay] within supplied soil texture classes
  z <- texcl_to_ssc(tx, sample = TRUE)
  
  # bootstrapSoilTexture() requires upper case, annoying
  names(z) <- toupper(names(z))
  
  # simulate from these samples
  s <- bootstrapSoilTexture(z, n = 100, method = 'dirichlet')
  
  # retain only samples
  s <- s$samples
  
  # return names to lower case
  names(s) <- tolower(names(s))
  
  return(s)
}

# cluster A
tx <- sample(c('LS', 'SL'), size = 10, replace = TRUE)
A <- simData(tx)

# cluster B
tx <- sample(c('SiL', 'Si'), size = 10, replace = TRUE)
B <- simData(tx)

# cluster C
tx <- sample(c('CL', 'C'), prob = c(0.9, 0.1), size = 10, replace = TRUE)
C <- simData(tx)

# stack and label groups of rows
x <- lattice::make.groups(A, B, C)

# plot on soil TT
ggsoiltexture(x, show_grid = TRUE, class = "USDA") + 
  geom_point(aes(color = which), size = 1) + 
  scale_colour_brewer(palette = 'Set1') + 
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom")








