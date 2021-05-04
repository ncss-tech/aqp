
# get from CRAN first, then install latest from GH
# install.packages(c('aqp', 'sharpshootR'))
# remotes::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade=FALSE, build=FALSE)
# remotes::install_github("ncss-tech/sharpshootR", dependencies=FALSE, upgrade=FALSE, build=FALSE)

library(aqp)
library(sharpshootR)

data("soil_minerals")


quartz <- soil_minerals$color[soil_minerals$mineral == 'quartz']
hematite <- soil_minerals$color[soil_minerals$mineral == 'hematite-fine']
humus <- soil_minerals$color[soil_minerals$mineral == 'humus']

chips <- c(quartz, hematite, humus)
w <- c(10, 1, 5)

plural <- ifelse(w > 1, 's', '')
names(chips) <- sprintf(
  fmt = "%s part%s\n%s", 
  w, 
  plural, 
  c('quartz', 'hematite', 'humus')
)
  
plotColorMixture(chips, w = w, mixingMethod = 'exact')

colorMixtureVenn(chips, w = w, mixingMethod = 'exact', names = TRUE)


## consider converting other mineral reflectance spectra into "colors"
# https://www.usgs.gov/labs/spec-lab/capabilities/spectral-library
