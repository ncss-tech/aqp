## Traditional color names for select Munsell chips
## sourced from "colordecoder" NASIS property
## originally by Jim Fortner (2008), later revised by Kevin Godsey
## USDA-NRCS
## 
## Sample data object prepared and documented by D.E. Beaudette
## 2021-04-22
## 

# copy -> paste -> edit -> CSV
traditionalColorNames <- read.csv('traditional-names-from-NASIS.csv')
traditionalColorNames$traditional_name <- trimws(traditionalColorNames$traditional_name)

save(traditionalColorNames, file = '../../../data/traditionalColorNames.rda', compress = 'xz')

# another, possibly better approach
# using Color Block via 'munsellinterpol' package
# munsellinterpol::ColorBlockFromMunsell(colors)
# 
# 
# data(munsell)
# colors <- with(munsell, sprintf('%s %s/%s', hue, value, chroma))
# 
# cb <- ColorBlockFromMunsell(colors)
# 
# ... save this to a new object perhaps
