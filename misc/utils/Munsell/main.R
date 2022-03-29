## Code / Data related to preparation of Munsell color interpretation in aqp
## 2022-03-29
## D.E. Beaudette, A.G. Brown




# make munsell and related LUT
# munsell hue / value / chroma
# sRGB, CIELAB coordinates


# add neutral chips



# spectral reference data
# illuminants / observers
source('make-spectral-reference-RDA.R')

# traditional Munsell color names
source('traditional-names.R')

# prepare a simplified spectral library of Munsell color chips
source('prepare-simplfied-spectra-library.R')

# interpolate odd chroma spectra
source('interpolate-spectra.R')

# create Munsell hue position data
source('make-munsellHuePosition.R')
