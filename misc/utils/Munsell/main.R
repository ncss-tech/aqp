## Code / Data related to preparation of Munsell color interpretation in aqp
## 2024-10-03
## D.E. Beaudette, A.G. Brown

# make Munsell and related LUT
# + neutral chips
# + odd chroma
# + 0.5 value 
# xyY [C] -> XYZ [D65] -> sRGB -> CIELAB
source('prepare-munsell-LUT.R')

# spectral reference data
# illuminants / observers
source('make-spectral-reference-RDA.R')

# traditional Munsell color names
source('traditional-names.R')

# prepare a simplified spectral library of Munsell color chips
source('prepare-simplfied-spectra-library.R')

# interpolate odd chroma and 2.5 value spectra
source('interpolate-spectra.R')

# create Munsell hue position data
source('make-munsellHuePosition.R')
