## Code / Data related to preparation of Munsell color interpretation in aqp
## 2025-12-10
## D.E. Beaudette, A.G. Brown

# make Munsell and related LUT
# + neutral chips from Munsell Color Atlas
# + odd chroma chips
# + 1/2 value chips
# xyY [C] -> XYZ [D65] -> sRGB -> CIELAB
source('prepare-munsell-LUT.R')

# estimate neutral chip constant reflectance values
# requires current N chip color coordinates
source('neutral-chip-spectra.R')

# spectral reference data
# illuminants / observers
source('make-spectral-reference-RDA.R')

# traditional Munsell color names
source('traditional-names.R')

# prepare a simplified spectral library of Munsell color chips
# simulate neutral chip spectra from constant reflectance
source('prepare-simplfied-spectra-library.R')

# interpolate odd chroma and select 1/2 chip value spectra
source('interpolate-spectra.R')

# create Munsell hue position data
source('make-munsellHuePosition.R')

# TODO: investigate poor agreement between 
# Munsell reference and predicted CIELAB -> Munsell conversion
# these are probably extrapolation artifacts
source('investigate-spectral-interpolation-errors.R')
