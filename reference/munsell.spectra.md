# Spectral Library of Munsell Colors

The original database
"SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt" was provided by
Paul Centore and downloaded July, 2020. Reflectance values for odd
chroma and 8.5-value chips have been interpolated from adjacent chips.
See `aqp/misc/utils/Munsell/` for the entire set of processing steps.

Munsell value typically ranges from 2-9, and chroma from 1-12. Ranges
vary by hue. Run `aqp:::.summarizeMunsellSpectraRanges()` for a detailed
listing by hue.

The original database contains the following description:

This file contains spectral reflectance measurements of X-Rite's 2007
Munsell Book of Color (Glossy Finish). The measurements were made in
2012 with a ColorMunki spectrophotometer. The first column is the
Munsell name. The remaining columns give reflectance values for 380 nm
to 730 nm, in steps of 10 nm. The reflectance is a value between 0
(indicating that no light at that wavelength is reflected) and 1
(indicating that all the light at that wavelength is reflected).
Occasionally an entry is slightly greater than 1. The likely cause is
random variability, and those entries can be adjusted to 1 with
negligible loss. In all, 1485 colour samples were measured. Researchers
are invited to analyze the data in this file.

## Usage

``` r
data(munsell.spectra)
```

## Format

A data frame with 89496 rows and 10 variables:

- munsell:

  munsell color

- hue:

  hue component

- value:

  value component

- chroma:

  chroma component

- wavelength:

  wavelength (nm)

- reflectance:

  reflectance

## References

Centore, Paul. Colour Tools for Painters.
http://www.munsellcolourscienceforpainters.com/.
