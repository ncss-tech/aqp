# Convert reflectance spectra to closest Munsell chip

Convert reflectance spectra to closest Munsell chip

## Usage

``` r
spec2Munsell(
  x,
  res = 10,
  convert = TRUE,
  SO = c("CIE1931", "CIE1964"),
  illuminant = c("D65", "F2"),
  ...
)
```

## Arguments

- x:

  reflectance spectra, must range from 380nm to 730nm with resolution
  specified in `res`

- res:

  spectra resolution in nm, typically 5nm or 10nm

- convert:

  logical, convert sRGB coordinates to closest Munsell chip (see
  [`?munsell`](https://ncss-tech.github.io/aqp/reference/munsell.md))

- SO:

  CIE standard observer: these are the color matching functions defined
  by CIE and used to represent "average" human color perception. CIE1931
  is the 2 degree standard observer more useful for describing color
  perception over very small areas or at distance. CIE1964 is the 10
  degree standard observer, used for most industrial color matching
  applications.

- illuminant:

  CIE standard illuminants:

  - D65 represents average daylight

  - F2 represents typical fluorescent lighting

- ...:

  further arguments to
  [`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md)

## Value

output from
[`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md)

## Details

See the [expanded
tutorial](https://ncss-tech.github.io/AQP/aqp/mix-colors.html) for
additional examples.

## References

Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color
for Science, Art, and Technology (pp. 32-96). North-Holland.

CIE Colorimetry – Part 1: CIE standard colorimetric observers.
CIES014-1/E:2006 – ISO 11664-1:2007(E)

CIE. (n.d.). CIE 15:2004 Tables Data. Retrieved from
https://law.resource.org/pub/us/cfr/ibr/003/cie.15.2004.tables.xls
