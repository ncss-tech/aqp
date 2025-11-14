# Estimate dry soil colors from moist soil colors and vice versa.

Soil color is typically described at dry and moist conditions. This
function attempts to estimate soil color at dry or moist condition when
one is missing. Estimation proceeds as:

- convert Munsell notation to CIELAB color coordinates via
  [`munsell2rgb()`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md)

- apply scaling, rotation, and translation parameters in CIELAB color
  space

- locate closest Munsell chip to CIELAB coordinates via `col2munsell()`

Estimation of dry from moist soil color state is not guaranteed to be
symmetric with estimation of moist from dry.

## Usage

``` r
estimateSoilColor(hue, value, chroma, sourceMoistureState = c("dry", "moist"))
```

## Arguments

- hue:

  vector of Munsell hue ('10YR', '2.5Y', etc.)

- value:

  vector of Munsell value (2,2.5 2.5, 3, 5, 6, etc.)

- chroma:

  vector of Munsell chroma (2, 3, 4, etc.)

- sourceMoistureState:

  character, source colors are either 'dry' or 'moist'

## Value

`data.frame` of estimated colors in Munsell notation. The `sigma` column
contains CIE2000 color contrast metric values describing the perceptual
distance between estimated color in CIELAB coordinates and closest
Munsell chip.

## Details

Scaling, rotation, and translation parameters for shifting between dry
\<–\> moist CIELAB coordinates was determined using
`vegan::procrustes()`, from those official series descriptions (OSD)
where moist and dry soil colors were available.

Estimates for colors having a (dry or moist) Munsell value of 10 are not
likely correct.

This is still a work in progress.

## Author

D.E. Beaudette

## Examples

``` r
# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
#>    hue value chroma    sigma
#> 1 10YR     5      3 3.883131

# note that estimation is not symmetric
estimateSoilColor(hue = '10YR', value = 5, chroma = 3, sourceMoistureState = 'dry')
#>    hue value chroma    sigma
#> 1 10YR     4      3 4.144553
```
