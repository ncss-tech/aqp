# Estimate dry soil colors from moist soil colors and vice versa.

All else equal, soil color will predictably shift in perceived lightness
(change in Munsell value) as moisture content changes. Field-described
soil colors are typically collected at approximately air dry ("dry") and
field capacity ("moist") states. This function estimates "dry" soil
colors from "moist" soil colors and vice versa. Two methods are
available for estimation, both developed from a national collection of
field-described soil colors (approx. 800k horizons). "

- "procrustes": soil colors are converted using scale, rotation, and
  translation parameters in CIELAB color space

- "ols": soil colors are converted using 3 multiple linear regression
  models (CIELAB coordinates)

Estimates for colors having a (dry or moist) Munsell value \>= 10 are
not likely correct.

This is still a work in progress.

## Usage

``` r
estimateSoilColor(
  hue,
  value,
  chroma,
  method = c("procrustes", "ols"),
  sourceMoistureState = c("dry", "moist"),
  returnMunsell = TRUE
)
```

## Arguments

- hue:

  vector of Munsell hue ('10YR', '2.5Y', etc.)

- value:

  vector of Munsell value (2,2.5 2.5, 3, 5, 6, etc.)

- chroma:

  vector of Munsell chroma (2, 3, 4, etc.)

- method:

  character, one of 'procrustes' or 'ols', see details

- sourceMoistureState:

  character, source colors are either 'dry' or 'moist'

- returnMunsell:

  logical, `TRUE`: return closest Munsell chip, `FALSE`: return
  estimated CIELAB coordinates

## Value

`data.frame` of estimated colors in Munsell notation. The `sigma` column
contains CIE2000 color contrast metric values describing the perceptual
distance between estimated color in CIELAB coordinates and closest
Munsell chip.

## Details

For both methods, estimation proceeds as:

- convert Munsell notation to CIELAB color coordinates via
  [`munsell2rgb()`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md)

- apply rotation or regression model to color coordinates in CIELAB
  space

- locate closest Munsell chip to resulting CIELAB coordinates via
  `col2munsell()`

Estimation of dry from moist soil color state is not guaranteed to be
symmetric with estimation of moist from dry.

Scaling, rotation, and translation parameters for shifting between dry
\<–\> moist CIELAB coordinates were determined using
`vegan::procrustes()`. Multiple linear regression models were fit using
`rms::ols()`.

## References

J. A. Shields, E. A. Paul, R. J. St. Arnaud, and W. K. Head. 1968.
SPECTROPHOTOMETRY MEASUREMENT OF SOIL COLOR AND ITS RELATIONSHIP TO
MOISTURE AND ORGANIC MATTER. Canadian Journal of Soil Science. 48(3):
271-280. https://doi.org/10.4141/cjss68-037

## Author

D.E. Beaudette

## Examples

``` r
# keep examples from using more than 2 cores
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
#>    hue value chroma    sigma
#> 1 10YR     4      3 5.045292

# estimation is not always symmetric
estimateSoilColor(hue = '10YR', value = 4, chroma = 3, sourceMoistureState = 'dry')
#>    hue value chroma    sigma
#> 1 10YR     3      3 1.051237

# more examples
estimateSoilColor(hue = '2.5Y', value = 8, chroma = 2, sourceMoistureState = 'dry')
#>    hue value chroma    sigma
#> 1 2.5Y     6      2 1.038539
estimateSoilColor(hue = '2.5YR', value = 3, chroma = 4, sourceMoistureState = 'moist')
#>     hue value chroma    sigma
#> 1 2.5YR     4      4 4.972274

estimateSoilColor(hue = 'N', value = 2, chroma = 0, sourceMoistureState = 'moist')
#>   hue value chroma    sigma
#> 1   N     4      0 3.034095

estimateSoilColor(hue = '7.5YR', value = 2, chroma = 2, sourceMoistureState = 'moist')
#>     hue value chroma    sigma
#> 1 7.5YR     4      2 3.677677

# resulting hue is not always the same
estimateSoilColor(hue = '5G', value = 6, chroma = 6, sourceMoistureState = 'dry')
#>    hue value chroma    sigma
#> 1 2.5G     4      5 3.887497

# return estimated CIELAB coordinates
estimateSoilColor(hue = '5G', value = 6, chroma = 6, sourceMoistureState = 'dry',
 returnMunsell = FALSE)
#>          L         A        B
#> 1 44.97181 -24.41767 12.80506
```
