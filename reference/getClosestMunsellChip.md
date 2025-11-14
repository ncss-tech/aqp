# Get Approximate Munsell Chip

Non-standard Munsell notation ('7.9YR 2.7/2.0') can be matched
(nearest-neighbor, no interpolation) to the closest color within the
`munsell` sRGB/CIELAB look-up table via `getClosestMunsellChip()`. A
more accurate estimate of sRGB values from non-standard notation can be
achieved with the
[munsellinterpol](https://CRAN.R-project.org/package=munsellinterpol)
package. For example, conversion from Munsell to CIELAB, assuming a D65
illuminant via:
`MunsellToLab('0.1Y 3.3/4.4', white='D65', adapt='Bradford')`.

## Usage

``` r
getClosestMunsellChip(munsellColor, convertColors = TRUE, ...)
```

## Arguments

- munsellColor:

  character vector of strings containing Munsell notation of color, e.g.
  '10YR 4/3', not NA-safe

- convertColors:

  logical, should parsed Munsell colors be converted into sRGB values

- ...:

  further arguments to `munsell2rgb`

## Value

a `data.frame` when `convertColors=TRUE`, otherwise character vector

## Examples

``` r
# convert a non-standard color to closest "chip" in `munsell` look-up table
getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE)
#> [1] "7.5YR 2.5/2"

# convert directly to R color
getClosestMunsellChip('7.9YR 2.7/2.0')
#> [1] "#48382CFF"

# special case for 2.5 value -> no rounding, we have these records in the conversion LUT
getClosestMunsellChip('7.5YR 2.5/2', convertColors = FALSE)
#> [1] "7.5YR 2.5/2"


getClosestMunsellChip('7.5YR 6.8/4.4', convertColors = FALSE)
#> [1] "7.5YR 7/4"
```
