# Parse Munsell Color Notation

Split Munsell color notation into "hue", "value", and "chroma", with
optional conversion to sRGB hex notation, sRGB coordinates, and CIELAB
coordinates. Conversion is performed by
[`munsell2rgb()`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md).

## Usage

``` r
parseMunsell(munsellColor, convertColors = TRUE, delim = NA, ...)
```

## Arguments

- munsellColor:

  character vector of Munsell colors (e.g. `c('10YR 3/4', '5YR 4/6')`)

- convertColors:

  logical, convert colors to sRGB hex notation, sRGB coordinates, CIELAB
  coordinates

- delim:

  optional, specify the type of delimiter used between value and chroma
  parts of the Munsell code. By default ":", ",:, "'", and "/" are
  supported.

- ...:

  additional arguments to
  [`munsell2rgb()`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md)

## Value

a `data.frame` object

## Author

P. Roudier and D.E. Beaudette

## Examples

``` r
# just sRGB
parseMunsell("10YR 3/5", return_triplets = TRUE)
#>           r         g          b
#> 1 0.3754983 0.2555129 0.09093377

# sRGB + CIELAB (D65 illuminant)
parseMunsell("10YR 3/5", return_triplets = TRUE, returnLAB = TRUE)
#>           r         g          b        L        A        B
#> 1 0.3754983 0.2555129 0.09093377 30.27986 8.888747 29.95071

# CIELAB only
parseMunsell("10YR 3/5", return_triplets = FALSE, returnLAB = TRUE)
#>          L        A        B
#> 1 30.27986 8.888747 29.95071

# neutral hue
# note chroma encoded as '0'
parseMunsell('N 3/', convertColors = FALSE)
#>   hue value chroma
#> 1   N     3      0
```
