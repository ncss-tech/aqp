# Validate Standard Munsell Notation

This function validates Munsell color notation according to the
following requirements:

1.  *hue* must not be absent, and within the set of "standard hues", see
    [`huePosition()`](https://ncss-tech.github.io/aqp/reference/huePosition.md)

2.  *value* must not be absent, and within `vRange`

3.  *chroma* must be within `cRange`, and is only allowed to be absent
    in neutral colors (e.g. `N 4/`)

## Usage

``` r
validateMunsell(m, vRange = c(1, 12), cRange = c(0, 50))
```

## Arguments

- m:

  character vector of colors in Munsell notation

- vRange:

  numeric vector of length 2, range of expected Munsell value

- cRange:

  numeric vector of length 2, range of expected Munsell chroma

## Value

logical vector,

- `TRUE`: valid Munsell notation

- `FALSE`: invalid notation

## See also

[`huePosition()`](https://ncss-tech.github.io/aqp/reference/huePosition.md)

## Examples

``` r
# valid
validateMunsell('5Y 6/8')
#> [1] TRUE
validateMunsell('N 4/')
#> [1] TRUE
validateMunsell('2.5Y 4/0')
#> [1] TRUE

# invalid
validateMunsell('5G 0/4')
#> [1] FALSE
validateMunsell(NA)
#> [1] FALSE

# mixture
validateMunsell(c('5G 4/4', 'N 2/', NA, 'NA', '100R 3/3'))
#> [1]  TRUE  TRUE FALSE FALSE FALSE
```
