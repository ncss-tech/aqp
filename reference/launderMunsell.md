# Fix Common Errors in Munsell Notation

This function will "launder" colors in Munsell notation, fixing (and
optionally flagging) the following:

- colors specified with 0-chroma will be converted to neutral hue (N)
  and original value

- for neutral colors, any chroma \>0 will be set to 0

- non-standard hues will be converted to NA (unless
  `standardHues = FALSE`)

Examples:

- '10YR 2/0' -\> 'N 2/0'

- 'N 4/1 -\> 'N 4/0'

- '5Z 3/3' -\> NA

- '5YR 3/' -\> NA (hues other than N must have a valid chroma)

See
[`formatMunsell()`](https://ncss-tech.github.io/aqp/reference/formatMunsell.md)
for additional details.

## Usage

``` r
launderMunsell(m, verbose = FALSE, ...)
```

## Arguments

- m:

  character vector of Munsell colors

- verbose:

  logical, optionally return a `data.frame` comparing modifications

- ...:

  additional arguments to
  [`formatMunsell()`](https://ncss-tech.github.io/aqp/reference/formatMunsell.md)

## Value

either character vector, or when `verbose = TRUE` a `data.frame`

## See also

[`formatMunsell()`](https://ncss-tech.github.io/aqp/reference/formatMunsell.md)

## Examples

``` r
# => 'N 2/0'
launderMunsell('10YR 2/0')
#> [1] "N 2/0"

# => 'N 4/0'
launderMunsell('N 4/1')
#> [1] "N 4/0"

# alternative neutral convention => 'N 4/'
launderMunsell('N 4/0', neutralConvention = 'empty')
#> [1] "N 4/"

# => 'N 4/0'
launderMunsell('N 4/NA')
#> Warning: NAs introduced by coercion
#> [1] "N 4/0"
launderMunsell('N 4/')
#> [1] "N 4/0"

# => 'N 4/0'
launderMunsell('5GY 4/0')
#> [1] "N 4/0"

# not a standard hue => NA
launderMunsell('4ZR 4/6')
#> some colors have non-standard hue, result is NA
#> some colors missing hue or value, result is NA
#> [1] NA

# missing chroma, not N => NA
launderMunsell('2.5Y 4/')
#> some colors missing hue or value, result is NA
#> [1] NA

# invalid chroma => NA
launderMunsell('2.5Y 4/A')
#> Warning: NAs introduced by coercion
#> some colors missing hue or value, result is NA
#> [1] NA
```
