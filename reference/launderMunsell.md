# Fix Common Errors in Munsell Notation

This function will "launder" colors in Munsell notation, fixing (and
optionally flagging) the following:

- colors specified with 0-chroma will be converted to neutral hue (N)
  and original value

- for neutral colors, any chroma \>0 will be set to 0

Examples:

- '10YR 2/0' -\> 'N 2/0'

- 'N 4/1 -\> 'N 4/0'

## Usage

``` r
launderMunsell(m, verbose = FALSE)
```

## Arguments

- m:

  character vector of Munsell colors

- verbose:

  logical, optionally return a `data.frame` comparing modifications

## Value

either character vector, or when `verbose = TRUE` a `data.frame`

## Examples

``` r
# will be converted to 'N 4/0'
launderMunsell('5G 4/0')
#> [1] "N 4/0"
```
