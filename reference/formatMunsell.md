# Format Munsell Notation from Hue, Value, and Chroma

Format hue, value, and chroma values into valid Munsell notation, or NA
when not possible. The following rules are applied to the input:

- for all hues other than N, there must be a valid hue, value, and
  chroma

- N hues may specify chroma as 0, NA, or ” (empty string)

- when `standardHues = TRUE`, hue must be a standard Munsell hue, see
  [`huePosition()`](https://ncss-tech.github.io/aqp/reference/huePosition.md)

## Usage

``` r
formatMunsell(
  hue,
  value,
  chroma,
  neutralConvention = c("zero", "empty"),
  standardHues = TRUE
)
```

## Arguments

- hue:

  character vector of Munsell hue

- value:

  character or numeric vector of Munsell value

- chroma:

  character or numeric vector of Munsell chroma

- neutralConvention:

  character, neutral color encoding convention

  Neutral colors are encoded as:

  - 'empty': N 3/

  - 'zero': N 3/0

- standardHues:

  logical, when `TRUE` non-standard hues are converted to NA

## Value

character vector of same length as `hue`, `value`, and `chroma`

## See also

[`launderMunsell()`](https://ncss-tech.github.io/aqp/reference/launderMunsell.md)

## Author

D.E. Beaudette

## Examples

``` r
d <- data.frame(
hue = c('10YR', NA, 'N', 'N', 'N', '5G', '5Z'),
value = c(4, 3, 2, 4, 3, NA, 4),
chroma = c(4, 3, NA, 0, NA_integer_, 6, 3)
)

formatMunsell(d$hue, d$value, d$chroma)
#> some colors have non-standard hue, result is NA
#> some colors missing hue or value, result is NA
#> [1] "10YR 4/4" NA         "N 2/0"    "N 4/0"    "N 3/0"    NA         NA        
formatMunsell(d$hue, d$value, d$chroma, neutralConvention = 'empty')
#> some colors have non-standard hue, result is NA
#> some colors missing hue or value, result is NA
#> [1] "10YR 4/4" NA         "N 2/"     "N 4/"     "N 3/"     NA         NA        
formatMunsell(d$hue, d$value, d$chroma, standardHues = FALSE)
#> some colors missing hue or value, result is NA
#> [1] "10YR 4/4" NA         "N 2/0"    "N 4/0"    "N 3/0"    NA         "5Z 4/3"  

# all result in 'N 6/0' (neutralConvention = 'zero')
formatMunsell('N', 6, 0)
#> [1] "N 6/0"
formatMunsell('N', 6, '')
#> [1] "N 6/0"
formatMunsell('N', 6, NA)
#> [1] "N 6/0"
```
