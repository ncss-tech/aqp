# Make High Contrast Label Colors

Generate a vector of white or black label colors conditioned on a vector
of colors to maximize label contrast.

## Usage

``` r
invertLabelColor(colors, threshold = 0.65)
```

## Arguments

- colors:

  vector of colors

- threshold:

  black \| white threshold

## Value

vector of label colors

## Author

D.E. Beaudette

## Examples

``` r
# test with shades of grey
s <- seq(0, 1, by = 0.05)
cols <- grey(s)
soilPalette(cols, lab = as.character(s))


# test with 10YR x/3
m <- sprintf('10YR %s/3', 1:8)
cols <- parseMunsell(m)
soilPalette(cols, lab = m)


```
