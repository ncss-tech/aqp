# Visualize Spectral Mixing of Munsell Colors

Lattice visualization demonstrating subtractive mixtures of colors in
Munsell notation and associated spectra.

## Usage

``` r
plotColorMixture(
  x,
  w = rep(1, times = length(x))/length(x),
  mixingMethod = c("exact", "reference"),
  n = 1,
  swatch.cex = 1.5,
  label.cex = 0.85,
  showMixedSpec = FALSE,
  overlapFix = TRUE
)
```

## Arguments

- x:

  vector of colors in Munsell notation, should not contain duplicates

- w:

  vector of weights, can sum to any number

- mixingMethod:

  approach used to simulate a mixture:

  - `exact`: simulate a subtractive mixture of pigments, color
    conversion via CIE1931 color-matching functions (see
    [`mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.md))

  - `reference` : simulate a subtractive mixture of pigments, selecting
    `n` closest reference spectra, requires `gower` package

- n:

  number of closest mixture candidates when `mixingMethod = 'reference'`
  (see
  [`mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.md)),
  results can be hard to interpret when `n > 2`

- swatch.cex:

  scaling factor for color swatch rectangle width and height, relative
  to `label.cex`, typically between 1 and 3

- label.cex:

  scaling factor for swatch labels

- showMixedSpec:

  show weighted geometric mean (mixed) spectra as dotted line (only when
  `mixingMethod = 'reference'`)

- overlapFix:

  attempt to "fix" overlapping chip labels via
  [`fixOverlap()`](https://ncss-tech.github.io/aqp/reference/fixOverlap.md),
  using `method = 'E'`

## Value

a `lattice` graphics object

## Details

If present, `names` attribute of `x` is used for the figure legend. See
the [expanded
tutorial](https://ncss-tech.github.io/AQP/aqp/mix-colors.html) for
examples.

## See also

[`mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.md)

## Author

D.E. Beaudette
