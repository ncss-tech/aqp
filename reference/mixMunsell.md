# Mix Munsell Colors via Spectral Library

Simulate mixing of colors in Munsell notation, similar to the way in
which mixtures of pigments operate.

## Usage

``` r
mixMunsell(
  x,
  w = rep(1, times = length(x))/length(x),
  mixingMethod = c("exact", "reference", "estimate", "adaptive"),
  n = 1,
  keepMixedSpec = FALSE,
  distThreshold = 0.025,
  ...
)
```

## Arguments

- x:

  vector of colors in Munsell notation

- w:

  vector of proportions, can sum to any number

- mixingMethod:

  approach used to simulate a mixture:

  - `exact`: simulate a subtractive mixture of pigments, color
    conversion via CIE1931 color-matching functions (see details)

  - `reference`: simulate a subtractive mixture of pigments, selecting
    `n` closest reference spectra from
    [`munsell.spectra.wide`](https://ncss-tech.github.io/aqp/reference/munsell.spectra.md)
    (requires `gower` package)

  - `estimate`: closest Munsell chip to a weighted mean of CIELAB
    coordinates (fastest)

  - `adaptive`: use `exact` method when possible, falling-back to
    `estimate` (weighted mean of CIELAB coordinates) otherwise

- n:

  number of closest matching color chips (`mixingMethod = reference`
  only)

- keepMixedSpec:

  keep weighted geometric mean spectra, final result is a `list`
  (`mixingMethod = reference` only)

- distThreshold:

  spectral distance used to compute `scaledDistance`, default value is
  based on an analysis of spectral distances associated with adjacent
  Munsell color chips. This argument is only used with
  `mixingMethod = 'reference'`.

- ...:

  additional arguments to
  [`spec2Munsell`](https://ncss-tech.github.io/aqp/reference/spec2Munsell.md)

## Value

A `data.frame` with the closest matching Munsell color(s):

- `munsell`: Munsell notation of the n-closest spectra

- `distance`: spectral (Gower) distance to the n-closest spectra

- `scaledDistance`: spectral distance scaled by `distThreshold`

- `mixingMethod`: method used for each mixture

When `keepMixedSpec = TRUE` then a `list`:

- `mixed`: a `data.frame` containing the same elements as above

- `spec`: spectra for the 1st closest match

## Details

See the [expanded
tutorial](https://ncss-tech.github.io/AQP/aqp/mix-colors.html) for
examples.

An accurate simulation of pigment mixtures ("subtractive" color
mixtures) is incredibly complex due to factors that aren't easily
measured or controlled: pigment solubility, pigment particle size
distribution, water content, substrate composition, and physical
obstruction to name a few. That said, it is possible to simulate
reasonable, subtractive color mixtures given a reference spectra library
(350-800nm) and some assumptions about pigment qualities and lighting.
For the purposes of estimating a mixture of soil colors (these are
pigments after all) we can relax these assumptions and assume a standard
light source. The only missing piece is the spectral library for all
Munsell chips in our color books.

Thankfully, [Scott Burns has outlined the entire
process](https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf), and
Paul Centore has provided a Munsell color chip reflectance spectra
library (http://www.munsellcolourscienceforpainters.com). The estimation
of a subtractive mixture of soil colors can proceed as follows:

1.  look up the associated spectra for each color in `x`

2.  compute the weighted (`w` argument) geometric mean of the spectra

3.  convert the spectral mixture to the closest Munsell color via:

- search for the closest `n` matching spectra in the reference library
  (`mixtureMethod = 'reference'`)

- direct conversion of spectra to closest Munsell color via
  [`spec2Munsell()`](https://ncss-tech.github.io/aqp/reference/spec2Munsell.md)
  (`mixtureMethod = 'exact'`)

1.  suggest resulting Munsell chip(s) as the best candidate for a
    simulated mixture

Key assumptions include:

- similar particle size distribution

- similar mineralogy (i.e. pigmentation qualities)

- similar water content.

For the purposes of estimating (for example) a "mixed soil color within
the top 18cm of soil" these assumptions are usually valid. Again, these
are estimates that are ultimately "snapped" to the nearest chip and not
do not need to approach the accuracy of paint-matching systems.

A message is printed when `scaledDistance` is larger than 1.

## References

Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color
for Science, Art, and Technology (pp. 32-96). North-Holland.

- [inspiration / calculations based on the work of Scott
  Burns](https://arxiv.org/ftp/arxiv/papers/1710/1710.06364.pdf)

- [related discussion on Stack
  Overflow](https://stackoverflow.com/questions/10254022/implementing-kubelka-munk-like-krita-to-mix-colours-color-like-paint/29967630#29967630)

- spectral library source:
  http://www.munsellcolourscienceforpainters.com/MunsellResources/SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt

## See also

[munsell.spectra](https://ncss-tech.github.io/aqp/reference/munsell.spectra.md)

## Author

D.E. Beaudette
