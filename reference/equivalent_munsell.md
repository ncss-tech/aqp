# Indices of "equivalent" Munsell chips in the `munsell` data set

A pre-calculated lookup list (made with
[`farver::compare_colour`](https://farver.data-imaginist.com/reference/compare_colour.html))
based on pair-wise color contrast (`CIE2000` or `dE00`) evaluated over
all "chips" in the
[`aqp::munsell`](https://ncss-tech.github.io/aqp/reference/munsell.md)
data set.

The intention is to identify Munsell chips that may be "functionally
equivalent" to some other given whole chip elsewhere in the Munsell
color space – as discretized in the
[`aqp::munsell`](https://ncss-tech.github.io/aqp/reference/munsell.md)
lookup table.

"Equivalent" chips are based (fairly arbitrarily) on the 0.001
probability level of `dE00` (default Type 7 `quantile`) within the upper
triangle of the 8467x8467 contrast matrix. This corresponds to a `dE00`
threshold of approximately 2.15.

This is a naive (to the subtleties of human color perception, and
overall magnitude of contrast between some of the "chips") but
computationally consistent approach. Using the lookup list, as opposed
to manual contrast via e.g.
[`farver::compare_colour`](https://farver.data-imaginist.com/reference/compare_colour.html)
may have some benefits for efficiency in certain applications where the
exact contrast value is not as important as the concept of having some
threshold that is non-zero, but very small.

## Usage

``` r
data(equivalent_munsell)
```

## Format

A named list with 8467 elements, each containing a numeric vector of
indices corresponding to the `munsell` data set, which has 8467 rows
(unique, whole-number chips). Names have the format `HUE VALUE/CHROMA`,
e.g. `"7.5YR 4/4"`

## References

Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000
Color-Difference Formula: Implementation Notes, Supplementary Test Data,
and Mathematical Observations. COLOR research and application.
30(1):21-30.
http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf

Thomas Lin Pedersen, Berendea Nicolae and Romain Francois (2020).
farver: High Performance Colour Space Manipulation. R package version
2.0.3. https://CRAN.R-project.org/package=farver

Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and
Stern, L.A. (2020). Strengths, Limitations, and Recommendations for
Instrumental Color Measurement in Forensic Soil Characterization. J
Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193

## See also

[`equivalentMunsellChips`](https://ncss-tech.github.io/aqp/reference/equivalentMunsellChips.md)

## Examples

``` r
data(equivalent_munsell)
```
