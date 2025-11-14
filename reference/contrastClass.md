# Soil Color Contrast

Determine soil color contrast class according to methods outlined in the
Soil Survey Manual. This function is typically called from
[`colorContrast()`](https://ncss-tech.github.io/aqp/reference/colorContrast.md)
which is simpler to use and provides more information.

## Usage

``` r
contrastClass(v1, c1, v2, c2, dH, dV, dC, verbose = FALSE)
```

## Arguments

- v1:

  Munsell value of first color

- c1:

  Munsell chroma of first color

- v2:

  Munsell value of second color

- c2:

  Munsell chroma of second color

- dH:

  delta Hue

- dV:

  delta Value

- dC:

  delta Chroma

- verbose:

  return a list for testing rules/cases

## Value

A vector of color contrast classes (ordered factor). A list when
`verbose` is TRUE.

## Details

This function is fully vectorized but expects all inputs have the same
length.

## References

- Soil Survey Technical Note 2 [wayback machine
  URL](https://web.archive.org/web/20220704214918/https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569)

## See also

[`colorContrast`](https://ncss-tech.github.io/aqp/reference/colorContrast.md)

## Author

D.E. Beaudette

## Examples

``` r
## standard use, result is an ordered factor
# 10YR 6/3 vs 5YR 3/4
contrastClass(v1=6, c1=3, v2=3, c2=4, dH=2, dV=3, dC=1)
#> [1] Prominent
#> Levels: Faint < Distinct < Prominent

## verbose output, useful for testing rules/cases
# 10YR 6/3 vs 5YR 3/4
contrastClass(v1=6, c1=3, v2=3, c2=4, dH=2, dV=3, dC=1, verbose = TRUE)
#> $faint
#>   v1 c1 v2 c2 dH dV dC f.case1 f.case2 f.case3 low.value.chroma       res
#> 1  6  3  3  4  2  3  1   FALSE   FALSE   FALSE            FALSE Prominent
#> 
#> $distinct
#>   v1 c1 v2 c2 dH dV dC d.case1 d.case2 d.case3       res
#> 1  6  3  3  4  2  3  1   FALSE   FALSE   FALSE Prominent
#> 
```
