# Soil Profile Color Signatures

Generate a color signature for each soil profile in a collection.

## Usage

``` r
soilColorSignature(
  spc,
  color,
  space = c("sRGB", "CIELAB"),
  method = c("colorBucket", "depthSlices", "pam"),
  perceptualDistMat = FALSE,
  pam.k = 3,
  prob = c(0.1, 0.5, 0.9),
  useProportions = TRUE,
  pigmentNames = c(".white.pigment", ".red.pigment", ".green.pigment", ".yellow.pigment",
    ".blue.pigment"),
  apply.fun = lapply,
  r = NULL,
  g = NULL,
  b = NULL,
  RescaleLightnessBy = NULL
)
```

## Arguments

- spc:

  a `SoilProfileCollection` object

- color:

  horizon-level attributes, either character of length 1 specifying a
  column containing Munsell or sRGB in hex notation, or character vector
  of three column names containing either sRGB or CIELAB color
  coordinates. sRGB color coordinates should be within the range of 0 to
  1.

- space:

  character, either 'sRGB' or 'LAB', specifying color space

- method:

  algorithm used to compute color signature, `colorBucket`,
  `depthSlices`, or `pam`

- perceptualDistMat:

  logical, optionally return a distance matrix based on perceptual color
  distances, when “method\` is one of 'depthSlices' or 'pam', see
  Details

- pam.k:

  number of color classes for `method = 'pam'`

- prob:

  numeric vector, requested percentiles for `method = 'depthSlices'`

- useProportions:

  use proportions or quantities, see details

- pigmentNames:

  names for resulting pigment proportions or quantities

- apply.fun:

  function passed to `aqp::profileApply(APPLY.FUN)` argument, can be
  used to add progress bars via `pbapply::pblapply()`, or parallel
  processing with `furrr::future_map()`

- r:

  deprecated, use `color` argument

- g:

  deprecated, use `color` argument

- b:

  deprecated, use `color` argument

- RescaleLightnessBy:

  deprecated, scaling factor for CIELAB L-coordinate

## Value

For the `colorBucket` method, a `data.frame`:

- id column: set according to `idname(spc)`

- `.white.pigment`: proportion or quantity of CIELAB L-values

- `.red.pigment`: proportion or quantity of CIELAB positive A-values

- `.green.pigment`: proportion or quantity of CIELAB negative A-values

- `.yellow.pigment`: proportion or quantity of CIELAB positive B-values

- `.blue.pigment`: proportion or quantity of CIELAB negative B-values

Column names can be adjusted with the `pigmentNames` argument.

For the `depthSlices` method, a `data.frame`:

- id column: set according to `idname(spc)`

- `L.1`, `A.1`, `B.1`: CIELAB color coordinates associated with the
  first depth slice, at depth percentile given in `prob[1]`

- ...

- `L.n`, `A.n`, `B.n`: CIELAB color coordinates associated with the `n`
  depth slice, at depth percentile given in `prob[n]`

For the `pam` method, a `data.frame`:

- id column: set according to `idname(spc)`

- `L.1`, `A.1`, `B.1`: CIELAB color coordinates associated with the
  first color cluster, after sorting all clusters in ascending order
  along L, A, B axes.

- ...

- `L.n`, `A.n`, `B.n`: CIELAB color coordinates associated with the
  `nth` color cluster, after sorting all clusters in ascending order
  along L, A, B axes.

When `perceptualDistMat = TRUE` and `method` is one of 'depthSlices' or
'pam', a distance matrix is returned.

## Details

Interpreation of color signature.

Choices related to weighting, scaling, and distance metric.

Perceptual distances (dE00), summed over color groups.

See the [related
tutorial](http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.md).

## References

https://en.wikipedia.org/wiki/Lab_color_space

## See also

[`plotProfileDendrogram()`](https://ncss-tech.github.io/aqp/reference/plotProfileDendrogram.md)

## Author

D.E. Beaudette

## Examples

``` r
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# Munsell notation
sp1$m <- sprintf("%s %s/%s", sp1$hue, sp1$value, sp1$chroma)

# extract color signature
pig <- soilColorSignature(sp1, color = 'm')
```
