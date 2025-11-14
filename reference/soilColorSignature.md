# Soil Profile Color Signatures

Generate a color signature for each soil profile in a collection.

## Usage

``` r
soilColorSignature(
  spc,
  r = "r",
  g = "g",
  b = "b",
  method = c("colorBucket", "depthSlices", "pam"),
  pam.k = 3,
  RescaleLightnessBy = 1,
  useProportions = TRUE,
  pigmentNames = c(".white.pigment", ".red.pigment", ".green.pigment", ".yellow.pigment",
    ".blue.pigment"),
  apply.fun = lapply
)
```

## Arguments

- spc:

  a `SoilProfileCollection` object

- r:

  horizon level attribute containing soil color (sRGB) red values

- g:

  horizon level attribute containing soil color (sRGB) green values

- b:

  horizon level attribute containing soil color (sRGB) blue values

- method:

  algorithm used to compute color signature, `colorBucket`,
  `depthSlices`, or `pam`

- pam.k:

  number of classes to request from
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)

- RescaleLightnessBy:

  rescaling factor for CIE LAB L-coordinate

- useProportions:

  use proportions or quantities, see details

- pigmentNames:

  names for resulting pigment proportions or quantities

- apply.fun:

  function passed to `aqp::profileApply(APPLY.FUN)` argument, can be
  used to add progress bars via `pbapply::pblapply`, or parallel
  processing with `furrr::future_map`

## Value

For the `colorBucket` method, a `data.frame` object containing:

- id column: set according to `idname(spc)`

- `.white.pigment`: proportion or quantity of CIE LAB L-values

- `.red.pigment`: proportion or quantity of CIE LAB positive A-values

- `.green.pigment`: proportion or quantity of CIE LAB negative A-values

- `.yellow.pigment`: proportion or quantity of CIE LAB positive B-values

- `.blue.pigment`: proportion or quantity of CIE LAB negative B-values

Column names can be adjusted with the `pigmentNames` argument.

For the `depthSlices` method ...

For the `pam` method ...

## Details

See the [related
tutorial](http://ncss-tech.github.io/AQP/aqp/soil-color-signatures.md).

## References

https://en.wikipedia.org/wiki/Lab_color_space

## See also

[`munsell2rgb`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md)

## Author

D.E. Beaudette

## Examples

``` r
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# convert Munsell -> sRGB triplets
rgb.data <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
sp1$r <- rgb.data$r
sp1$g <- rgb.data$g
sp1$b <- rgb.data$b

# extract color signature
pig <- soilColorSignature(sp1)
```
