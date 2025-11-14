# Annotate Diagnostic Features

Annotate diagnostic features within a sketch of soil profiles.

## Usage

``` r
addDiagnosticBracket(
  s,
  kind,
  feature = "featkind",
  top = "featdept",
  bottom = "featdepb",
  ...
)
```

## Arguments

- s:

  `SoilProfileCollection` object

- kind:

  filter applied to `feature` column of diagnostic horizons registered
  within `s`

- feature:

  column name containing feature kind

- top:

  column name containing feature top depth

- bottom:

  column name containing feature top depth

- ...:

  additional arguments passed to `addBracket`

## Details

Additional examples can be found in [this
tutorial](http://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.md).

## Note

This is a `low-level` plotting function: you must first plot a
`SoilProfileCollection` object before using this function.

## See also

[`addBracket()`](https://ncss-tech.github.io/aqp/reference/addBracket.md),
[`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)

## Author

D.E. Beaudette
