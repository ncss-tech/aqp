# Compare Site Level Attributes of a SoilProfileCollection

Compare site level attributes of a `SoilProfileCollection` object,
returning a distance matrix conformal with the output from
[`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md). Values
are within the range of 0-1.

## Usage

``` r
compareSites(x, vars, weights = rep(1, times = length(vars)), ...)
```

## Arguments

- x:

  `SoilProfileCollection` object

- vars:

  character vector listing one or more site level attributes of `x`

- weights:

  numeric vector, same length as `vars`, variable weighting

- ...:

  additional arguments to
  [`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html)

## Value

`dissimilarity` / `dist` class object containing pair-wise distances,
row/column names derived from `profile_id(x)`

## Details

This function is typically used in conjunction with the output from
[`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md).

## See also

[`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md)
[`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html)
