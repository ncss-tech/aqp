# Efficient Slicing of `SoilProfileCollection` Objects

Cut ("dice") soil horizons into 1-unit thick slices. This function
replaces
[`aqp::slice()`](https://ncss-tech.github.io/aqp/reference/slice.md),
which will be deprecated in aqp 2.0.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
dice(
  x,
  fm = NULL,
  SPC = TRUE,
  pctMissing = FALSE,
  fill = FALSE,
  strict = TRUE,
  byhz = TRUE,
  verbose = FALSE
)
```

## Arguments

- x:

  a `SoilProfileCollection` object

- fm:

  optional `formula` describing top depths and horizon level attributes
  to include: `integer.vector ~ var1 + var2 + var3` or
  `integer.vector ~ .` to include all horizon level attributes.
  Specification of `integer.vector` forces `fill = TRUE`. When `NULL`
  profiles are "diced" to depth and results will include all horizon
  level attributes. Note on interpretation of `integer.vector` (slice
  tops)

- SPC:

  return the diced `SoilPrfolileCollection`, if `FALSE` a `data.frame`
  of horizon-level attributes

- pctMissing:

  compute "percent missing data" by slice (when `TRUE` expect 6-8x
  longer run time)

- fill:

  logical, fill with empty placeholder horizons in gaps within profiles,
  and/or, above/below interval specified in `fm`. Automatically set to
  `TRUE` when LHS of `fm` is specified. Backwards compatibility with
  `slice` is maintained by setting `fill = TRUE` with or without `fm`.

- strict:

  perform horizon depth logic checking / flagging / removal

- byhz:

  Evaluate horizon depth logic at the horizon level (`TRUE`) or profile
  level (`FALSE`). Invalid depth logic invokes `HzDepthLogicSubset`
  which removes offending profiles or horizon records.

- verbose:

  Print information about object size/memory usage. Default: `FALSE`

## Value

a `SoilProfileCollection` object, or `data.frame` when `SPC = FALSE`

## Details

For large and potentially messy collections that may include missing
horizon depth logic errors, consider using
[`repairMissingHzDepths()`](https://ncss-tech.github.io/aqp/reference/repairMissingHzDepths.md)
before `dice()`. Consider using
[`accumulateDepths()`](https://ncss-tech.github.io/aqp/reference/accumulateDepths.md)
before invoking `dice()` on collections that may contain old-style O
horizon notation (e.g. 5-0cm).

## See also

[`repairMissingHzDepths()`](https://ncss-tech.github.io/aqp/reference/repairMissingHzDepths.md),
[`accumulateDepths()`](https://ncss-tech.github.io/aqp/reference/accumulateDepths.md),
[`fillHzGaps()`](https://ncss-tech.github.io/aqp/reference/fillHzGaps.md)

## Author

D.E. Beaudette and A.G. Brown
