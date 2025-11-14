# Subset `SoilProfileCollection` Objects or Horizons via `checkHzDepthLogic`

This function removes profiles or horizons from a
`SoilProfileCollection` that are flagged as having invalid horizon depth
logic by
[`checkHzDepthLogic`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md).
Invalid profiles may be created when setting `byhz = TRUE`; use caution
as some functions may not work properly in the presence of gaps.
Consider using
[`fillHzGaps`](https://ncss-tech.github.io/aqp/reference/fillHzGaps.md)
to fill these gaps.

## Usage

``` r
HzDepthLogicSubset(x, byhz = FALSE)
```

## Arguments

- x:

  a `SoilProfileCollection` object

- byhz:

  logical, evaluate horizon depth logic at the horizon level (profile
  level if `FALSE`)

## Value

a `SoilProfileCollection` object

## Note

This function cannot identify (and remove) overlapping horizons when
`byhz = TRUE`.
