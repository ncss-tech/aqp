# Select a subset of columns from a SoilProfileCollection

Reduce the number of columns in a `SoilProfileCollection` to a minimal
set plus additional selected columns. Optional metadata columns are
included if set. At a minimum the profile ID, horizon top and bottom
depths, and horizon ID are included. Horizon designation and horizon
texture class column names are included if metadata attributes are set.
See details.

## Usage

``` r
reduceSPC(p, column_names = NULL)
```

## Arguments

- p:

  a `SoilProfileCollection`

- column_names:

  a set of additional columns to include in the result

## Value

a `SoilProfileCollection`

## Details

Minimum column names included (when `column_names = NULL`)

- `idname(p)`, `horizonDepths(p)`, `hzidname(p)`

Optional column names included (when metadata are set)

- `hzdesgnname(p)`, `hztexclname(p)`, `GHL(p)`

## See also

[`hzdesgnname()`](https://ncss-tech.github.io/aqp/reference/hzdesgnname.md)
[`hztexclname()`](https://ncss-tech.github.io/aqp/reference/hztexclname.md)
[`GHL()`](https://ncss-tech.github.io/aqp/reference/GHL.md)
