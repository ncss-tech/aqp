# Matrix/data.frame-like access to profiles and horizons in a SoilProfileCollection

You can access the contents of a SoilProfileCollection by profile and
horizon "index", `i` and `j`, respectively: `spc[i, j, ...]`. Subset
operations are propagated to other slots (such as diagnostics or
spatial) when they result in removal of sites from a collection.

- `i` refers to the profile position within the collection. By default
  the order is based on the C SORT order of the variable that you
  specified as your unique profile ID at time of object construction.
  Note that if your ID variable was numeric, then it has been sorted as
  a character.

- `j` refers to the horizon or "slice" index. This index is most useful
  when either a) working with `slice`'d SoilProfileCollection or b)
  working with single-profile collections. `j` returns the layer in the
  specified index positions for all profiles in a collection.

- `...` is an area to specify an expression that is evaluated in the
  subset. Currently supported

  - `.LAST` (last horizon in each profile): return the last horizon from
    each profile. This uses `i` but ignores the regular `j` index.

  - `.FIRST` (first horizon in each profile): return the last horizon
    from each profile. This uses `i` but ignores the regular `j` index.

  - `.HZID` (horizon index): return the horizon indices corresponding to
    `i`+`j`+`...` ("k") constraints

  - `.NHZ` (number of horizons): return the number of horizons in the
    profiles resulting from `i`+`j`+`...` ("k") constraints

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  a SoilProfileCollection

- i:

  a numeric or logical value denoting profile indices to select in a
  subset

- j:

  a numeric or logical value denoting horizon indices to select in a
  subset

- ...:

  non-standard expressions to evaluate in a subset

- drop:

  Default: `TRUE`. When `drop=FALSE` placeholder horizons (profile ID
  with all other values `NA`) are created where the specified `j` index
  results in removal of all horizons.
