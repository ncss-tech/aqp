# Horizons Above or Below

Horizons Above or Below

## Usage

``` r
hzAbove(x, ..., offset = 1, SPC = TRUE, simplify = SPC)

hzBelow(x, ..., offset = 1, SPC = TRUE, simplify = SPC)

hzOffset(x, hzid, offset, SPC = FALSE, simplify = TRUE)
```

## Arguments

- x:

  A SoilProfileCollection

- ...:

  Comma-separated set of R expressions that evaluate as `TRUE` or
  `FALSE` in context of horizon data frame. Length for individual
  expressions matches number of horizons, in `x`.

- offset:

  Integer offset in terms of SoilProfileCollection `[,j]`
  (horizon/slice) index

- SPC:

  Return a SoilProfileCollection? Default `TRUE` for `horizon_*`
  methods.

- simplify:

  If `TRUE` return a vector (all elements combined), or a list (1
  element per profile). If `SPC` is `TRUE` then `simplify` is `TRUE`.

- hzid:

  A vector of target horizon IDs. These are calculated from `...` for
  `horizon_*()` methods

## Value

A SoilProfileCollection (when `SPC = TRUE`) or a vector of horizon row
indices (when `SPC = FALSE` and `simplify = TRUE`) or a list (when
`SPC = FALSE` and `simplify = FALSE`))

## Details

To minimize likelihood of issues with non-standard evaluation context,
especially when using `hzAbove()`/`hzBelow()` inside another function,
all expressions used in `...` should be in terms of variables that are
in the horizon data frame.

## Examples

``` r
data(sp4)
depths(sp4) <- id ~ top + bottom

# get the horizon above the last horizon (j-index of bottom horizon minus 1)
hzAbove(sp4, hzID(sp4) %in% getLastHorizonID(sp4))
#> Error: unable to find an inherited method for function ‘hzID’ for signature ‘object = "data.frame"’

# get horizons below the last horizon (none; j-index of bottom horizon plus 1)
hzBelow(sp4, hzID(sp4) %in% getLastHorizonID(sp4))
#> Error: unable to find an inherited method for function ‘hzID’ for signature ‘object = "data.frame"’
```
