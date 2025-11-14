# Calculate Thickness of Horizons Matching Logical Criteria

This function calculates the cumulative (`method="cumulative"`, default)
or maximum difference between (`method="minmax"`) horizons within a
profile that match a defined pattern (`pattern`) or, more generally, any
set of horizon-level logical expressions encoded in a function (`FUN`).

## Usage

``` r
thicknessOf(
  x,
  pattern = NULL,
  hzdesgn = hzdesgnname(x, required = TRUE),
  method = "cumulative",
  prefix = "",
  thickvar = "thickness",
  depthvars = horizonDepths(x),
  FUN = function(x, pattern, hzdesgn, ...) grepl(pattern, x[[hzdesgn]]),
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  A *SoilProfileCollection*

- pattern:

  *character*. A pattern to match in `hzdesgn`; used with the default
  `FUN` definition for regular expression pattern matching on horizons.

- hzdesgn:

  *character*. A column containing horizon designations or other
  horizon-level character label used to identify matches; used with the
  default `FUN` definition.

- method:

  *character*. Either `"cumulative"` (default) or `"minmax"`. See
  details.

- prefix:

  *character*. Column prefix for calculated `thickvar` (and `depthvar`
  for `method="minmax"`) column results. Default: `""`.

- thickvar:

  *character* Length `1`. Column name to use for calculated thickness
  column. Default: `"thickness"`

- depthvars:

  *character*. Length `2`. Column names to use for calculated minimum
  top depth and maximum bottom depth in `method="minmax"`. Default:
  `horizonDepths(x)`

- FUN:

  *function*. A function that returns a *logical* vector equal in length
  to the number of horizons in `x`. See details.

- na.rm:

  *logical*. Omit `NA` values in summaries of thickness and in matching?
  Default: `FALSE`

- ...:

  Additional arguments passed to the matching function `FUN`.

## Value

A *data.frame*-like object (corresponding to the
[`aqp_df_class()`](https://ncss-tech.github.io/aqp/reference/aqp_df_class.md)
of `x`) with one row per profile in `x`. First column is always the
profile ID which is followed by `"thickness"`. In `method="minmax"` the
upper and lower boundaries used to calculate `"thickness"` are also
returned as `"tmin"` and `"tmax"` columns, respectively.

## Details

The two thickness methods currently available are:

- `method="cumulative"` (default): cumulative thickness of horizons
  where `FUN` returns true

- `method="minmax"`: maximum bottom depth minus minimum top depth of
  horizons where `FUN` returns true

If a custom function (`FUN`) is used, it should accept arbitrary
additional arguments via an ellipsis (`...`). It is not necessary to do
anything with arguments, but the result should match the number of
horizons found in the input SoilProfileCollection `x`.

## Examples

``` r
data("jacobs2000")

# cumulative thickness of horizon designations matching "Bt"
thicknessOf(jacobs2000, "Bt")
#>     id thickness
#> 1 92-1       110
#> 2 92-2        99
#> 3 92-3       111
#> 4 92-4         0
#> 5 92-5        26
#> 6 92-6        64
#> 7 92-7         0

# maximum bottom depth minus minimum top depth of horizon designations matching "Bt"
thicknessOf(jacobs2000, "Bt", prefix = "Bt_", method = "minmax")
#>     id Bt_top Bt_bottom Bt_thickness
#> 1 92-1     43       153          110
#> 2 92-2     46       145           99
#> 3 92-3     64       175          111
#> 4 92-4    Inf      -Inf            0
#> 5 92-5    109       135           26
#> 6 92-6    104       168           64
#> 7 92-7    Inf      -Inf            0

# cumulative thickness of horizon designations matching "A|B"
thicknessOf(jacobs2000, "A|B", prefix = "AorB_")
#>     id AorB_thickness
#> 1 92-1            131
#> 2 92-2            117
#> 3 92-3            136
#> 4 92-4             20
#> 5 92-5             54
#> 6 92-6            110
#> 7 92-7             43

# maximum bottom depth minus minimum top depth of horizon designations matching "A|B"
thicknessOf(jacobs2000, "A|B", method = "minmax", prefix = "AorB_")
#>     id AorB_top AorB_bottom AorB_thickness
#> 1 92-1        0         156            156
#> 2 92-2        0         145            145
#> 3 92-3        0         175            175
#> 4 92-4        0          20             20
#> 5 92-5        0         135            135
#> 6 92-6        0         168            168
#> 7 92-7        0         140            140
# note that the latter includes the thickness of E horizons between the A and the B

# when using a custom function (be sure to accept ... and consider the effect of NA values)

# calculate cumulative thickness of horizons containing >18% clay
thicknessOf(jacobs2000, prefix = "claygt18_", 
            FUN = function(x, ...) !is.na(x[["clay"]]) & x[["clay"]] > 18)
#>     id claygt18_thickness
#> 1 92-1                170
#> 2 92-2                167
#> 3 92-3                 81
#> 4 92-4                  0
#> 5 92-5                  0
#> 6 92-6                 49
#> 7 92-7                  0
```
