# Transform a SPC (by profile) with a set of expressions

`mutate_profile()` is a function used for transforming
SoilProfileCollections. Each expression is applied to site or horizon
level attributes of individual profiles. This distinguishes this
function from `transform`, which is applied to all values in a
collection, regardless of which profile they came from.

## Usage

``` r
mutate_profile(object, ..., col_names = NULL, horizon_level = NULL)

mutate_profile_raw(object, expr, col_names = NULL, horizon_level = NULL)
```

## Arguments

- object:

  A SoilProfileCollection

- ...:

  A set of comma-delimited R expressions that resolve to a
  transformation to be applied to a single profile e.g
  `mutate_profile(hzdept = max(hzdept) - hzdept)`

- col_names:

  character. Optional column names. Should match the number of
  expressions in `...`.

- horizon_level:

  logical. If `TRUE` results of expressions are added to the
  SoilProfileCollection's horizon slot, if `FALSE` the results are added
  to the site slot. If `NULL` (default) the results are stored in the
  site or horizon slot based on the number of rows in each slot compared
  to the length of the result calculated from the *first* and *last*
  profile in the collection.

- expr:

  A list of expressions in terms of column names in site or horizon
  table of `object`

## Value

A SoilProfileCollection.

## Details

If the length an expression's result matches the number of horizons, the
result is stored as a horizon-level variable. If the result has length
1, it is stored as a site-level variable. In the ambiguous case where
the first and last profile have only *one* horizon, the results are
stored in the horizon slot by default. To force results into site slot
use `horizon_level = FALSE`.

## Author

Andrew G. Brown.

## Examples

``` r
data(sp4)
depths(sp4) <- id ~ top + bottom

mutate_profile(sp4, clay_wtd_average = weighted.mean(clay, bottom - top))
#> SoilProfileCollection with 10 profiles and 30 horizons
#> profile ID: id  |  horizon ID: hzID 
#> Depth range: 16 - 49 cm
#> 
#> ----- Horizons (6 / 30 rows  |  10 / 14 columns) -----
#>      id hzID top bottom name   K   Mg  Ca CEC_7 ex_Ca_to_Mg
#>  colusa    1   0      3    A 0.3 25.7 9.0  23.0        0.35
#>  colusa    2   3      8  ABt 0.2 23.7 5.6  21.4        0.23
#>  colusa    3   8     30  Bt1 0.1 23.2 1.9  23.7        0.08
#>  colusa    4  30     42  Bt2 0.1 44.3 0.3  43.0        0.01
#>   glenn    5   0      9    A 0.2 21.9 4.4  18.8        0.20
#>   glenn    6   9     34   Bt 0.3 18.9 4.5  27.5        0.20
#> [... more horizons ...]
#> 
#> ----- Sites (6 / 10 rows  |  2 / 2 columns) -----
#>         id clay_wtd_average
#>     colusa         37.19048
#>      glenn         31.61765
#>      kings         21.90000
#>   mariposa         30.32653
#>  mendocino         21.93333
#>       napa         16.40000
#> [... more sites ...]
#> 
#> Spatial Data:
#> [EMPTY]

data(jacobs2000)

set.seed(123)

## col_names allows for column names to be calculated
x <- mutate_profile(jacobs2000, bottom - top / sum(bottom - top),
                    col_names = paste0("relthk", floor(runif(1, 0, 100))))
x$relthk28
#>  [1]  18.00000  42.91549  78.79812 129.62911 152.38967 155.28169 212.26761
#>  [8]  18.00000  45.91549  83.78404 121.60563 144.42723 212.31925  15.00000
#> [15]  24.91429  63.85714  83.63429 111.52000 164.36000 174.05714  20.00000
#> [22]  52.90148  78.73892 129.61084 164.35961 184.18719 202.08867  28.00000
#> [29]  60.84699 108.66667 134.40437 182.26230  18.00000  45.89286  75.72619
#> [36] 103.54762 118.38095 167.29167  15.00000  40.90132  47.73026  60.68421
#> [43]  90.59868 131.40132 139.13158 151.07895

# mutate_profile_raw allows for lists of expressions to be evaluated
master_desgn <- c("O", "A", "E", "B", "C", "R", "L", "M")
thk_names <- paste0("thk_", master_desgn)

# calculate thickness for each horizon
x$thk <- x$bottom - x$top

## construct an arbitrary number of expressions using variable inputs
ops <- lapply(master_desgn, function(x) {
  substitute(sum(thk[grepl(PATTERN, name)], na.rm = TRUE), list(PATTERN = x))
})
names(ops) <- thk_names

# do mutation
y <- mutate_profile_raw(x, ops)

site(y)[c(idname(y), thk_names)]
#>     id thk_O thk_A thk_E thk_B thk_C thk_R thk_L thk_M
#> 1 92-1     0    18    25   113    60     0     0     0
#> 2 92-2     0    18    28    99    68     0     0     0
#> 3 92-3     0    25    49   111     0     0     0     0
#> 4 92-4     0    20     0     0   183     0     0     0
#> 5 92-5     0    28    81    26    48     0     0     0
#> 6 92-6     0    46    86    64     0     0     0     0
#> 7 92-7     0    15    97    28    12     0     0     0
```
