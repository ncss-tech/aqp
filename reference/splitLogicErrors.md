# Split a `SoilProfileCollection` into a list based on types of horizon logic errors

Uses
[`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md)
to identify presence of depth logic errors, same depths, missing depths,
and overlaps/gaps between the horizons of each profile in a
`SoilProfileCollection.`

## Usage

``` r
splitLogicErrors(object, interact = FALSE, ...)
```

## Arguments

- object:

  A `SoilProfileCollection`

- interact:

  Calculate interaction between the four logic errors for groups?
  Default: `FALSE` always returns 4 groups, one for each logic error
  type.

- ...:

  Additional arguments to `split.default`, called when `interact = TRUE`

## Value

A named list of `SoilProfileCollections` (or `NULL`), with names:
"depthLogic", "sameDepth", "missingDepth", "overlapOrGap". If
`interact = TRUE` then the list elements groups determined by
[`interaction()`](https://rdrr.io/r/base/interaction.html) of the error
types.

## Examples

``` r
data(sp4)
depths(sp4) <- id ~ top + bottom

# no errors (all four list elements return NULL)
splitLogicErrors(sp4)
#> $depthLogic
#> NULL
#> 
#> $sameDepth
#> NULL
#> 
#> $missingDepth
#> NULL
#> 
#> $overlapOrGap
#> NULL
#> 

# NA in top depth triggers depth logic and missing depth errors
data(sp4)
sp4$top[1] <- NA
depths(sp4) <- id ~ top + bottom
#> This is already a SoilProfileCollection-class object, doing nothing.

splitLogicErrors(sp4)
#> $depthLogic
#> SoilProfileCollection with 1 profiles and 4 horizons
#> profile ID: id  |  horizon ID: hzID 
#> Depth range: 42 - 42 cm
#> 
#> ----- Horizons (4 / 4 rows  |  10 / 14 columns) -----
#>      id hzID top bottom name   K   Mg  Ca CEC_7 ex_Ca_to_Mg
#>  colusa    1  NA      3    A 0.3 25.7 9.0  23.0        0.35
#>  colusa    2   3      8  ABt 0.2 23.7 5.6  21.4        0.23
#>  colusa    3   8     30  Bt1 0.1 23.2 1.9  23.7        0.08
#>  colusa    4  30     42  Bt2 0.1 44.3 0.3  43.0        0.01
#> 
#> ----- Sites (1 / 1 rows  |  1 / 1 columns) -----
#>      id
#>  colusa
#> 
#> Spatial Data:
#> [EMPTY]
#> 
#> $sameDepth
#> NULL
#> 
#> $missingDepth
#> SoilProfileCollection with 1 profiles and 4 horizons
#> profile ID: id  |  horizon ID: hzID 
#> Depth range: 42 - 42 cm
#> 
#> ----- Horizons (4 / 4 rows  |  10 / 14 columns) -----
#>      id hzID top bottom name   K   Mg  Ca CEC_7 ex_Ca_to_Mg
#>  colusa    1  NA      3    A 0.3 25.7 9.0  23.0        0.35
#>  colusa    2   3      8  ABt 0.2 23.7 5.6  21.4        0.23
#>  colusa    3   8     30  Bt1 0.1 23.2 1.9  23.7        0.08
#>  colusa    4  30     42  Bt2 0.1 44.3 0.3  43.0        0.01
#> 
#> ----- Sites (1 / 1 rows  |  1 / 1 columns) -----
#>      id
#>  colusa
#> 
#> Spatial Data:
#> [EMPTY]
#> 
#> $overlapOrGap
#> NULL
#> 

# interact = TRUE gets errors for profile 1 in same group
#  and allows you to pass extra arguments to split.default()
splitLogicErrors(sp4, interact = TRUE, sep = "_", drop = TRUE)
#> $`___`
#> SoilProfileCollection with 9 profiles and 26 horizons
#> profile ID: id  |  horizon ID: hzID 
#> Depth range: 16 - 49 cm
#> 
#> ----- Horizons (6 / 26 rows  |  10 / 14 columns) -----
#>        id hzID top bottom name   K   Mg  Ca CEC_7 ex_Ca_to_Mg
#>     glenn    5   0      9    A 0.2 21.9 4.4  18.8        0.20
#>     glenn    6   9     34   Bt 0.3 18.9 4.5  27.5        0.20
#>     kings    7   0      4    A 0.2 12.1 1.4  23.7        0.58
#>     kings    8   4     13  Bt1 0.6 12.1 7.0  18.0        0.51
#>     kings    9  13     40  Bt2 0.8 17.7 4.4  20.0        0.25
#>  mariposa   10   0      3    A 0.6 28.3 5.8  29.3        0.20
#> [... more horizons ...]
#> 
#> ----- Sites (6 / 9 rows  |  1 / 1 columns) -----
#>          id
#>       glenn
#>       kings
#>    mariposa
#>   mendocino
#>        napa
#>  san benito
#> [... more sites ...]
#> 
#> Spatial Data:
#> [EMPTY]
#> 
#> $depthLogic__missingDepth_
#> SoilProfileCollection with 1 profiles and 4 horizons
#> profile ID: id  |  horizon ID: hzID 
#> Depth range: 42 - 42 cm
#> 
#> ----- Horizons (4 / 4 rows  |  10 / 14 columns) -----
#>      id hzID top bottom name   K   Mg  Ca CEC_7 ex_Ca_to_Mg
#>  colusa    1  NA      3    A 0.3 25.7 9.0  23.0        0.35
#>  colusa    2   3      8  ABt 0.2 23.7 5.6  21.4        0.23
#>  colusa    3   8     30  Bt1 0.1 23.2 1.9  23.7        0.08
#>  colusa    4  30     42  Bt2 0.1 44.3 0.3  43.0        0.01
#> 
#> ----- Sites (1 / 1 rows  |  1 / 1 columns) -----
#>      id
#>  colusa
#> 
#> Spatial Data:
#> [EMPTY]
#> 
```
