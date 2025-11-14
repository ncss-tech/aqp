# Find lagged horizon values

This function finds adjacent values to a horizon values at lagged
distances.

## Usage

``` r
hz_lag(
  object,
  lag = 1,
  unit = "index",
  idcol = "id",
  depthcols = c("top", "bottom"),
  order = FALSE
)
```

## Arguments

- object:

  a `data.frame`

- lag:

  integer: number of horizons to lag

- unit:

  character: lag units in index or depth.

- idcol:

  character: column name of the pedon ID within the object.

- depthcols:

  a character vector of length 2 specifying the names of the horizon
  depths (e.g. `c("top", "bottom")`).

- order:

  logical: indicating whether or not to order the \#'

## Value

A `data.frame` with lagged values.

## Details

.

## See also

[`hz_dissolve()`](https://ncss-tech.github.io/aqp/reference/hz_dissolve.md),
[`hz_intersect()`](https://ncss-tech.github.io/aqp/reference/hz_intersect.md),
[`hz_segment()`](https://ncss-tech.github.io/aqp/reference/hz_segment.md)

## Author

Stephen Roecker

## Examples

``` r
h <- data.frame(
  id = 1,
  top    = c(0,  25, 44, 46, 50),
  bottom = c(25, 44, 46, 50, 100),
  texcl     = c("SL", "SL", "CL", "CL", "L"),
  clay   = c(10, 12, 27, 35, 16)
)

hz_lag(h)
#>    clay_bot.1 texcl_bot.1
#> 2          12          SL
#> 3          27          CL
#> 4          35          CL
#> 5          16           L
#> NA         NA        <NA>

hz_lag(h, -1)
#>    clay_top.1 texcl_top.1
#> NA         NA        <NA>
#> 1          10          SL
#> 2          12          SL
#> 3          27          CL
#> 4          35          CL

hz_lag(h, 10:15, unit = "depth")
#>     clay_bot.10 clay_bot.11 clay_bot.12 clay_bot.13 clay_bot.14 clay_bot.15
#> 2            12          12          12          12          12          12
#> 5            16          16          16          16          16          16
#> 5.1          16          16          16          16          16          16
#> 5.2          16          16          16          16          16          16
#> NA           NA          NA          NA          NA          NA          NA
#>     texcl_bot.10 texcl_bot.11 texcl_bot.12 texcl_bot.13 texcl_bot.14
#> 2             SL           SL           SL           SL           SL
#> 5              L            L            L            L            L
#> 5.1            L            L            L            L            L
#> 5.2            L            L            L            L            L
#> NA          <NA>         <NA>         <NA>         <NA>         <NA>
#>     texcl_bot.15
#> 2             SL
#> 5              L
#> 5.1            L
#> 5.2            L
#> NA          <NA>

transform(cbind(h, lag = hz_lag(h)), 
  clay_dif = lag.clay_bot.1 - clay,
  texcl_contrast = paste0(texcl, "-", lag.texcl_bot.1)
)
#>    id top bottom texcl clay lag.clay_bot.1 lag.texcl_bot.1 clay_dif
#> 2   1   0     25    SL   10             12              SL        2
#> 3   1  25     44    SL   12             27              CL       15
#> 4   1  44     46    CL   27             35              CL        8
#> 5   1  46     50    CL   35             16               L      -19
#> NA  1  50    100     L   16             NA            <NA>       NA
#>    texcl_contrast
#> 2           SL-SL
#> 3           SL-CL
#> 4           CL-CL
#> 5            CL-L
#> NA           L-NA
```
