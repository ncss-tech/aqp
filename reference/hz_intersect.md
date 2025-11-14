# Intersecting horizon boundaries by horizon depths

This function intersects two horizon tables by harmonizing their depths
and merging them where they overlap. This can be useful to rejoin the
results of
[`hz_dissolve()`](https://ncss-tech.github.io/aqp/reference/hz_dissolve.md)
to it's original horizon table, and then perform an aggregation on the
dissolved variables.

## Usage

``` r
hz_intersect(x, y, idcol = "id", depthcols = c("top", "bottom"))
```

## Arguments

- x:

  a `data.frame`

- y:

  a `data.frame`

- idcol:

  character: column name of the pedon ID within the object.

- depthcols:

  a character vector of length 2 specifying the names of the horizon
  depths (e.g. `c("top", "bottom")`).

## Value

A `data.frame` with harmonized depth intervals (i.e. segment_id) and
columns from both of the original `data.frame`. If both `data.frame`
contain the same column names, they will both be returned (with the
exception of the idcol and depthcols), and appended with either x or y
to indicate which `data.frame` they originated from.

## Details

.

## See also

[`hz_dissolve()`](https://ncss-tech.github.io/aqp/reference/hz_dissolve.md),
[`hz_lag()`](https://ncss-tech.github.io/aqp/reference/hz_lag.md),
[`hz_segment()`](https://ncss-tech.github.io/aqp/reference/hz_segment.md)

## Author

Stephen Roecker

## Examples

``` r
h <- data.frame(
  id = 1,
  top    = c(0,  25, 44, 46, 50),
  bottom = c(25, 44, 46, 50, 100),
  by     = c("Yes", "Yes", "No", "No", "Yes"),
  clay   = c(10, 12, 27, 35, 16)
)

hz_dissolve(h, "by")
#>   id top bottom variable value   dissolve_id
#> 1  1   0     44       by   Yes 1_000-044_Yes
#> 2  1  44     50       by    No  1_044-050_No
#> 3  1  50    100       by   Yes 1_050-100_Yes

hz_intersect(x = hz_dissolve(h, "by"), y = h)
#>   segment_id id top bottom variable value   dissolve_id  by clay
#> 1    000-025  1   0     25       by   Yes 1_000-044_Yes Yes   10
#> 2    025-044  1  25     44       by   Yes 1_000-044_Yes Yes   12
#> 3    044-046  1  44     46       by    No  1_044-050_No  No   27
#> 4    046-050  1  46     50       by    No  1_044-050_No  No   35
#> 5    050-100  1  50    100       by   Yes 1_050-100_Yes Yes   16

hi <- hz_intersect(x = h, y = hz_dissolve(h, "by"))
aggregate(clay ~ dissolve_id, data = hi, mean)
#>     dissolve_id clay
#> 1 1_000-044_Yes   11
#> 2  1_044-050_No   31
#> 3 1_050-100_Yes   16
```
