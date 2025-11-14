# Find Overlap within a Sequence

Establish which elements within a vector of horizontal positions overlap
beyond a given threshold

Desc.

## Usage

``` r
findOverlap(x, thresh)

overlapMetrics(x, thresh)
```

## Arguments

- x:

  vector of relative horizontal positions, one for each profile

- thresh:

  threshold defining "overlap", typically \< 1

  @return a `list`:

  - `idx`: unique index to overlapping elements in `x`

  - `ov`: normalized overlap (see details)

## Value

unique index to affected (overlapping) elements in `x`

## Examples

``` r
x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)

findOverlap(x, thresh = 0.5)
#> Warning: This function is deprecated, please use the `overLapMetrics()` function instead.
#> [1] 3 4 5


x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)

overlapMetrics(x, thresh = 0.5)
#> $idx
#> [1] 3 4 5
#> 
#> $ov
#> [1] 0.125
#> 

```
