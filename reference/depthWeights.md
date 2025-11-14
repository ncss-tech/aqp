# Return a vector of contributing fractions over a depth interval

`depthWeights()` calculates the contributing fraction for each pair of
horizon top and bottom depths, given an upper and lower boundary.

## Usage

``` r
depthWeights(top, bottom, upper, lower)
```

## Arguments

- top:

  A numeric vector of horizon top depths.

- bottom:

  A numeric vector of horizon bottom depths.

- upper:

  A unit length numeric vector with upper boundary.

- lower:

  A unit length numeric vector with lower boundary.

## Value

A named list.

## Author

Andrew G. Brown.
