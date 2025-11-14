# Check for "empty" profiles in a SoilProfileCollection

"Empty" profiles are used as placeholders for positions in a
`SoilProfileCollection` These profiles result from operations that
remove or extract portions of horizons from source profiles.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
isEmpty(object, ...)
```

## Arguments

- object:

  A SoilProfileCollection

- ...:

  Additional arguments not used.

## Value

logical. Vector of length equal to number of profiles in `object`.
Returns `TRUE` when a profile has one horizon with `NA` top and bottom
depths

## Details

In a `SoilProfileCollection` an empty profile occurs when it has one
horizon, with `NA` top and bottom depths. Generally all non-profile ID
site and horizon-level values are all also `NA`, but only the depths are
checked by `isEmpty()`.
