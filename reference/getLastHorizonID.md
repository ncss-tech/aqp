# Get IDs of Deepest Horizons by Profile

Return horizon IDs of the deepest horizon within each profile of a
`SoilProfileCollection`. IDs are returned in the same order as
`profile_id(x)`. Horizon top depths are used because there are cases
where bottom depths may be missing.

## Usage

``` r
getLastHorizonID(x)
```

## Arguments

- x:

  a `SoilProfileCollection`
