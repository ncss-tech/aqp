# Quickly assess relative state of site and horizon slots

Determine "state" of SoilProfileCollection before or after major
modifications of site or horizon slot contents.

Two logical checks are performed on the site and horizon tables, and a
third element `valid` returns `TRUE` when both checks are `TRUE`.

Check 1: Same number of sites in site as number of sites in horizons. No
intermingling of IDs, no orphan horizons, no sites without horizons (for
now)

Check 2: Site IDs match coalesced profile ID from horizons. Ensures the
same *relative* ordering, but horizons still may be out of order within
profiles

## Usage

``` r
spc_in_sync(object)
```

## Arguments

- object:

  A SoilProfileCollection

## Value

data.frame

## Author

Andrew G. Brown

## Examples

``` r
data(sp5)

spc_in_sync(sp5)
#>   nSites relativeOrder valid
#> 1   TRUE          TRUE  TRUE
```
