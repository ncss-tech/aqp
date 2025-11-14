# Get or Set Restriction data in a SoilProfileCollection

Restrictions describe root-limiting features in the soil. A single
profile may have multiple restrictions.

- `restrictions()` (get method): Get restriction data from a
  SoilProfileCollection.

&nbsp;

- `restrictions<-` (set method): Set restriction data for a
  SoilProfileCollection. The profile ID column from `object`
  (`idname(object)`) must be present in the replacement `value` object.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
restrictions(object)

# S4 method for class 'SoilProfileCollection'
restrictions(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  An data.frame object containing at least a column with name
  `idname(object)`

## Examples

``` r
# load test data
data(sp2)

# promote to SPC
depths(sp2) <- id ~ top + bottom

# assign abrupt textural change to a profile
newdata <- data.frame(id = c("hon-21"),
                      restrkind = "abrupt textural change",
                      restrdep = 46)

# do left join
restrictions(sp2) <- newdata

# inspect site table: newvalue TRUE only for horizons
#  with top depth equal to zero
restrictions(sp2)
#>       id              restrkind restrdep
#> 1 hon-21 abrupt textural change       46
```
