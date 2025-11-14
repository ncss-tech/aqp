# Get or Set Diagnostic Horizon data in a SoilProfileCollection

Diagnostic horizons describe features of the soil relevant to taxonomic
classification. A single profile may have multiple diagnostic features
or horizons, each of which may be comprised of multiple horizons.

- `diagnostic_hz()` (get method): Get diagnostic feature data from a
  SoilProfileCollection.

&nbsp;

- `diagnostic_hz<-` (set method): Set diagnostic feature data for a
  SoilProfileCollection. The profile ID column from `object`
  (`idname(object)`) must be present in the replacement `value` object.

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
diagnostic_hz(object)

# S4 method for class 'SoilProfileCollection'
diagnostic_hz(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  An object inheriting from `data.frame`

## Examples

``` r
# load test data
data(sp2)

# promote to SPC
depths(sp2) <- id ~ top + bottom

# assign two profiles a zone related to the mollic epipedon
newdata <- data.frame(id = c("hon-1","hon-17"),
                      featkind = "fixed-depth surface sample",
                      featdept = 0,
                      featdepb = 18)

# do left join
diagnostic_hz(sp2) <- newdata

# inspect site table: newvalue TRUE only for horizons
#  with top depth equal to zero
diagnostic_hz(sp2)
#>       id                   featkind featdept featdepb
#> 1  hon-1 fixed-depth surface sample        0       18
#> 2 hon-17 fixed-depth surface sample        0       18
```
