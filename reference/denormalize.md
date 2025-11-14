# Create a (redundant) horizon-level attribute from a site-level attribute

Create a (redundant) horizon-level attribute from a site-level
attribute. Specify a SoilProfileCollection and a site-level attribute
from that SPC (by name) to receive a vector of length equal to the
number of horizons containing the site-level values. This vector is
directly usable with the SoilProfileCollection horizon setter.

`denormalize` is the inverse operation for the formula interface that
"normalizes" a horizon level variable to site level:

`site(object) <- ~ horizonvar`

## Usage

``` r
denormalize(object, attr)
```

## Arguments

- object:

  A SoilProfileCollection

- attr:

  Site-level attribute name (character string) to denormalize to
  horizon.

## Value

A vector of values of equal length to the number of rows in the horizon
table of the input SPC.

## Details

"Denormalization" is the process of trying to improve the read
performance of a database, at the expense of losing some write
performance, by adding redundant copies of data or by grouping data.
Sometimes it is beneficial to have site-level attributes denormalized
for grouping of horizon-level data in analyses. `denormalize` achieves
this result for SoilProfileCollections.

## Author

Andrew G. Brown, Dylan Beaudette

## Examples

``` r
data(sp1)

# create a SoilProfileCollection from horizon data
depths(sp1) <- id ~ top + bottom

# create random site-level attribute `sitevar` with a binary (0/1) outcome
sp1$sitevar <- round(runif(length(sp1)))

# use denormalize() to create a mirror of sitevar in the horizon table
# name the attribute something different (e.g. `hz.sitevar`) to 
# prevent collision with the site attribute
# the attributes can have the same name but you will then need 
# site() or horizons() to access explicitly
sp1$hz.sitevar <- denormalize(sp1, 'sitevar')

# compare number of profiles to number of sitevar assignments
length(sp1)
#> [1] 9
table(sp1$sitevar)
#> 
#> 0 1 
#> 6 3 

# compare number of horizons to number of horizon-level copies of sitevar `hz.'sitevar`
nrow(sp1)
#> [1] 60
table(sp1$hz.sitevar)
#> 
#>  0  1 
#> 39 21 
```
