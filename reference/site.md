# Retrieve site data from SoilProfileCollection

Get site data from SoilProfileCollection. Result is returned in the same
`data.frame` class used to initially construct the
SoilProfileCollection.

There are two options available via the `site<-` setter.

The first is a "normalization" by formula interface, whereby one
specifies an attribute that is constant in horizons within profiles to
be promoted to a site-level variable: `site(spc) <- ~ horizonvariable`

The second is creation of site data from an external `data.frame` via
merge (LEFT JOIN). There must be one or more same-named columns (with at
least some matching data) on the left and right hand side to facilitate
the join: `site(spc) <- newdata`

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
site(object)

site(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  A formula or object inheriting `data.frame`

## Examples

``` r
# load test data
data(sp2)

# promote to SPC
depths(sp2) <- id ~ top + bottom

# normalize a horizon-level attribute to site
site(sp2) <- ~ surface

# inspect site table
site(sp2)
#>        id         surface
#> 1   hon-1        holocene
#> 2  hon-10   lower modesto
#> 3  hon-11   lower modesto
#> 4  hon-13   upper modesto
#> 5  hon-14   upper modesto
#> 6  hon-17 lower riverbank
#> 7  hon-18 lower riverbank
#> 8  hon-19 lower riverbank
#> 9   hon-2        holocene
#> 10 hon-20 lower riverbank
#> 11 hon-21    lower laguna
#> 12 hon-22    lower laguna
#> 13  hon-3        holocene
#> 14  hon-4        holocene
#> 15  hon-5   upper modesto
#> 16  hon-6   upper modesto
#> 17  hon-7   upper modesto
#> 18  hon-8   upper modesto

# make some data: classify two geomorphic surfaces with numeric value
newdata <- data.frame(surface = c("holocene",
                                  "lower riverbank"),
                      newvalue = c(1,2))

# do left join based on newly-normalized "surface" attribute
site(sp2) <- newdata

# inspect site table: holocene & lower riverbank have values
site(sp2)
#>            surface     id newvalue
#> 1         holocene  hon-1        1
#> 2    lower modesto hon-10       NA
#> 3    lower modesto hon-11       NA
#> 4    upper modesto hon-13       NA
#> 5    upper modesto hon-14       NA
#> 6  lower riverbank hon-17        2
#> 7  lower riverbank hon-18        2
#> 8  lower riverbank hon-19        2
#> 9         holocene  hon-2        1
#> 10 lower riverbank hon-20        2
#> 11    lower laguna hon-21       NA
#> 12    lower laguna hon-22       NA
#> 13        holocene  hon-3        1
#> 14        holocene  hon-4        1
#> 15   upper modesto  hon-5       NA
#> 16   upper modesto  hon-6       NA
#> 17   upper modesto  hon-7       NA
#> 18   upper modesto  hon-8       NA
```
