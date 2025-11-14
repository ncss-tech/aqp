# Split a SoilProfileCollection object into a list of SoilProfileCollection objects.

This function splits a `SoilProfileCollection` into a list of
`SoilProfileCollection` objects using a site-level attribute to define
groups or profile ID (`idname(x)`).

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
split(x, f, drop = TRUE, ...)
```

## Arguments

- x:

  `SoilProfileCollection` object

- f:

  character vector naming a single site-level attribute that defines
  groups, a ‘factor’ in the sense that `as.factor(f)` defines the
  grouping, or a list of such factors in which case their interaction is
  used for the grouping.

- drop:

  logical indicating if levels that do not occur should be dropped (if f
  is a factor or a list). When `drop=FALSE` and `f` contains missing
  values an additional group "missing" is returned.

- ...:

  additional arguments are ignored

## Value

A list of `SoilProfileCollection` or `NULL` for empty result.

## Details

As of aqp 1.25, omission of `f` argument is no longer possible, as the
base R generic is overloaded by this `SoilProfileCollection` method.
This used to result in an "identity" split, according to `idname(x)`,
e.g. a list as long as `length(x)`, with a single-profile
`SoilProfileCollection` per list element. Replicate this behavior using
`f = idname(x)` or `f = profile_id(x)`.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r
data(sp2)
depths(sp2) <- id ~ top + bottom

# add a more interesting site-level attribute
site(sp2) <- ~ surface

# using identity site-level attribute (profile ID)
p1 <- split(sp2, f = idname(sp2))
#> converting id to factor
names(p1)
#>  [1] "hon-1"  "hon-10" "hon-11" "hon-13" "hon-14" "hon-17" "hon-18" "hon-19"
#>  [9] "hon-2"  "hon-20" "hon-21" "hon-22" "hon-3"  "hon-4"  "hon-5"  "hon-6" 
#> [17] "hon-7"  "hon-8" 
length(p1)
#> [1] 18

# using vector equal in length to number of profiles (profile ID, again)
p2 <- split(sp2, f = profile_id(sp2))
names(p2)
#>  [1] "hon-1"  "hon-10" "hon-11" "hon-13" "hon-14" "hon-17" "hon-18" "hon-19"
#>  [9] "hon-2"  "hon-20" "hon-21" "hon-22" "hon-3"  "hon-4"  "hon-5"  "hon-6" 
#> [17] "hon-7"  "hon-8" 
length(p2)
#> [1] 18

# which are both equivalent to setting `f` to NULL
p3 <- split(sp2, f = NULL)
names(p3)
#>  [1] "hon-1"  "hon-10" "hon-11" "hon-13" "hon-14" "hon-17" "hon-18" "hon-19"
#>  [9] "hon-2"  "hon-20" "hon-21" "hon-22" "hon-3"  "hon-4"  "hon-5"  "hon-6" 
#> [17] "hon-7"  "hon-8" 
length(p3)
#> [1] 18

# split on surface (age) site-level var
p4 <- split(sp2, f = "surface")
names(p4)
#> [1] "holocene"        "lower modesto"   "upper modesto"   "lower riverbank"
#> [5] "upper riverbank" "lower laguna"   
length(p4) # 5 unique "surfaces", 5 SPCs in result list
#> [1] 6
```
