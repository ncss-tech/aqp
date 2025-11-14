# Generate Soil Depth Class Matrix

Generate a boolean matrix of soil depth classes, actual soil depth
class, and estimate of soil depth from a `SoilProfileCollection` object.
Soil depths are estimated using pattern matching applied to horizon
designations, by
[`estimateSoilDepth()`](https://ncss-tech.github.io/aqp/reference/estimateSoilDepth.md).
The default REGEX pattern (`p = 'Cr|R|Cd'`) will match most "contacts"
described using the USDA / Soil Taxonomy horizon designation
conventions.

## Usage

``` r
getSoilDepthClass(
  f,
  depth.classes = c(very.shallow = 25, shallow = 50, mod.deep = 100, deep = 150,
    very.deep = 10000),
  ...
)
```

## Arguments

- f:

  a SoilProfileCollection object

- depth.classes:

  a named vector of classes and depth breaks

- ...:

  arguments passed to
  [`estimateSoilDepth`](https://ncss-tech.github.io/aqp/reference/estimateSoilDepth.md)

## Value

a `data.frame` containing soil depth and depth class for each profile,
see examples

## See also

[`estimateSoilDepth`](https://ncss-tech.github.io/aqp/reference/estimateSoilDepth.md)

## Author

D.E. Beaudette and J.M. Skovlin

## Examples

``` r
data(sp1)
depths(sp1) <- id ~ top + bottom

# generate depth-class matrix
sdc <- getSoilDepthClass(sp1, name = 'name')

# inspect
head(sdc)
#>     id depth very.shallow shallow mod.deep  deep very.deep depth.class
#> 1 P001    89        FALSE   FALSE     TRUE FALSE     FALSE    mod.deep
#> 2 P002    59        FALSE   FALSE     TRUE FALSE     FALSE    mod.deep
#> 3 P003    67        FALSE   FALSE     TRUE FALSE     FALSE    mod.deep
#> 4 P004    62        FALSE   FALSE     TRUE FALSE     FALSE    mod.deep
#> 5 P005    68        FALSE   FALSE     TRUE FALSE     FALSE    mod.deep
#> 6 P006   200        FALSE   FALSE    FALSE FALSE      TRUE   very.deep

# join back into sp1 as site-level data
site(sp1) <- sdc

if (FALSE) { # \dontrun{
# sample data
data(gopheridge, package='soilDB')

getSoilDepthClass(gopheridge, name = 'hzname')
} # }
```
