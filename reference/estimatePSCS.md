# Estimate boundaries of the U.S Soil Taxonomy Particle Size Control Section

Estimates the upper and lower boundary of the particle size control
section for Mineral or Organic soilsby applying a programmatic version
of the particle size control section key from the Keys to Soil Taxonomy
(13th edition). See details for assumptions and required data elements.

## Usage

``` r
estimatePSCS(
  p,
  hzdesgn = hzdesgnname(p, required = TRUE),
  clay.attr = hzmetaname(p, "clay", required = TRUE),
  texcl.attr = hztexclname(p, required = TRUE),
  lieutex = hzmetaname(p, "lieutex"),
  tax_order_field = "tax_order",
  bottom.pattern = "Cr|R|Cd|m",
  simplify = TRUE,
  ...
)
```

## Arguments

- p:

  A SoilProfileCollection

- hzdesgn:

  Name of the horizon attribute containing the horizon designation.
  Default `hzdesgnname(p, required = TRUE)`

- clay.attr:

  Name of the horizon attribute containing clay contents. Default
  `hzmetaname(p, "clay", required = TRUE)`

- texcl.attr:

  Name of the horizon attribute containing textural class (used for
  finding sandy textures). Default `hztexclname(p, required = TRUE)`

- lieutex:

  Optional data element used in addition to the horizon designation to
  identify kinds of organic soil material for soils with organic
  surfaces. Default: `hzmetaname(p, "lieutex")`

- tax_order_field:

  Name of the site attribute containing taxonomic order; for handling
  PSCS rules for Andisols in lieu of lab data. May be NA or column
  missing altogether, in which case Andisol PSC possibility is ignored.

- bottom.pattern:

  Regular expression pattern to match a root-restrictive contact.
  Default matches Cr, R, Cd or m. This argument is passed to both
  [`minDepthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md)
  and
  [`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md).

- simplify:

  Return a length 2 vector with upper and lower boundary when p has
  length 1? Default TRUE.

- ...:

  additional arguments are passed to getArgillicBounds()

## Value

A numeric vector (when `simplify=TRUE`) containing the top and bottom
depth of the particle size control section. First value is top, second
value is bottom. If `p` contains more than one profile, the result is a
data.frame with profile ID plus PSCS top and bottom depths.

## Details

Requires information to identify argillic horizons (clay contents,
horizon designations) with
[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md)
as well as the presence of plow layers and surface organic soil
material. Any
[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md)
arguments may be passed to `estimatePSCS`.

Also, requires information on taxonomic order to handle Andisols.

In aqp 2.1, particle size control sections of organic soils Histosols
and Histels are supported. This requires setting the `"lieutex"` horizon
metadata column using `hzmetaname<-()` Horizon designations containing
`"f"` or `"W"` are recognized as permafrost and water layers,
respectively, for application of the organic soils key to control
sections. In lieu textures `"SPM"` and `"PEAT"` are used to identify low
density organic materials used for surface tier thickness. To avoid
using the 160 cm surface tier, set the `"lieutex"` column to any column
that does not contain `"SPM"` or `"PEAT"` values.

WARNING: Soils in arenic or grossarenic subgroups, with fragipans, or
with strongly contrasting PSCs may not be classified correctly. The
author would welcome a dataset to develop this functionality for.

## References

Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural
Resources Conservation Service, Washington, DC.

## See also

[`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md),
[`getSurfaceHorizonDepth()`](https://ncss-tech.github.io/aqp/reference/getSurfaceHorizonDepth.md)

## Author

Andrew Gene Brown

## Examples

``` r
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# set required metadata
hzdesgnname(sp1) <- 'name'
hztexclname(sp1) <- 'texture'
hzmetaname(sp1, 'clay') <- 'prop'

x <- estimatePSCS(sp1)
x
#>     id pscs_top pscs_bottom
#> 1 P001       49          89
#> 2 P002       30          59
#> 3 P003        2          52
#> 4 P004       32          62
#> 5 P005        5          55
#> 6 P006       31         106
#> 7 P007       25         100
#> 8 P008       27         102
#> 9 P009       28         103

# change horizon texture and set inlieu texture column to turn
# first profile into an organic soil
sp1$name[1:6] <- c("Oi1", "Oi2", "Oi3", "Oaf", "Cf1", "Cf2")
sp1$texture <- as.character(sp1$texture)
sp1$texture[1:6] <- c("PEAT", "PEAT", "PEAT", "MUCK", "GRVLS", "GRVLS")
sp1$bottom[6] <- 200
hzmetaname(sp1, 'lieutex') <- 'texture'

y <- estimatePSCS(sp1[1, ], simplify = FALSE)

# 74cm lower boundary is 25cm past the upper boundary of permafrost (49cm)
# but minimum depth is 100cm unless there is a root-limiting layer
y
#>     id pscs_top pscs_bottom
#> 1 P001        0         100
```
