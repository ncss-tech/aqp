# Calculate the minimum thickness requirement for Mollic epipedon

Utilize horizon depths, designations and textures in a profile to
estimate the thickness requirement for the Mollic or Umbric epipedon,
per criterion 6 in the U.S. Keys to Soil Taxonomy (12th Edition).

## Usage

``` r
mollic.thickness.requirement(
  p,
  hzdesgn = hzdesgnname(p, required = TRUE),
  texcl.attr = hztexclname(p, required = TRUE),
  clay.attr = hzmetaname(p, "clay", required = TRUE),
  truncate = TRUE
)
```

## Arguments

- p:

  A single-profile SoilProfileCollection.

- hzdesgn:

  Column in horizon table containing designations. Default:
  `guessHzDesgnName(p)`

- texcl.attr:

  Column in horizon table containing texture classes. Default:
  `guessHzTexClName(p)`

- clay.attr:

  Column in horizon table containing clay contents. Default:
  `guessHzAttrName(p, 'clay', c('total','_r'))`

- truncate:

  Should sliding scale (Criterion 6C) results be truncated to 18 to 25cm
  interval? (Experimental; Default: TRUE)

## Value

A unit length numeric vector containing Mollic or Umbric epipedon
minimum thickness requirement.

## Author

Andrew G. Brown

## Examples

``` r
# construct a fake profile
spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxeralfs",
                  hzname   = c("A","AB","Bt","BCt","R"),
                  hzdept   = c(0,  20, 32, 42,  49),
                  hzdepb   = c(20, 32, 42, 49, 200),
                  prop     = c(18, 22, 28, 24,  NA),
                  texcl    = c("l","l","cl", "l","br"),
                  d_value  = c(5,   5,  5,  6,  NA),
                  m_value  = c(2.5, 3,  3,  4,  NA),
                  m_chroma = c(2,   3,  4,  4,  NA))

# promote to SoilProfileCollection
depths(spc) <- id ~ hzdept + hzdepb
hzdesgnname(spc) <- 'hzname'
hztexclname(spc) <- 'texcl'

# print results in table
data.frame(id = spc[[idname(spc)]],
           thickness_req = mollic.thickness.requirement(spc, clay.attr='prop'),
           thickness_req_nobound = mollic.thickness.requirement(spc,
                                        clay.attr='prop', truncate=FALSE))
#>   id thickness_req thickness_req_nobound
#> 1  1            18              16.33333
```
