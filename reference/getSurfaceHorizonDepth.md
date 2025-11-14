# Determine thickness of horizons (continuous from surface) matching a pattern

Find the thickness of horizon designations, or any other character
patterns, that are continuous from the soil surface (depth = 0 or
shallowest depth in profile).

## Usage

``` r
getSurfaceHorizonDepth(
  p,
  pattern,
  hzdesgn = hzdesgnname(p, required = TRUE),
  simplify = TRUE
)

getMineralSoilSurfaceDepth(
  p,
  hzdesgn = hzdesgnname(p, required = TRUE),
  pattern = "O",
  simplify = TRUE
)

getPlowLayerDepth(
  p,
  hzdesgn = hzdesgnname(p, required = TRUE),
  pattern = "^Ap[^b]*",
  simplify = TRUE
)
```

## Arguments

- p:

  a SoilProfileCollection

- pattern:

  a regular expression pattern to match for all horizons to be
  considered part of the "surface".

- hzdesgn:

  column name containing horizon designation. Default:
  `hzdesgnname(p, required = TRUE)`.

- simplify:

  logical. Return single profile results as vector (default: `TRUE`) or
  `data.frame` (`FALSE`)

## Value

a numeric value corresponding to the bottom depth of the last horizon
matching 'pattern' that is contiguous with other matching horizons up to
the soil surface. If `length(p) > 1` then a *data.frame* containing
profile ID, horizon ID, top or bottom depths, horizon designation and
pattern.

## Details

The horizon designation to match is specified with the regular
expression pattern 'pattern'. All horizons matching that pattern, that
are continuous from the soil surface, count towards the depth /
thickness value that is ultimately returned. For instance: horizon
designations: A1-A2-A3-C-Ab , would return A3 bottom depth given
`pattern = "^A[1-9]*$"`.

`getSurfaceHorizonDepth` is used by `getPlowLayerDepth` for matching Ap
horizons; and, it is used by `getMineralSoilSurfaceDepth` to find the
thickness of O horizons in lieu of lab data.

## Author

Andrew G. Brown

## Examples

``` r
library(aqp)
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
q <- sp1[2]

# look at horizon designations in p and q
p$name
#> [1] "A1" "A2" "AB" "BA" "Bt" "Rt"
q$name
#> [1] "Oi" "A"  "C1" "C2"

# thickness of all surface horizons containing A
getSurfaceHorizonDepth(p, pattern = 'A', hzdesgn = 'name')
#> [1] 57

# thickness of all surface horizons that start with A
getSurfaceHorizonDepth(p, pattern = '^A', hzdesgn = 'name')
#> [1] 49

# thickness of all surface horizons that start with A, and the A is not followed by B
getSurfaceHorizonDepth(p, pattern = '^A[^B]*', hzdesgn = 'name')
#> [1] 49

# thickness of all surface horizons that start with A
#  followed by a number from _2_ to 9 (returns ZERO)
getSurfaceHorizonDepth(p, pattern = '^A[2-9]*', hzdesgn = 'name')
#> [1] 49

# getPlowLayerDepth matches first two horizons in fake Ap horizon data with "buried Ap"
p$aphorizons <- c("Ap1","Ap2","AB", rep('C', nrow(p) - 4), "Apb")
getPlowLayerDepth(p, hzdesgn = 'aphorizons')
#> [1] 14

# getMineralSoilSurfaceDepthmatches first 3 horizons in fake O horizon data
p$ohorizons <- c("Oi1","Oi2","Oe", rep('C', nrow(p) - 4), "2C")
getMineralSoilSurfaceDepth(p, hzdesgn='ohorizons')
#> [1] 49

# matches first Oi horizon with original horizon designations of pedon 2
getMineralSoilSurfaceDepth(q, hzdesgn='name')
#> [1] 5
```
