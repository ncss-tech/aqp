# Estimate upper and lower boundary of argillic diagnostic subsurface horizon

`getArgillicBounds` estimates the upper and lower boundary of argillic
diagnostic subsurface horizon for a profile in a single-profile
SoilProfileCollection object (`p`).

The upper boundary is where the clay increase threshold is met. The
function uses `crit.clay.argillic` as the threshold function for
determining whether a clay increase occurs and `get.increase.matrix` to
determine whether the increase is met, whether vertical distance of
increase is sufficiently small, and in which horizon.

## Usage

``` r
getArgillicBounds(
  p,
  hzdesgn = hzdesgnname(p, required = TRUE),
  clay.attr = hzmetaname(p, "clay", required = TRUE),
  texcl.attr = hztexclname(p, required = TRUE),
  require_t = TRUE,
  bottom.pattern = "Cr|R|Cd",
  lower.grad.pattern = "^[2-9]*B*CB*[^rtd]*[1-9]*$",
  sandy.texture.pattern = "-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$",
  vertical.distance = 30,
  simplify = TRUE,
  verbose = FALSE
)
```

## Arguments

- p:

  A SoilProfileCollection

- hzdesgn:

  the name of the column/attribute containing the horizon designation;
  default="hzname"

- clay.attr:

  the name of the column/attribute containing the clay content;
  default="clay"

- texcl.attr:

  the name of the column/attribute containing the textural class (used
  for finding sandy horizons); default="texcl"

- require_t:

  require a "t" subscript for positive identification of upper and lower
  bound of argillic? default: TRUE

- bottom.pattern:

  regular expression passed to `estimateSoilDepth` to match the lower
  boundary of the soil. default is "Cr\|R\|Cd" which approximately
  matches paralithic, lithic and densic contacts.

- lower.grad.pattern:

  this is a pattern for adjusting the bottom depth of the argillic
  horizon upwards from the bottom depth of the soil. The absence of
  illuviation is used as a final control on horizon pattern matching.

- sandy.texture.pattern:

  this is a pattern for matching sandy textural classes:
  `-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$`

- vertical.distance:

  Vertical distance in which clay increase must be met. Default `30` cm

- simplify:

  Return a length 2 vector with upper and lower boundary when `p` has
  length 1? Default `TRUE`.

- verbose:

  Print out information about 't' subscripts, sandy textures, plow
  layers and lower gradational horizons?

## Value

Returns a numeric vector; first value is top depth, second value is
bottom depth. If as.list is TRUE, returns a list with top depth named
"ubound" and bottom depth named "lbound". If `p` has more than one
profile or if `simplify = FALSE` the result is a data.frame containing
profile ID, upper and lower boundary columns.

## Details

The lower boundary is first approximated as the depth to a
lithic/paralithic/densic contact, or some other horizon matchable by a
custom regular expression pattern. Subsequently, that boundary is
extended upwards to the end of "evidence of illuviation."

The depth to contact is estimated using 'bottom.pattern' "Cr\|R\|Cd" by
default. It matches anything containing Cr, R or Cd.

The lower gradational horizon regular expression ‘lower.grad.pattern'
default is `^[2-9]*B*CB*[^rtd]*[1-9]*$}`. It matches anything that
starts with a lithologic discontinuity (or none) and a C master horizon
designation. May contain B as second horizon designation in transitional
horizon. May not contain 'r' or 't' subscript.

The minimum thickness of the argillic horizon is dependent on whether
all subhorizons are "sandy" or not. The `sandy.texture.pattern` default
`-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$` captures USDA textural class
fine earth fractions that meet "sandy" particle size class criteria.

There also is an option ‘require_t' to omit the requirement for evidence
of eluviation in form of 't' subscript in 'hzdesgn'. Even if "t"
subscript is not required for positive identification, the presence of
lower gradational C horizons lacking 't' will still be used to modify
the lower boundary upward from a detected contact, if needed. If this
behavior is not desired, just set 'lower.grad.pattern' to something that
will not match any horizons in your data.

## Author

Andrew G. Brown

## Examples

``` r
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# set required metadata
hzdesgnname(sp1) <- 'name'
hztexclname(sp1) <- 'texture'
hzmetaname(sp1, 'clay') <- 'prop'

x <- getArgillicBounds(sp1)
x
#>     id ubound lbound
#> 1 P001     49     89
#> 2 P002     NA     NA
#> 3 P003      2     67
#> 4 P004     32     62
#> 5 P005      5     68
#> 6 P006     NA     NA
#> 7 P007     NA     NA
#> 8 P008     NA     NA
#> 9 P009     NA     NA
```
