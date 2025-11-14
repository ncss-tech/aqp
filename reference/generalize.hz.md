# Generalize Horizon Names

Generalize a vector of horizon names, based on new classes, and REGEX
patterns. Or create a new column `ghl` in a `SoilProfileCollection`
(requires a horizon designation name to be defined for the collection,
see details)

## Usage

``` r
generalize.hz(
  x,
  new,
  pattern,
  non.matching.code = "not-used",
  hzdepm = NULL,
  ordered = !missing(hzdepm),
  na.rm = TRUE,
  ...
)

# S4 method for class 'character'
generalizeHz(
  x,
  new,
  pattern,
  non.matching.code = "not-used",
  hzdepm = NULL,
  ordered = !missing(hzdepm),
  ...
)

# S4 method for class 'SoilProfileCollection'
generalizeHz(
  x,
  new,
  pattern,
  non.matching.code = "not-used",
  hzdepm = NULL,
  ordered = !missing(hzdepm),
  ghl = "genhz",
  ...
)
```

## Arguments

- x:

  character vector of horizon names or a `SoilProfileCollection` object

- new:

  character vector of generalized horizon labels (GHL)

- pattern:

  character vector of REGEX patterns, same length as `new`

- non.matching.code:

  character, label used for any horizon not matched by `pattern`

- hzdepm:

  numeric vector of horizon mid-points; `NA` values in `hzdepm` will
  result in `non.matching.code` (or `NA` if not defined) in result

- ordered:

  logical, default `TRUE` when `hzdepm` argument is specified

- na.rm:

  logical, default `TRUE` will ignore missing depths in calculating sort
  order when `hzdepm` is specified and `ordered=TRUE`

- ...:

  additional arguments passed to
  [`grep()`](https://rdrr.io/r/base/grep.html) such as `perl = TRUE` for
  advanced REGEX

- ghl:

  Generalized Horizon Designation column name (to be created/updated
  when `x` is a `SoilProfileCollection`)

## Value

factor (an ordered factor when `ordered=TRUE`) of the same length as `x`
(if character) or as number of horizons in `x` (if
`SoilProfileCollection`)

## Details

When `x` is a `SoilProfileCollection` the `ghl` column will be updated
with the factor results. This requires that the "horizon designation
name" metadata be defined for the collection to set the column for input
designations.

## References

Beaudette, D.E., Roudier, P., Skovlin, J. (2016). Probabilistic
Representation of Genetic Soil Horizons. In: Hartemink, A., Minasny, B.
(eds) Digital Soil Morphometrics. Progress in Soil Science. Springer,
Cham. https://doi.org/10.1007/978-3-319-28295-4_18

## See also

[`hzdesgnname()`](https://ncss-tech.github.io/aqp/reference/hzdesgnname.md)

## Author

D.E. Beaudette

## Examples

``` r
data(sp1)

# check original distribution of hz designations
table(sp1$name)
#> 
#>   2C  2C1  2C2  3Ab 3Bwb   3C  3Cb    A   A1   A2   A3   AB  AB1  AB2  AB3   BA 
#>    2    2    2    1    2    2    1    4    4    4    1    5    1    1    1    2 
#>   Bt  Bt1  Bt2  Bw1  Bw2  Bw3    C   C1   C2 Oa/A   Oe   Oi   Rt 
#>    1    3    3    3    3    1    1    2    2    1    1    3    1 

# generalized horizon labels
# character vector input
sp1$genhz <- generalizeHz(
  sp1$name,
  new = c('O','A','B','C','R'),
  pattern = c('O', '^A','^B','C','R'),
  ordered = TRUE
)

# see how we did / what we missed
table(sp1$genhz, sp1$name)
#>           
#>            2C 2C1 2C2 3Ab 3Bwb 3C 3Cb A A1 A2 A3 AB AB1 AB2 AB3 BA Bt Bt1 Bt2
#>   O         0   0   0   0    0  0   0 0  0  0  0  0   0   0   0  0  0   0   0
#>   A         0   0   0   0    0  0   0 4  4  4  1  5   1   1   1  0  0   0   0
#>   B         0   0   0   0    0  0   0 0  0  0  0  0   0   0   0  2  1   3   3
#>   C         2   2   2   0    0  2   1 0  0  0  0  0   0   0   0  0  0   0   0
#>   R         0   0   0   0    0  0   0 0  0  0  0  0   0   0   0  0  0   0   0
#>   not-used  0   0   0   1    2  0   0 0  0  0  0  0   0   0   0  0  0   0   0
#>           
#>            Bw1 Bw2 Bw3 C C1 C2 Oa/A Oe Oi Rt
#>   O          0   0   0 0  0  0    1  1  3  0
#>   A          0   0   0 0  0  0    0  0  0  0
#>   B          3   3   1 0  0  0    0  0  0  0
#>   C          0   0   0 1  2  2    0  0  0  0
#>   R          0   0   0 0  0  0    0  0  0  1
#>   not-used   0   0   0 0  0  0    0  0  0  0


## a more advanced example, requries `perl = TRUE`
# example data
x <- c('A', 'AC', 'Bt1', '^AC', 'C', 'BC', 'CB')

# new labels
n <- c('A', '^AC', 'C')

# patterns:
# "A anywhere in the name"
# "literal '^A' anywhere in the name"
# "C anywhere in name, but without preceding A"
p <- c('A', '^A', '(?<!A)C')

# note additional argument
res <- generalizeHz(
  x, 
  new = n, 
  pattern = p, 
  perl = TRUE
)

# double-check: OK
table(res, x)
#>           x
#> res        A AC BC Bt1 C CB ^AC
#>   A        0  0  0   0 0  0   1
#>   ^AC      1  1  0   0 0  0   0
#>   C        0  0  1   0 1  1   0
#>   not-used 0  0  0   1 0  0   0

## apply to a SoilProfileCollection
data(sp1)
depths(sp1) <- id ~ top + bottom

# must set horizon designation metadata
hzdesgnname(sp1) <- 'name'

# result is a SoilProfileCollection
x <- generalizeHz(
  sp1,
  new = c('O','A','B','C','R'),
  pattern = c('O', '^A','^B','C','R'),
  ordered = TRUE
)

# GHL stored in 'genhz' column
x$genhz
#>  [1] A        A        A        B        B        R        O        A       
#>  [9] C        C        O        A        B        B        B        A       
#> [17] A        A        B        B        A        A        B        B       
#> [25] O        A        A        A        A        C        C        C       
#> [33] C        C        A        A        B        B        B        C       
#> [41] C        not-used C        O        A        A        B        B       
#> [49] C        C        C        O        A        A        A        B       
#> [57] B        C        not-used not-used
#> Levels: O < A < B < C < R < not-used

# GHL metadata is set
GHL(x)
#> [1] "genhz"
```
