# Dissolving horizon boundaries by grouping variables

This function dissolves or combines horizons that have a common set of
grouping variables. It only combines those horizon records that are
sequential (e.g. share a horizon boundary). Thus, it can be used to
identify discontinuities in the grouping variables along a profile and
their unique depths. It is particularly useful for determining the depth
to the top or bottom of horizons with a specific category, and should be
simpler than previous methods that require aggregating over profiles.

## Usage

``` r
hz_dissolve(
  object,
  by,
  idcol = "id",
  depthcols = c("top", "bottom"),
  collapse = FALSE,
  order = FALSE
)

dissolve_hz(
  object,
  by,
  id = "idcol",
  hztop = "top",
  hzbot = "bottom",
  collapse = FALSE,
  order = FALSE
)
```

## Arguments

- object:

  a `data.frame`

- by:

  character: column names, to be used as grouping variables, within the
  object.

- idcol:

  character: column name of the pedon ID within the object.

- depthcols:

  a character vector of length 2 specifying the names of the horizon
  depths (e.g. `c("top", "bottom")`).

- collapse:

  logical: indicating whether to not combine grouping variables before
  dissolving.

- order:

  logical: indicating whether or not to order the object by the id,
  hztop, and hzbot columns.

- id:

  deprecated and replaced with idcol.

- hztop:

  deprecated and replaced by depthcols.

- hzbot:

  deprecated and replaced by depthcols.

## Value

A `data.frame` with the original idcol, by grouping variables, and
non-consecutive horizon depths.

## Details

This function assumes the profiles and horizons within the object follow
the logic defined by `checkHzDepthLogic` (e.g. records are ordered
sequentially by id, hztop, and hzbot and without gaps). If the records
are not ordered, set the `order = TRUE`.

## See also

[`hz_lag()`](https://ncss-tech.github.io/aqp/reference/hz_lag.md),
[`hz_intersect()`](https://ncss-tech.github.io/aqp/reference/hz_intersect.md),
[`hz_segment()`](https://ncss-tech.github.io/aqp/reference/hz_segment.md)
,
[`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md)

## Author

Stephen Roecker

## Examples

``` r
# example 1
data(jacobs2000)
spc <- jacobs2000

spc$dep_5 <- spc$depletion_pct >=5
spc$genhz <- generalize.hz(spc$name, c("A", "E", "B", "C"), c("A", "E", "B", "C")) 
h <- horizons(spc)

test <- hz_dissolve(h, by = c("genhz", "dep_5"), idcol = "id", depthcols = c("top", "bottom"))
#> non-character grouping variables are being converted to characters

vars <- c("id", "top", "bottom", "genhz", "dep_5")
h[h$id == "92-1", vars]
#>     id top bottom genhz dep_5
#> 1 92-1   0     18     A FALSE
#> 2 92-1  18     43     E FALSE
#> 3 92-1  43     79     B FALSE
#> 4 92-1  79    130     B FALSE
#> 5 92-1 130    153     B FALSE
#> 6 92-1 153    156     C FALSE
#> 7 92-1 156    213     C  TRUE
test[test$id == "92-1", ]
#>      id top bottom variable value        dissolve_id
#> 1  92-1   0     18    genhz     A     92-1_000-018_A
#> 2  92-1  18     43    genhz     E     92-1_018-043_E
#> 3  92-1  43    153    genhz     B     92-1_043-153_B
#> 4  92-1 153    213    genhz     C     92-1_153-213_C
#> 27 92-1   0    156    dep_5 FALSE 92-1_000-156_FALSE
#> 28 92-1 156    213    dep_5  TRUE  92-1_156-213_TRUE


# example 2
df <- data.frame(
    id = 1,
    top    = c(0, 5,  10, 15, 25, 50), 
    bottom = c(5, 10, 15, 25, 50, 100),
    hzname = c("A1",  "A2",  "E/A", "2Bt1", "2Bt2", "2C"),
    genhz  = c("A",   "A",   "E",   "2Bt",  "2Bt", "2C"),
    texcl  = c("sil", "sil", "sil", "sl",   "sl",   "s")
    )

df
#>   id top bottom hzname genhz texcl
#> 1  1   0      5     A1     A   sil
#> 2  1   5     10     A2     A   sil
#> 3  1  10     15    E/A     E   sil
#> 4  1  15     25   2Bt1   2Bt    sl
#> 5  1  25     50   2Bt2   2Bt    sl
#> 6  1  50    100     2C    2C     s

hz_dissolve(df, c("genhz", "texcl"))
#>   id top bottom variable value   dissolve_id
#> 1  1   0     10    genhz     A   1_000-010_A
#> 2  1  10     15    genhz     E   1_010-015_E
#> 3  1  15     50    genhz   2Bt 1_015-050_2Bt
#> 4  1  50    100    genhz    2C  1_050-100_2C
#> 5  1   0     15    texcl   sil 1_000-015_sil
#> 6  1  15     50    texcl    sl  1_015-050_sl
#> 7  1  50    100    texcl     s   1_050-100_s
hz_dissolve(df, c("genhz", "texcl"), collapse = TRUE)
#>   id top bottom      variable    value        dissolve_id
#> 1  1   0     10 genhz & texcl  A & sil  1_000-010_A & sil
#> 2  1  10     15 genhz & texcl  E & sil  1_010-015_E & sil
#> 3  1  15     50 genhz & texcl 2Bt & sl 1_015-050_2Bt & sl
#> 4  1  50    100 genhz & texcl   2C & s   1_050-100_2C & s

test <- hz_dissolve(df, "genhz")
subset(test, value == "2Bt")
#>   id top bottom variable value   dissolve_id
#> 3  1  15     50    genhz   2Bt 1_015-050_2Bt
```
