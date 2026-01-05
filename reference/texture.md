# Textural conversions

These functions consist of several conversions between sand, silt and
clay to texture class and visa versa, textural modifiers to rock
fragments, and grain size composition to the family particle size class.

## Usage

``` r
texcl_to_ssc(texcl = NULL, clay = NULL, sample = FALSE)

ssc_to_texcl(
  sand = NULL,
  clay = NULL,
  simplify = FALSE,
  as.is = FALSE,
  droplevels = TRUE
)

texmod_to_fragvoltot(texmod = NULL, lieutex = NULL)

texture_to_taxpartsize(
  texcl = NULL,
  clay = NULL,
  sand = NULL,
  sandvf = NULL,
  fragvoltot = NULL
)

texture_to_texmod(texture, duplicates = "combine")

fragvol_to_texmod(
  object = NULL,
  gravel = NULL,
  cobbles = NULL,
  stones = NULL,
  boulders = NULL,
  channers = NULL,
  flagstones = NULL,
  paragravel = NULL,
  paracobbles = NULL,
  parastones = NULL,
  paraboulders = NULL,
  parachanners = NULL,
  paraflagstones = NULL,
  as.is = TRUE,
  droplevels = TRUE
)
```

## Arguments

- texcl:

  vector of texture classes than conform to the USDA code conventions
  (e.g. c\|C, sil\|SIL, sl\|SL, cos\|COS)

- clay:

  vector of clay percentages

- sample:

  logical: should ssc be random sampled from the lookup table? (default:
  FALSE)

- sand:

  vector of sand percentages

- simplify:

  Passed to
  [`SoilTextureLevels()`](https://ncss-tech.github.io/aqp/reference/SoilTextureLevels.md)
  to set the number of possible texture classes. If `TRUE`, the ordered
  factor has a maximum of 12 levels, if `FALSE` (default) the ordered
  factor has a maximum of 21 levels (including e.g. very
  fine/fine/coarse variants)

- as.is:

  logical: should character vectors be converted to factors? (default:
  TRUE)

- droplevels:

  logical: indicating whether to drop unused levels in factors. This is
  useful when the results have a large number of unused classes, which
  can waste space in tables and figures.

- texmod:

  vector of textural modifiers that conform to the USDA code conventions
  (e.g. gr\|GR, grv\|GRV)

- lieutex:

  vector of in lieu of texture terms that conform to the USDA code
  conventions (e.g. gr\|GR, pg\|PG), only used when fragments or
  artifacts are \> 90 percent by volume (default: NULL))

- sandvf:

  vector of very fine sand percentages

- fragvoltot:

  vector of total rock fragment percentages

- texture:

  vector of combinations of texcl, texmod, and lieutex (e.g. CL, GR-CL,
  CBV-S, GR)

- duplicates:

  character: specifying how multiple values should be handled, options
  are `"combined"` (e.g. 'GR & GRV) or `"max"`(e.g. 'GRV')

- object:

  data.frame: containing the following column names: gravel, cobbles,
  stones, boulders, channers, flagstones, paragravel, paracobbles,
  parastones, paraboulders, parachanners, paraflagstones

- gravel:

  numeric: gravel volume %

- cobbles:

  numeric: cobble volume %

- stones:

  numeric: stone volume %

- boulders:

  numeric: boulder volume %

- channers:

  numeric: channer volume %

- flagstones:

  numeric: flagstone volume %

- paragravel:

  numeric: para gravel volume %

- paracobbles:

  numeric: para cobble volume %

- parastones:

  numeric: para stone volume %

- paraboulders:

  numeric: para boulder volume %

- parachanners:

  numeric: para channer volume %

- paraflagstones:

  numeric: para flagstone volume %

## Value

- `texcl_to_ssc`: A `data.frame` containing columns `"sand"`,`"silt"`,
  `"clay"`

&nbsp;

- `ssc_to_texcl`: A `character` vector containing texture class

&nbsp;

- `texmod_to_fragvoltot`: A `data.frame` containing columns
  `"fragvoltot_l"`, `"fragvoltot_r"`, `"fragvoltot_h"`,
  `"fragvoltot_l_nopf"`, `"fragvoltot_r_nopf"`, `"fragvoltot_h_nopf"`

&nbsp;

- `texture_to_taxpartsize`: a character vector containing
  `"taxpartsize"` classes

&nbsp;

- `texture_to_texmod`: a character vector containing `"texmod"` classes

&nbsp;

- `texmod_to_fragvol`: a data.frame containing `"texmod"` and
  `"lieutex"` classes

## Details

These functions are intended to estimate missing values or allocate
particle size fractions to classes. The `ssc_to_texcl()` function uses
the same logic as the particle size estimator calculation in NASIS to
classify sand and clay into texture class. The results are stored in
`soiltexture` and used by `texcl_to_ssc()` as a lookup table to convert
texture class to sand, silt and clay. The function `texcl_to_ssc()`
replicates the functionality described by Levi (2017). The
`texmod_to_fragvol()` function similarly uses the logical from the
Exhibit618-11_texture_modifier.xls spreadsheet to determine the textural
modifier from the various combinations of rock and pararock fragments
(e.g. GR and PGR).

When `sample = TRUE`, the results can be used to estimate within-class,
marginal distributions of sand, silt, and clay fractions. It is
recommended that at least 10 samples be drawn for reasonable estimates.

The function `texmod_to_fragvoltot` returns a data.frame with multiple
fragvoltot columns differentiated by tailing abbreviations (e.g. \_r)
which refer to the following:

1.  l = low

2.  r = representative

3.  h = high

4.  nopf = no pararock fragments (i.e. total fragments - pararock
    fragments)

The function `texture_to_texmod()` parses texture (e.g. GR-CL) to
extract the texmod values from it in the scenario where it is missing
from texmod column. If multiple texmod values are present (for example
in the case of stratified textures) and `duplicates = "combine"` they
will be combined in the output (e.g. GR & CBV). Otherwise if
`duplicates = "max"` the texmod with the highest rock fragment (e.g.
CBV) will be returned.

Unlike the other functions, `texture_to_taxpartsize()` is intended to be
computed on weighted averages within the family particle size control
section. Also recall from the criteria that carbonate clay should be
subtracted from clay content and added to silt content. Similarly, if
the percent of very fine sand is known it should be subtracted from the
sand, and added to the silt content. Unlike the other functions,
`texture_to_taxpartsize()` is intended to be computed on weighted
averages within the family particle size control section. Also recall
from the criteria that carbonate clay should be subtracted from clay
content and added to silt content. Similarly, if the percent of very
fine sand is known it should be subtracted from the sand, and added to
the silt content.

## References

Matthew R. Levi, Modified Centroid for Estimating Sand, Silt, and Clay
from Soil Texture Class, Soil Science Society of America Journal, 2017,
81(3):578-588, ISSN 1435-0661,
[doi:10.2136/sssaj2016.09.0301](https://doi.org/10.2136/sssaj2016.09.0301)
.

## See also

[`SoilTextureLevels`](https://ncss-tech.github.io/aqp/reference/SoilTextureLevels.md)

[`hz_to_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/hz_to_taxpartsize.md),
[`lookup_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/lookup_taxpartsize.md)

## Author

Stephen Roecker

## Examples

``` r
# \donttest{
# example of ssc_to_texcl()
tex <- expand.grid(sand = 0:100, clay = 0:100)
tex <- subset(tex, (sand + clay) < 101)
tex$texcl <- ssc_to_texcl(sand = tex$sand, clay = tex$clay)
head(tex)
#>   sand clay texcl
#> 1    0    0    si
#> 2    1    0    si
#> 3    2    0    si
#> 4    3    0    si
#> 5    4    0    si
#> 6    5    0    si

# example of texcl_to_ssc(texcl)
texcl <- c("cos", "s", "fs", "vfs", "lcos", "ls",
          "lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l",
          "sil", "si", "scl", "cl", "sicl", "sc", "sic", "c"
          )
test <- texcl_to_ssc(texcl)
head(test <- cbind(texcl, test), 10)
#>    texcl sand silt clay
#> 1    cos   92    5    3
#> 2      s   92    5    3
#> 3     fs   92    5    3
#> 4    vfs   92    5    3
#> 5   lcos   82   12    6
#> 6     ls   82   12    6
#> 7    lfs   82   12    6
#> 8   lvfs   82   12    6
#> 9   cosl   65   25   10
#> 10    sl   65   25   10


# example of texcl_to_ssc(texcl, clay)
data(soiltexture)
st <- soiltexture$values
idx <- sample(1:length(st$texcl), 10)
st <- st[idx, ]
ssc <- texcl_to_ssc(texcl = st$texcl)
head(cbind(texcl = st$texcl, clay = ssc$clay))
#>      texcl  clay
#> [1,] "c"    "55"
#> [2,] "sicl" "33"
#> [3,] "sl"   "10"
#> [4,] "sc"   "41"
#> [5,] "cl"   "33"
#> [6,] "sicl" "33"


# example of texmod_to_fragvoltol
frags <- c("gr", "grv", "grx", "pgr", "pgrv", "pgrx")
head(texmod_to_fragvoltot(frags))
#>   fragvoltot_l fragvoltot_r fragvoltot_h fragvoltot_h_nopf fragvoltot_r_nopf
#> 1           15           25           34                34                25
#> 2           35           48           59                59                48
#> 3           60           75           89                89                75
#> 4           15           25           34                 0                 0
#> 5           35           48           59                 0                 0
#> 6           60           75           89                 0                 0
#>   fragvoltot_l_nopf
#> 1                15
#> 2                35
#> 3                60
#> 4                 0
#> 5                 0
#> 6                 0


# example of texture_to_taxpartsize()
tex <- data.frame(texcl = c("c", "cl", "l", "ls", "s"),
                  clay  = c(55, 33, 18, 6, 3),
                  sand  = c(20, 33, 42, 82, 93),
                  fragvoltot = c(35, 15, 34, 60, 91))
tex$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
                                   clay = tex$clay,
                                   sand = tex$sand,
                                   fragvoltot = tex$fragvoltot)
head(tex)
#>   texcl clay sand fragvoltot            fpsc
#> 1     c   55   20         35 clayey-skeletal
#> 2    cl   33   33         15      fine-loamy
#> 3     l   18   42         34      fine-loamy
#> 4    ls    6   82         60  sandy-skeletal
#> 5     s    3   93         91      fragmental


# example of texture_to_taxpartsize() with carbonate clay and very fine sand
carbclay <- rnorm(5, 2, 3)
vfs <- rnorm(5, 10, 3)
st$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
                                  clay = tex$clay - carbclay,
                                  sand = tex$sand - vfs,
                                  fragvoltot = tex$fragvoltot)
#> Warning: some of the texcl records don't match the calculated texcl via ssc_to_texcl()
head(tex)
#>   texcl clay sand fragvoltot            fpsc
#> 1     c   55   20         35 clayey-skeletal
#> 2    cl   33   33         15      fine-loamy
#> 3     l   18   42         34      fine-loamy
#> 4    ls    6   82         60  sandy-skeletal
#> 5     s    3   93         91      fragmental


# example of sample = TRUE
texcl <- rep(c("cl", "sil", "sl"), 10)
ssc1 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = FALSE))
ssc2 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = TRUE))
ssc1$sample <- FALSE
ssc2$sample <- TRUE
ssc  <- rbind(ssc1, ssc2)
aggregate(clay ~ sample + texcl, data = ssc, summary)
#>   sample texcl clay.Min. clay.1st Qu. clay.Median clay.Mean clay.3rd Qu.
#> 1  FALSE    cl     33.00        33.00       33.00     33.00        33.00
#> 2   TRUE    cl     30.00        31.25       33.50     34.00        36.50
#> 3  FALSE   sil     13.00        13.00       13.00     13.00        13.00
#> 4   TRUE   sil      1.00         7.50       11.00     12.30        16.75
#> 5  FALSE    sl     10.00        10.00       10.00     10.00        10.00
#> 6   TRUE    sl     10.00        11.75       15.00     14.30        16.50
#>   clay.Max.
#> 1     33.00
#> 2     39.00
#> 3     13.00
#> 4     25.00
#> 5     10.00
#> 6     18.00
# }
# \donttest{
# example of texture_to_texmod()
tex <- c("SL", "GR-SL", "CBV-L", "SR- GR-FS GRX-COS")
texture_to_texmod(tex)
#> [1] NA         "gr"       "cbv"      "gr & grx"
texture_to_texmod(tex, duplicates = "max")
#> [1] NA    "gr"  "cbv" "grx"
# }
# \donttest{
# example of fragvol_to_texmod()
df <- expand.grid(
  gravel  = seq(0, 100, 5), 
  cobbles = seq(0, 100, 5), 
  stones  = seq(0, 100, 5), 
  boulders = seq(0, 100, 5)
  )
df <- df[rowSums(df) < 100, ]

# data.frame input
test <- fragvol_to_texmod(df)
table(test$texmod)
#> 
#>   by  byv  byx   cb  cbv  cbx   gr  grv  grx   st  stv  stx 
#>   81  510 2135   37  192  710   20   82  273   57  371 1502 
table(test$lieutex)
#> 
#>   by   cb   gr   st 
#> 1634   37    2 1197 

# vector inputs
fragvol_to_texmod(gravel = 10, cobbles = 10)
#>   texmod lieutex
#> 1     cb    <NA>

# }
```
