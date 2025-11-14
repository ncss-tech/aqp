# Allocate Particle Size Class for the Control Section.

This function aggregates information in the horizon table and allocates
it to the particle size class for the control section.

## Usage

``` r
hz_to_taxpartsize(
  x,
  y,
  taxpartsize = "taxpartsize",
  clay = "clay",
  idcol = "id",
  depthcols = c("top", "bottom")
)
```

## Arguments

- x:

  a `data.frame` containing the original horizon table.

- y:

  a `data.frame` containing the particle size control section depths for
  each idcol.

- taxpartsize:

  `character` column name for taxonomic family particle size class.

- clay:

  `character` column name for clay percent.

- idcol:

  character: column name of the pedon ID within the object.

- depthcols:

  a character vector of length 2 specifying the names of the horizon
  depths (e.g. `c("top", "bottom")`).

## Value

A `data.frame` object containing the original idcol, the aggregated
particle size control section allocation, and an aniso column to
indicate more than one contrasting class.

## Details

This function differs from
[`texture_to_taxpartsize`](https://ncss-tech.github.io/aqp/reference/texture.md)
in that is aggregates the results of
[`texture_to_taxpartsize`](https://ncss-tech.github.io/aqp/reference/texture.md),
and accounts for strongly contrasting particle size classes.

## See also

[`texture_to_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/texture.md),
[`lookup_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/lookup_taxpartsize.md)

## Author

Stephen Roecker

## Examples

``` r
h <- data.frame(
  id = 1,
  hzname = c("A", "BA", "Bw", "BC", "C"),
  top    = c(0, 10, 45, 60,  90),
  bottom = c(10, 45, 60, 90, 150),
  clay   = c(15, 16, 45, 20,  10),
  sand   = c(10, 35, 40, 50,  90),
  frags  = c(0,  5, 10, 38,  40)
)

h <- cbind(h,
           texcl = ssc_to_texcl(clay = h$clay, sand = h$sand))

pscs <- data.frame(id = 1,
                   top = 25,
                   bottom = 100)

h <- cbind(h,
           taxpartsize = texture_to_taxpartsize(
             texcl = h$texcl,
             clay  = h$clay,
             sand  = h$sand,
             fragvoltot = h$frags
           ))

depths(h) <- id ~ top + bottom

# set required metadata for estimatePSCS()
hzdesgnname(h) <- "hzname"
hztexclname(h) <- "texcl"
hzmetaname(h, "clay") <- "clay"

pscs <- data.frame(id = h$id, rbind(estimatePSCS(h)))
names(pscs)[2:3] <- c("top", "bottom")

hz_to_taxpartsize(horizons(h), pscs)
#>   id taxpartsizemod              taxpartsize
#> 1  1          aniso coarse-loamy over clayey
 
```
