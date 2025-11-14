# Sieve the Coarse Fraction of Soil

Sieve applies thresholds to a numeric vector of fragment diameter
values, returning fragment size classes. Particle diameter thresholds
are evaluated as `d < threshold`.

## Usage

``` r
fragmentSieve(
  diameter,
  sieves = NULL,
  ordered = FALSE,
  prefix = "",
  new_names = NULL,
  ...
)
```

## Arguments

- diameter:

  numeric. Vector of diameters of coarse fragments to "sieve". Default
  `sieves` are specified in millimeters.

- sieves:

  leave as `NULL` to use fragment class labels and diameters defined by
  [`fragmentClasses()`](https://ncss-tech.github.io/aqp/reference/fragmentClasses.md),
  or a named vector of fragment diameters. See examples.

- ordered:

  logical. Return as an ordered factor.

- prefix:

  character. Add a prefix to result names? Default: `""` adds no prefix.
  For example `"para"` might be used for size classes of pararock
  fragments.

- new_names:

  Optional: apply new labels to result classes. Should match length of
  `sieves`.

- ...:

  additional arguments to
  [`fragmentClasses()`](https://ncss-tech.github.io/aqp/reference/fragmentClasses.md),
  such as `sys`, `flat`, and `rounded`, see examples.

## Value

character. Size class labels based on names of `sieves`, `new_names`,
and `prefix` (if specified).

## References

Soil Science Division Staff. 2017. Soil survey manual. C. Ditzler, K.
Scheffe, and H.C. Monger (eds.). USDA Handbook 18. Government Printing
Office, Washington, D.C.

## See also

[`fragmentClasses()`](https://ncss-tech.github.io/aqp/reference/fragmentClasses.md)

## Examples

``` r
# use a simplified version of the USDA system
# common within NRCS/SPSD and NCSS
fragmentSieve(c(30, 125, 180, 500, 1000))
#> [1] "gravel"   "cobbles"  "cobbles"  "stones"   "boulders"

# pararock fragments
fragmentSieve(c(30, 125, 180, 500, 1000), prefix = 'para')
#> [1] "paragravel"   "paracobbles"  "paracobbles"  "parastones"   "paraboulders"

# result as an ordered factor
fragmentSieve(c(30, 125, 180, 500, 1000), ordered = TRUE)
#> [1] gravel   cobbles  cobbles  stones   boulders
#> Levels: gravel < cobbles < stones < boulders

# USDA system, flat size classes
fragmentSieve(c(30, 125, 180, 500, 1000), flat = TRUE)
#> [1] "channers"   "channers"   "flagstones" "stones"     "boulders"  

# alternative classification systems
fragmentSieve(c(30, 125, 180, 500, 1000), sys = 'usda')
#> [1] "coarse_gravel" "cobbles"       "cobbles"       "stones"       
#> [5] "boulders"     
fragmentSieve(c(30, 125, 180, 500, 1000), sys = 'international')
#> [1] "stones" "stones" "stones" "stones" "stones"
fragmentSieve(c(30, 125, 180, 500, 1000), sys = 'unified')
#> [1] "coarse_gravel" "cobbles"       "cobbles"       "boulders"     
#> [5] "boulders"     
fragmentSieve(c(30, 125, 180, 500, 1000), sys = 'aashto')
#> [1] "coarse_gravel" "broken_rock"   "broken_rock"   "broken_rock"  
#> [5] "broken_rock"  
fragmentSieve(c(30, 125, 180, 500, 1000), sys = 'mod.wentworth')
#> [1] "pebbles"  "cobbles"  "cobbles"  "boulders" "boulders"

# custom fragment labels / diameter
fragmentSieve(
  c(30, 125, 180, 500, 1000),
  sieves = c(clumps = 50, chunks = 300, blocks = 100000)
)
#> [1] "clumps" "chunks" "chunks" "blocks" "blocks"

# unnamed sieves, generic labels used
fragmentSieve(c(10, 50), sieves = c(30, 70))
#> [1] "class_1" "class_2"

fragmentSieve(c(10, 50), sieves = c(30, 70), ordered = TRUE)
#> [1] class_1 class_2
#> Levels: class_1 < class_2
 
```
