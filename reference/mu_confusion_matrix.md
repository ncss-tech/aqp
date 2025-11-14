# Map unit confusion matrix and other classification measures

This function reverse engineers a confusion matrix and other
classification measures from soil map unit component percentages (i.e.
composition) and area (i.e. acres).

## Usage

``` r
mu_confusion_matrix(
  x,
  mapunit = "nationalmusym",
  cophase = "coiid",
  comppct = "comppct_r",
  muacres = "muacres"
)
```

## Arguments

- x:

  `data.frame`

- mapunit:

  `character` column name containing the mapunit identifier (e.g.
  nationalmusym)

- cophase:

  `character` column name containing the soil component phase identifier
  (e.g. coiid or paste(compname, localphase))

- comppct:

  `character` column name containing the component percent (e.g.
  comppct_r)

- muacres:

  `character` column name containing the total area of the mapunit (e.g.
  muacres)

## Value

`list` a confusion matrix, overall purity (OP) (i.e. overall accuracy),
map unit purity (MP) (i.e. user accuracy), class representation (CR)
(i.e. producer accuracy), and Shannon entropy (E). The measure names
were selected to be consistent with the alternative terms proposed by
Lark (1995) and Brus (2011).

## Details

`mu_confusion_matrix` There are several common statistical measures used
to gauge the accuracy of categorical maps. These measures are typically
not estimated for soil surveys but can be inferred from a map unit’s
soil component composition percentages and size (i.e. acres). In
general, overall purity or accuracy (OP) is related to map unit kind
(e.g. consociations vs complexes). For several reasons, the “true”
accuracies are unknown, and these values should be interpreted as
Bayesian prior estimates. However, it is likely that the estimates are
optimistic if they haven't been derived from an external validation.
Existing and future digital soil mapping products could be used to
validate how optimistic the current OA estimates are.

## References

- Brus DJ, Kempen B, Heuvelink GBM. 2011. Sampling for validation of
  digital soil maps. European Journal of Soil Science. 62(3):394–407.
  [doi:10.1111/j.1365-2389.2011.01364.x](https://doi.org/10.1111/j.1365-2389.2011.01364.x)

- Lark RM. 1995. Components of accuracy of maps with special reference
  to discriminant analysis on remote sensor data. International Journal
  of Remote Sensing. 16(8):1461–1480.
  [doi:10.1080/01431169508954488](https://doi.org/10.1080/01431169508954488)

## See also

[`tauW()`](https://ncss-tech.github.io/aqp/reference/tauW.md),
[`shannonEntropy()`](https://ncss-tech.github.io/aqp/reference/shannonEntropy.md),
[`confusionIndex()`](https://ncss-tech.github.io/aqp/reference/confusionIndex.md)

## Author

Stephen Roecker

## Examples

``` r
# example data
mu <- rbind(
    data.frame(mapunit = "A", cophase = c("Alpha", "Beta"), comppct = c(90, 10), muacres = 100),
    data.frame(mapunit = "B", cophase = c("Beta", "Alpha"), comppct = c(70, 30), muacres = 1000)
    )

mu_confusion_matrix(mu, mapunit = "mapunit", cophase = "cophase", comppct = "comppct")
#> $confusion_matrix
#>         component phase
#> map unit Alpha Beta
#>        A    90   10
#>        B   300  700
#> 
#> $overall_purity
#> [1] 0.7181818
#> 
#> $map_purity
#>   A   B 
#> 0.9 0.7 
#> 
#> $class_representation
#>     Alpha      Beta 
#> 0.7692308 0.9859155 
#> 
#> $shannon_entropy
#>         A         B 
#> 0.4689956 0.8812909 
#> 
```
