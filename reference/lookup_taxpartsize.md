# Ranking Systems for USDA Taxonomic Particle-Size and Substitute Classes of Mineral Soils

Generate a lookup table of USDA Particle-Size and Substitute Classes
names, ranked according to approximate particle size

## Usage

``` r
lookup_taxpartsize()
```

## Value

A data.frame with a rank column, taxonomic family particle size class,
and a flag for contrasting.

## References

[Field Book for Describing and Sampling Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991)

## See also

[`hz_to_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/hz_to_taxpartsize.md),
[`texture_to_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/texture.md),
[`SoilTextureLevels()`](https://ncss-tech.github.io/aqp/reference/SoilTextureLevels.md)

## Author

Stephen Roecker

## Examples

``` r
# class codes
lu <- lookup_taxpartsize()

idx <- lu$contrasting == FALSE

lu$taxpartsize[idx]
#>  [1] diatomaceous      very-fine         clayey            fine             
#>  [5] hydrous           fine-silty        fine-gypseous     fine-loamy       
#>  [9] medial            loamy             coarse-loamy      coarse-silty     
#> [13] coarse-gypseous   ashy              sandy             clayey-skeletal  
#> [17] hydrous-pumiceous hydrous-skeletal  medial-pumiceous  medial-skeletal  
#> [21] loamy-skeletal    gypseous-skeletal ashy-pumiceous    ashy-skeletal    
#> [25] sandy-skeletal    pumiceous         cindery           fragmental       
#> 99 Levels: diatomaceous < very-fine < clayey < ... < fragmental

lu$rank[as.integer(lu$taxpartsize)[idx]]
#>  [1]  84.00  74.00  60.02  46.04  44.04  26.00  25.80  25.60  24.00  17.24
#> [11]   8.88   8.50   7.50   6.50   4.67 -43.33 -55.96 -55.96 -76.00 -76.00
#> [21] -83.23 -83.35 -93.50 -93.50 -95.33 -95.83 -96.33 -98.94
```
