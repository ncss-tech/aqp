# Ranking Systems for USDA Soil Texture Classes

Generate a vector of USDA soil texture codes or class names, sorted
according to approximate particle size

## Usage

``` r
SoilTextureLevels(which = "codes", simplify = FALSE)
```

## Arguments

- which:

  'codes' (texture codes) or 'names' (texture class names)

- simplify:

  Return 12-class factor levels (`TRUE`) or 21-class factor levels
  (default: `FALSE`)? The 12-class system does not separate sands, loamy
  sands and sandy loams into sand fraction variants (e.g. "very fine
  sandy loam" in the 21-class system is "sandy loam" in 12-class system)

## Value

an ordered factor

## References

[Field Book for Describing and Sampling Soils, version
3.0](https://nrcspad.sc.egov.usda.gov/DistributionCenter/product.aspx?ProductID=991)

## Examples

``` r
# class codes
SoilTextureLevels()
#>  [1] cos  s    fs   vfs  lcos ls   lfs  lvfs cosl sl   fsl  vfsl l    sil  si  
#> [16] scl  cl   sicl sc   sic  c   
#> 21 Levels: cos < s < fs < vfs < lcos < ls < lfs < lvfs < cosl < sl < ... < c

# class names
SoilTextureLevels(which = 'names')
#>  [1] coarse sand          sand                 fine sand           
#>  [4] very fine sand       loamy coarse sand    loamy sand          
#>  [7] loamy fine sandy     loamy very fine sand coarse sandy loam   
#> [10] sandy loam           fine sandy loam      very fine sandy loam
#> [13] loam                 silt loam            silt                
#> [16] sandy clay loam      clay loam            silty clay loam     
#> [19] sandy clay           silty clay           clay                
#> 21 Levels: coarse sand < sand < fine sand < ... < clay

# simpler class names
SoilTextureLevels(which = 'names', simplify = TRUE)
#>  [1] sand            loamy sand      sandy loam      loam           
#>  [5] silt loam       silt            sandy clay loam clay loam      
#>  [9] silty clay loam sandy clay      silty clay      clay           
#> 12 Levels: sand < loamy sand < sandy loam < loam < silt loam < ... < clay
 
```
