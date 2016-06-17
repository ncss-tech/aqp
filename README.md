[![Travis-CI Build Status](https://travis-ci.org/ncss-tech/aqp.svg?branch=master)](https://travis-ci.org/ncss-tech/aqp)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/aqp)](http://cran.r-project.org/web/packages/aqp)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/aqp)](https://cran.r-project.org/package=aqp)

# Algorithms for Quantitative Pedology (aqp) package for R

## Installation
Get the stable version from CRAN:

`install.packages('aqp', dep=TRUE)`

Get the development version from Github, after installing the CRAN version + dependencies:

`devtools::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade_dependencies=FALSE)`

## Documentation
http://ncss-tech.github.io/AQP/

## Examples
```r
library(aqp)
data(sp4)
depths(sp4) <- id ~ top + bottom

par(mar=c(0,0,4,0))
plot(sp4, color='clay')
plot(sp4, color='CF')

```

## Citation
  D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, March 2013, Pages 258-268, ISSN 0098-3004, http://dx.doi.org/10.1016/j.cageo.2012.10.020.
  

## Related Packages
 * [soilDB](https://github.com/ncss-tech/soilDB)
 * [sharpshootR](https://github.com/ncss-tech/sharpshootR)
 
 
 
 
