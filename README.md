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
To cite the aqp package in publications use:

  Beaudette, D.E., Roudier P., and A.T. O'Geen. Algorithms for Quantitative Pedology: A Toolkit for Soil
  Scientists. 2012

A BibTeX entry for LaTeX users is:
```
@Manual{,
    title = {Algorithms for Quantitative Pedology: A Toolkit for Soil Scientists},
    author = {D.E. debeaudette@ucdavis.edu Beaudette and P. Roudier and A.T. O'Geen},
    year = {2013},
    url = {http://r-forge.r-project.org/projects/aqp/},
  }
```


## Related Packages
 * [soilDB](https://github.com/ncss-tech/soilDB)
 * [sharpshootR](https://github.com/ncss-tech/sharpshootR)
 
 
 
 