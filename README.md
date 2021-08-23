[![R build status](https://github.com/ncss-tech/aqp/workflows/R-CMD-check/badge.svg)](https://github.com/ncss-tech/aqp/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/aqp)](http://cran.r-project.org/web/packages/aqp)
[![Total_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/aqp)](https://cran.r-project.org/package=aqp)
[![aqp Manual](https://img.shields.io/badge/docs-HTML-informational)](http://ncss-tech.github.io/aqp/docs/)



# Algorithms for Quantitative Pedology (aqp) package for R

## Installation
Get the stable version from CRAN:

`install.packages('aqp', dep=TRUE)`

Get the development version from Github, after installing the CRAN version + dependencies:

`remotes::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade=FALSE, build=FALSE)`

<!-- aqp hexsticker! -->
<a href="https://raw.githubusercontent.com/ncss-tech/aqp/master/misc/hexstickers/aqp_sticker_v2.png">
<img src = "https://raw.githubusercontent.com/ncss-tech/aqp/master/misc/hexstickers/aqp_sticker_v2.png" alt = "aqp hexsticker (Paxton, Montauk, Woodbridge, Ridgebury, Whitman, Catden soil series dendogram)" title = "aqp hexsticker (Paxton, Montauk, Woodbridge, Ridgebury, Whitman, Catden soil series dendogram)" width = "45%" height = "45%" hspace="15" vspace="15" align="right"/></a>

## Website
[http://ncss-tech.github.io/AQP/](http://ncss-tech.github.io/AQP/)

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
Dylan Beaudette, Pierre Roudier and Andrew Brown (2021). aqp: Algorithms for Quantitative Pedology. R package version 1.29. https://CRAN.R-project.org/package=aqp

D.E. Beaudette, P. Roudier, A.T. O'Geen, Algorithms for quantitative pedology: A toolkit for soil scientists, Computers & Geosciences, Volume 52, March 2013, Pages 258-268, ISSN 0098-3004, http://dx.doi.org/10.1016/j.cageo.2012.10.020.
  

## Related Packages
 * [soilDB](https://github.com/ncss-tech/soilDB)
 * [sharpshootR](https://github.com/ncss-tech/sharpshootR)
 
## aqp in the Wild
   * https://www.pnas.org/content/115/26/6751
   * [as found by Scopus](https://www.scopus.com/results/citedbyresults.uri?sort=plf-f&cite=2-s2.0-84871520076&src=s&imp=t&sid=77a47f45322dcfd492772ab2198cbd60&sot=cite&sdt=a&sl=0&origin=inward&editSaveSearch=&txGid=2178c12c5b47dbcdd8b2f12cd9a81478)
   * [as found by Google Scholar](https://scholar.google.com/scholar?cites=14155970656017510549&as_sdt=5,29&sciodt=0,29&hl=en)
   * http://www.scielo.br/scielo.php?script=sci_arttext&pid=S2179-80872019000100121

   
   
## Dependency Graph
![](https://cran.microsoft.com/packagedata/graphs/aqp.png)

