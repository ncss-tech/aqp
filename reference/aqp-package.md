# Algorithms for Quantitative Pedology

The aqp (Algorithms for Quantitative Pedology) package for R was
developed to address some of the difficulties associated with processing
soils information, specifically related to visualization, aggregation,
and classification of soil profile data. This package is based on a mix
of S3/S4 functions and classes, and most functions use basic dataframes
as input, where rows represent soil horizons and columns define
properties of those horizons. Common to most functions are the
requirements that horizon boundaries are defined as depth from 0, and
that profiles are uniquely defined by an id column. The aqp package
defines an S4 class, "SoilProfileCollection", for storage of
profile-level metadata, as well as summary, print, and plotting methods
that have been customized for common tasks related to soils data.

## Details

Demos: `demo(aqp)`

[Project homepage](http://ncss-tech.github.io/AQP/)

## See also

`depths<-()`,
[`SoilProfileCollection()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-class.md),
[`sp1`](https://ncss-tech.github.io/aqp/reference/sp1.md),
[`sp2`](https://ncss-tech.github.io/aqp/reference/sp2.md),
[`sp3`](https://ncss-tech.github.io/aqp/reference/sp3.md),
[`sp4`](https://ncss-tech.github.io/aqp/reference/sp4.md),
[`sp5`](https://ncss-tech.github.io/aqp/reference/sp5.md),
[`sp6`](https://ncss-tech.github.io/aqp/reference/sp6.md)

## Author

Dylan E. Beaudette <debeaudette@ucdavis.edu>, Pierre Roudier, Andrew G.
Brown
