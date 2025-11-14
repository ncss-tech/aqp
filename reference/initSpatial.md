# Initialize Spatial Data in a SoilProfileCollection

`initSpatial()<-`: Set the column names containing spatial data and the
corresponding coordinate reference system for a SoilProfileCollection.

`getSpatial()`: Get spatial data associated with a SoilProfileCollection

## Usage

``` r
# S4 method for class 'SoilProfileCollection,ANY,ANY'
initSpatial(object, crs = NULL) <- value

# S4 method for class 'SoilProfileCollection,ANY,character'
initSpatial(object, crs = NULL) <- value

# S4 method for class 'SoilProfileCollection'
getSpatial(object)

# S4 method for class 'SoilProfileCollection'
coordinates(obj)

# S4 method for class 'SoilProfileCollection,ANY'
coordinates(object) <- value

# S4 method for class 'SoilProfileCollection,character'
coordinates(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- crs:

  Optional: character. Representation of Coordinate Reference System as
  `"authority:code"`, integer EPSG code, WKT2019 or PROJ4 string, an sf
  `crs` or sp `CRS` object.

- value:

  A formula specifying names of columns containing geometry (x and y
  coordinates), or character with the column names

- obj:

  A SoilProfileCollection

## See also

[`prj()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-crs.md)

## Examples

``` r
data(sp5)

# coordinates are stored in x and y column of site
sp5$x <- rnorm(length(sp5))
sp5$y <- rnorm(length(sp5))

# coordinates takes a formula object as input
initSpatial(sp5) <- ~ x + y

# optionally specify Coordinate Reference System (crs) on left-hand side
initSpatial(sp5, crs = "OGC:CRS84") <- ~ x + y
```
