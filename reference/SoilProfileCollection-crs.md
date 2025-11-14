# Get or Set Coordinate Reference System for SoilProfileCollection

`prj()`: Get Coordinate Reference System (Projection) metadata

`prj()<-`: Set Coordinate Reference System metadata for the
SoilProfileCollection

`proj4string()`: (Deprecated) Get Coordinate Reference System as PROJ4
String

`proj4string()<-`: (Deprecated) Set Coordinate Reference System metadata
for the SoilProfileCollection

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
prj(object, ...)

# S4 method for class 'SoilProfileCollection'
prj(object, ...) <- value

# S4 method for class 'SoilProfileCollection'
proj4string(obj)

# S4 method for class 'SoilProfileCollection'
proj4string(obj) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- ...:

  Additional arguments (not used)

- value:

  character. Representation of Coordinate Reference System as
  `"authority:code"`, integer EPSG code, WKT2019 / PROJ4 string, an sf
  `crs` or sp `CRS` object.

- obj:

  A SoilProfileCollection

## See also

[`initSpatial<-()`](https://ncss-tech.github.io/aqp/reference/initSpatial.md)
