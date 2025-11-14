# Thompson-Bell (1996) Index

Calculate the "Profile Darkness Index" by the method of Thompson & Bell
(1996) "Color index for identifying hydric conditions for seasonally
saturated mollisols in Minnesota" DOI:
10.2136/sssaj1996.03615995006000060051x. The Thompson-Bell Index has
been shown to reflect catenary relationships in some Mollisols of
Minnesota (generally: wetter landscape positions = thicker, darker
surfaces).

## Usage

``` r
thompson.bell.darkness(
  p,
  name = hzdesgnname(p, required = TRUE),
  pattern = "^A",
  value = "m_value",
  chroma = "m_chroma"
)
```

## Arguments

- p:

  A single-profile SoilProfileCollection (e.g. via profileApply())

- name:

  Column name containing horizon designations used to find A horizons
  (default: first column name containing 'name')

- pattern:

  Regular expression to match A horizons (default: "^A" which means
  horizon designation *starts with* A)

- value:

  Column name containing horizon color values (default: "m_value")

- chroma:

  Column name containing horizon color chromas (default: "m_chroma")

## Value

A numeric vector reflecting horizon darkness (lower values = darker).

## References

Thompson, J.A. and Bell, J.C. (1996), Color Index for Identifying Hydric
Conditions for Seasonally Saturated Mollisols in Minnesota. Soil Science
Society of America Journal, 60: 1979-1988.
doi:10.2136/sssaj1996.03615995006000060051x

## Author

Andrew G. Brown
