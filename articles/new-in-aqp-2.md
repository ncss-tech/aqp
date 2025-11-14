# What is new in aqp 2.x?

This is a major update to {aqp} that may create some issues for code
depending on specific inputs/outputs as of {aqp} \<= 1.42, particularly
those relying on
[`slice()`](https://ncss-tech.github.io/aqp/reference/slice.md),
[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md), and
`profile_compare()`. As of {aqp} 2.0,
[`slice()`](https://ncss-tech.github.io/aqp/reference/slice.md) and
`profile_compare()` are deprecated, but will continue to work for the
rest of calendar year 2023. There are no plans to maintain these
functions beyond {aqp} 2.0. The new version of
[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md) is a
drop-in replacement for the previous version.

See the manual pages for
[`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md),
[`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md), and
[`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md). Note that
these new / updated functions perform stricter tests for horizon depth
logic errors. Messages and warnings are issued when invalid depth logic
is reported by
[`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md).
New functions
[`accumulateDepths()`](https://ncss-tech.github.io/aqp/reference/accumulateDepths.md),
[`repairMissingHzDepths()`](https://ncss-tech.github.io/aqp/reference/repairMissingHzDepths.md),
and
[`fillHzGaps()`](https://ncss-tech.github.io/aqp/reference/fillHzGaps.md)
can usually salvage problematic profiles.

The `replace_na` and `add_soil_flag` arguments to `profile_compare()`
are not present in
[`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md); missing
data are always replaced by maximum dissimilarity (1 when Gower’s
distance metric is used) and the soil/non-soil matrix is always used to
compare profiles of different depth.

## Notable Changes

- Deprecation of
  [`slice()`](https://ncss-tech.github.io/aqp/reference/slice.md) in
  favor of the new, faster, more robust implementation via
  [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md).
  Be sure to update existing code to use
  [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md)
  from now on. Some argument names have changed.
- Complete overhaul of
  [`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md) (thanks
  to @brownag), with new arguments, faster back-end, and weighted
  aggregation implemented (finally).
- Deprecation of `profile_compare()` in favor of the new
  [`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md)
  function–a complete overhaul based on Maynard et al., 2020. Note that:
  - site level attributes are now handled by
    [`compareSites()`](https://ncss-tech.github.io/aqp/reference/compareSites.md)
  - variable weights are specified by argument
- The functions
  [`perturb()`](https://ncss-tech.github.io/aqp/reference/perturb.md)
  and
  [`estimatePSCS()`](https://ncss-tech.github.io/aqp/reference/estimatePSCS.md)
  are now vectorized, and optimized for larger `SoilProfileCollection`
  objects.
- [`mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.md)
  now uses `mixingMethod = 'exact'` by default for the simulation of
  subtractive color mixtures
- `gower` package moved to SUGGESTS
- [`plotColorMixture()`](https://ncss-tech.github.io/aqp/reference/plotColorMixture.md)
  now using grid graphics functions to determine color swatch geometry
  and setting overlap detection threshold
- Deprecation of
  [`rgb2munsell()`](https://ncss-tech.github.io/aqp/reference/rgb2munsell.md)
  in favor of the more general
  [`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md)
- Removal of `PMS2Munsell()` and support data
- Deprecation of `coordinates()<-` and `proj4string()<-` in favor of
  `initSpatial()<-`
- Removal of `rruff.sample` example XRD patterns
- [`get.ml.hz()`](https://ncss-tech.github.io/aqp/reference/get.ml.hz.md)
  no longer uses the `name` argument

## Major changes to `plotSPC()`

- The maximum depth range of the figure is now based on `max.depth` or
  `max(x)`. This means that sketches generated with {aqp} 2.x will
  generally have less white space at the bottom of the figure. Make more
  room for additional annotation or visual effect by setting the desired
  depth range with the `max.depth` argument.
- Now using the
  [`electroStatics_1D()`](https://ncss-tech.github.io/aqp/reference/electroStatics_1D.md)
  method for fixing horizon depth label overlap, solutions are
  deterministic and almost always better. Adjust label overlap
  adjustment strategy with `fixOverlapArgs = list(...)`.
- Better depth axis interval heuristics (if not specified), varying
  based on figure depth range.
- Depth axis adjustments via new argument `depth.axis`, logical or list
- deprecation of arguments:
  - `plot.depth.axis`: set via `depth.axis = TRUE`,
    `depth.axis = FALSE`, or customize `depth.axis = list(...)`
  - `cex.depth.axis`: set via `depth.axis = list(cex = 1)`
  - `axis.line.offset`: set via `depth.axis = list(line = -2)`

## New Features

- New example data, `wilson2022`.
- Fast prototyping of `SoilProfileCollection` objects via
  [`quickSPC()`](https://ncss-tech.github.io/aqp/reference/quickSPC.md)
  and list / character templates.
- Re-use arguments to
  [`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)
  via setting `options(.aqp.plotSPC.args = list(...))`. Set to
  `options(.aqp.plotSPC.args = NULL)` to disable.
- Coarse fragment classification via
  [`fragmentSieve()`](https://ncss-tech.github.io/aqp/reference/fragmentSieve.md)
  and
  [`fragmentClasses()`](https://ncss-tech.github.io/aqp/reference/fragmentClasses.md).
- S4 `as.data.frame(<SPC>)` as shorthand for `as(<SPC>, 'data.frame')`
- [`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)
  now marks truncated profiles with a ragged bottom.
- [`fixOverlap()`](https://ncss-tech.github.io/aqp/reference/fixOverlap.md)
  now has [two label-placement
  solvers](https://ncss-tech.github.io/aqp/articles/label-placement.html):
  - electrostatic simulation (`method = "E"`)
  - simulated annealing (`method = "S"`)
- New [depth axis
  styles](https://ncss-tech.github.io/AQP/aqp/sketches.html) in
  [`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md).
- New function
  [`flagOverlappingHz()`](https://ncss-tech.github.io/aqp/reference/flagOverlappingHz.md)
  for identifying horizons with perfect overlap
- New function
  [`warpHorizons()`](https://ncss-tech.github.io/aqp/reference/warpHorizons.md)
  for warping horizon thickness (inflate/deflate)
- [`simulateColor()`](https://ncss-tech.github.io/aqp/reference/simulateColor.md)
  adds multivariate simulation in CIELAB colorspace

## Incremental changes, should have no effect on previous code

- Bug fix in
  [`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md)
  when `fixLabelCollisions = TRUE`, adjustments suggested to
  [`fixOverlap()`](https://ncss-tech.github.io/aqp/reference/fixOverlap.md)
  are now scaled correctly.
- [`explainPlotSPC()`](https://ncss-tech.github.io/aqp/reference/explainPlotSPC.md)
  reports label adjustment index when label collision repair is enabled.
- Aesthetic cleanup in
  [`explainPlotSPC()`](https://ncss-tech.github.io/aqp/reference/explainPlotSPC.md).
- [`soilColorSignature()`](https://ncss-tech.github.io/aqp/reference/soilColorSignature.md)
  gains arguments and perceptual color distances (dE00) via {farver}
  package.
- `as(<SPC>, "data.frame")`: Replace `plyr::join()` with
  [`merge()`](https://edzer.github.io/sp/reference/merge.html).
- [`correctAWC()`](https://ncss-tech.github.io/aqp/reference/correctAWC.md):
  NA handling - return NA when frags are NA.
- [`mutate_profile()`](https://ncss-tech.github.io/aqp/reference/mutate_profile.md):
  Faster (data.table-based) evaluation of profile-level expressions.
- `profileApply`: Add support for custom
  [`lapply()`](https://rdrr.io/r/base/lapply.html)-like function
  (`APPLY.FUN`) for processing chunks.
- Add `.interpretHorizonColor()` outputs to `last_spc_plot` in `aqp.env`
  for use in custom
  [`legend()`](https://rdrr.io/r/graphics/legend.html).
- Add `simplify` argument to
  [`SoilTextureLevels()`](https://ncss-tech.github.io/aqp/reference/SoilTextureLevels.md)
  and
  [`ssc_to_texcl()`](https://ncss-tech.github.io/aqp/reference/texture.md)
  to optionally convert to an ordered factor with maximum of 12 levels
  (rather than 21). This smaller list of classes excludes sand grain
  size variants such as fine sand, loamy coarse sand, and very fine
  sandy loam.
- Major updates to
  [`profileInformationIndex()`](https://ncss-tech.github.io/aqp/reference/profileInformationIndex.md)
  methods and performance boost.
- Enhancements to
  [`evalMissingData()`](https://ncss-tech.github.io/aqp/reference/evalMissingData.md).
