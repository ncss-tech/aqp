# aqp 1.42 (2022-04-08)
 * CRAN release
 * `getArgillicBounds()`, `getCambicBounds()`, `mollic.thickness.requirement()`, `getSurfaceHorizonDepth()` and related functions have been optimized and now work on SoilProfileCollection objects with length > 1
 * Added `reduceSPC()` function for selecting a subset of minimal plus additional specified columns from a SoilProfileCollection
 * Added new "k-keyword" `.NHZ` for use with `SoilProfileCollection` `[` method
   * `object[,,.NHZ]` returns a vector with the number of horizons in each profile
 * proper identification and conversion of Munsell value == 2.5
 * interpolated reference spectra for Munsell value == 2.5 
 * neutral color chips in the Munsell look-up table are now based on direct measurement vs. estimation

# aqp 1.41 (2022-02-11)
 * `plotSPC()` gains new arguments for controlling horizon depth labels
 * `plotSPC()` gains horizon depth annotation collision fixes (https://github.com/ncss-tech/aqp/issues/240)
 * `plotSPC()` minor bugfix for cases when `n != length(x)`
 * `plotSPC()` important change! `y.offset` is now automatically re-ordered by `plot.order`
 * bugfix for R 4.2, related to named arguments passed to `base::aggregate` (thanks AGB)
 * `depthOf()` and related functions: better handling of `NA` results from `FUN`

# aqp 1.40 (2022-01-04)
 * CRAN release
 * fixed small bug in `panel.depth_function()` that affected plotting of grouped data, created by `slab()`
 * `plotSPC()` now uses variable (vertical) text alignment for horizon depth labels
 * new function `profileInformationIndex()`
 * `estimateSoilDepth()` now requires a valid horizon designation (no longer attempts a guess)
 * {aqp} no longer masks {dplyr} verbs: `group_by()`, `summarize()`, `mutate()`
 * removed (deprecated) `f.noise()` and `resample.twotheta()`: use {powdR} package instead
 * new functions: `texture_to_texmod()` and `fragvol_to_texmod()` c/o @smroecker
 * faster, more robust version of `slice()` implemented as `dice()`

# aqp 1.32 (2021-09-28)
 * `colorChart()` can now show neutral hues in a skinny panel
 * updates to `colorContrast()`, `parseMunsell()`, and `huePosition()` to work properly with neutral hues
 * `huePosition()` gains new arguments and functionality: `origin` and `direction`
 * new function `huePositionCircle()` for helping visualize differences in Munsell hue on the unit circle
 * `plotSPC` defaults change:
   - `width = 0.25`: slightly wider profile sketches
   - `name = hzdesgnname(x)`: use metadata if present, no longer guessing. This may cause your figures to "lose" previously guessed horizon names, specify with `name = 'column name'` or set metadata with `hzdesgnname(x) <- 'hzname'`

# aqp 1.31 (2021-08-19)
 * CRAN release
 * bug fix in `checkHzDepthLogic()` when `byhz = TRUE`
 * new function `simulateColor()` for simulation of reasonable colors
 * new function `colorChart()` for [graphical depiction of Munsell chip frequency by group](https://ncss-tech.github.io/AQP/aqp/investigating-soil-color.html#Color_Book_Style_Presentation)

# aqp 1.30 (2021-07-14)
 * `unique` method for `SoilProfileCollection` objects now returns a `SoilProfileCollection` by default
   - this may break existing code! use the new argument `SPC = FALSE` for previous behavior (https://github.com/ncss-tech/aqp/issues/159)
 * `aggregateColor()` now uses `mixMunsell` for the estimation of soil color mixtures
 * `plotColorMixture()` will respect "names" attribute of colors-to-mix, without erroneous alpha-sorting
 * `parseMunsell()` now more robust and faster, c/o P. Roudier
 * `mixMunsell`:
    - new method `exact` for direct conversion of mixture spectra to sRGB or closest Munsell chip (via `spec2Munsell()`)
 * new convenience function `PMS2Munsell()` for converting PMS codes -> closest Munsell chip (https://github.com/ncss-tech/aqp/issues/124)
 * `glom()` `z1` and `z2` arguments vectorized to allow for profile-specific intervals
   *  `z1` and `z2` support non-standard evaluation based on column names in `siteNames(p)`, and also can take character vector (length 1) with column names in `siteNames(p)`
 * `depthOf()`, `minDepthOf()`, `maxDepthOf()`, `getSurfaceHorizonDepth()`, `getMineralSoilSurfaceDepth()`, `getPlowLayerDepth()` can now be applied to multiple profiles.
   *  If the input `SoilProfileCollection` has more than one profile then result is a `data.frame` containing profile ID, top or bottom depths, horizon designation and pattern

# aqp 1.29 (2021-04-05)
 * CRAN release
 * Several `SoilProfileCollection` methods that conflict with {dplyr} 1.0+ have been deprecated:
    * `filter`, `mutate`, `group_by`, `summarize`
    * New overloaded {base} names:
       - {base}-like syntax: "filter" -> `subset`, "mutate" -> `transform`, "combine" -> `c`
    * New unique {aqp}/SoilProfileCollection aliases:
       - `*SPC` syntax: `filterSPC`, `mutateSPC`, `groupSPC`, `summarizeSPC`
    * Be aware `dplyr::combine` (deprecated in {dplyr} 0.7) still conflicts 
 * `aqp::union()`, previously deprecated, has been removed from namespace
    * Use `c()` or `combine()` for `SoilProfileCollection` input
    * Use`combine()` or `pbindlist()` for `list` input
 * Methods that used {rlang} for non-standard evaluation now use {base}
 * `plotSPC()` gains vectorized `y.offset` support (demonstrated in: http://ncss-tech.github.io/AQP/aqp/genhz-distance-eval.html)
 * `plotSPC()` gains argument `shrink.thin` for shrinking horizon designation labels when horizons are thin
 * new function `alignTransect()` for simplifying relative positioning of profile sketches
 * `plotMultipleSPC()` gains ability to automatically merge thematic legends
 * `coordinates<-` will check formula terms (_unique_ coordinates) in the `@horizons` slot, if needed.
 * new function `spec2Munsell()` for converting reflectance spectra into sRGB coordinates or closest Munsell chip
 * `mixMunsell`:
    - new `mixingMethod` argument for selecting several mixing strategies
    - suggestions on interpreting spectral distances, message printed when greater than reasonable threshold
 * minor bug-fix in `panel.depth_function` when plotting grouped step-functions
 
# aqp 1.28 (2021-03-02)
 * optimization of SoilProfileCollection `[,j]`-index extraction using {data.table}
 * introduction of `.LAST`, `.FIRST` and `.HZID` SoilProfileCollection "k-keywords"
 * `perturb()` is the new generalized replacement for `sim()` and `permute_profile()`
 * `checkHzDepthLogic()` now has a `byhz` argument for checking logic by _horizon_ rather than profile
 * `fillHzGaps` now has `to_top` and `to_bottom` arguments for filling above shallowest top / deepest bottom by profile
 * `fixOverlap()` more flexible and will usually settle on a solution in fewer iterations:
    - `overlapMetrics()` instead of `findOverlap()` for part of objective function
    - cooling schedule is now fully adjustable via `T0` and `k` arguments
 * `alignTransect` helper function for computing relative positions and ordering vector supplied to `plotSPC`
 * `plotSPC()` automatically converts `logical` data supplied to `color` argument into `factor`
 * `glom()` is now vectorized over profiles
 * `estimateAWC` introduced for testing lookup table estimation of available water capacity (AWC) of fine-earth fraction
 * `correctAWC` introduced for testing corrections of AWC estimates for rock fragment and salts
 
# aqp 1.27 (2021-01-22)
 * `fillHzGaps`: new function for fixing horizon depth topological errors and padding top/bottom of profiles with placeholder (empty) horizons
 * `mixMunsell` now relies on suggested package {gower} for 5-10x speed bump
 * {aqp} no longer imports from {reshape} (less one dependency), all transformations from wide<->long are done via {data.table}
 * methods from {data.table} are now imported by {aqp} (new dependency)
 * Major overhaul of `plotColorQuantiles()`, now using {lattice} graphics
 * New dataset `equivalent_munsell` and method `equivalentMunsellChips` for "equivalent" Munsell chips lookup list based on all pairwise dE00 contrasts for integer "chips" in {aqp} `munsell` data set
 * Argillic critical clay contents `crit.clay.argillic` rounded to whole numbers per NSSH Part 614, subpart B, sections 614.13 and 614.14

# aqp 1.26 (2020-11-18)
 * `mutate_profile` uses `data.table::rbindlist(fill=TRUE)` to combine site- and horizon-level transformations
 * updates to horizon boundary encoding functions (`hzTopographyCodeToOffset`, `hzTopographyCodeToLineType`, `hzDistinctnessCodeToOffset`)
 * new function `L1_profiles` computes multivariate (L1) medians, compare to marginal medians via `slab`
 * `plotSPC` updates:
   + argument named changes: `hz.boundary.lty` is a horizon-level attribute that contains line type codes
   + `hz.topography.offset` a horizon-level attribute that contains representative offsets that encode horizon boundary topography
   + `plotSPC` now encodes `hz.topography.offset` using a vertical "bump" (chevron)
 * `addBracket` can now accept multiple bracket annotations per profile

# aqp 1.25 (2020-10-15)
 * CRAN release
 * new lookup table `pms.munsell.lut` for converting Pantone spot color codes to (closest) Munsell chip
 * new function `duplicate` will makes copies of profiles within a `SoilProfileCollection`
 * new example data `us.state.soils`: 50 state soils + PR and VI soils
 * simulate subtractive mixtures of Munsell colors with `mixMunsell`
   + see companion function `plotColorMixture` for visualization of spectra / mixture
 * complete overhaul of `textureTriangleSummary`:
   + uses `soiltexture` package for visualization (`plotrix` implementation dropped)
   + argument names changes (! may break old code, sorry)
   + dropped simulation via `sim = TRUE` argument, see `bootstrapSoilTexture` for a better approach
 * new function `bootstrapSoilTexture` for simulating realistic sand/silt/clay compositions
 * `combine` replaces/expands `aqp::union` due to conflicts with `base::union`
 * `split` receives some upgrades to the S4 definition to increase parity with `split.default`
 * `filter` is now an alias for new method `subset`, which mirrors `base::subset`
 
# aqp 1.24 (2020-08-31)
 * `estimateSoilDepth` **loses** `top` and `bottom` arguments, these are automatically extracted
 * two new SoilProfileCollection wrapper methods: `munsell2SPC`, `spc2mpspline`
 * add `returnData` argument to `contrastChart`
 * improvements to `glom(..., invert=TRUE)`, `glomApply`, and better tests
 * new wrapper method around `glomApply`: `aqp::trunc` for cases when top and bottom depth interval is the same for all profiles in a _SoilProfileCollection_
 * fix for routing of `NULL` through `$<-` and `horizons<-` or `site<-` (https://github.com/ncss-tech/aqp/issues/163)
 * fix handling of missing metadata in (old) serialized _SoilProfileCollection_ objects
 * fix for promotion of `data.table` with character vector (not formula) interface

# aqp 1.23 (2020-07-14)
 * enhanced _SoilProfileCollection_ object validity checks via S4; new method `spc_in_sync` (https://github.com/ncss-tech/aqp/pull/152)
 * optimization of `[` subset method and optional use of `data.table` (https://github.com/ncss-tech/aqp/pull/155)
 * `depths<-` has been optimized and minimally validates input data
 * default horizon ID (`hzID`) is now a `character` data type
 * `aqp::union` uses `depths<-` internally; explicitly enforcing profile ID + top depth order in horizon data is safer but results in different ordering if `union`-ing IDs that "intermingle"  (need to be re-sorted). 
 * new experimental method is `permute_profile`; similar to `sim` but for boundaries. The interface to this function is likely to change/be expanded.
 * added `segment` c/o @smroecker
 * fix for unit-length and zero-length legends in `plotSPC`
 * fix for `plot` generic to show `aqp::plot` in `?plot`

# aqp 1.22 (2020-06-24)
 * basic support for promotion of `tbl_df` and `data.table` to _SoilProfileCollection_
 * new method `aqp_df_class` to determine class name in use in a _SoilProfileCollection_ object
 * `plotSPC` upgrades (https://github.com/ncss-tech/aqp/pull/146)
 * new methods related to mollic epipedon: `mollic.thickness.requirement`, `hasDarkColors`
 * new `estimateSoilDepth`-like methods for depth to multiple features via pattern matching: `depthOf`, `minDepthOf`, `maxDepthOf`
 * soil texture helper functions (`ssc_to_texcl`, `texcl_to_ssc`, `texmod_to_fragvoltot`, `texture_to_taxpartsize`)
 * optimization of `[i,]` `[,j]` subset methods for _data.frame_-based slots (https://github.com/ncss-tech/aqp/issues/135)
 * new verbs: `mutate`, `mutate_profile` (https://github.com/ncss-tech/aqp/issues/118)
 * ROSETTA centroids and water retention by texture class (https://github.com/ncss-tech/aqp/issues/131)
 * fix for `getSurfaceHorizonDepth` with buried horizons / non-contiguous instances of matching horizons (https://github.com/ncss-tech/aqp/issues/132)
 * fix for default `plotSPC` with small number of profiles ((https://github.com/ncss-tech/aqp/issues/128)
 * remove implicit conversion to SpatialPointsDataFrame with unit-length `[` j-index subset ((https://github.com/ncss-tech/aqp/issues/125)
 * fix in slab when `slab.structure[2] > max(x)`
 
# aqp 1.19.01 (2020-02-07)
 * proof of concept for tidy SoilProfileCollection subsetting
 * define `[[` subsetting method; an "ambivalent" accessor for site- or horizon-level properties
 * new subset verbs `grepSPC`, `filter`, `subApply` for use in `%>%`-lines 
 
# aqp 1.19 (2020-01-22)
 * CRAN release
 * new functions: `hzDesgn()`, get horizon designations from a SPC
 * new functions: `hzdesgnname()`/`hzdesgnname()<-` and `hztexclname()`/`hztexclname()<-` get/set column containing horizon designations and texture classes
 * better error/logic handling for `glom()`

# aqp 1.18.5 (2020-01-21)
  * `profileApply()` enhancement for large `SoilProfileCollection` objects (https://github.com/ncss-tech/aqp/issues/112)
  * add `profileApply()` `frameify` argument for `data.frame` output (https://github.com/ncss-tech/aqp/issues/111)
  * `checkHzDepthLogic()` replaces `test_hz_logic()`
  * thanks to [farver >= 2.0.3](https://CRAN.R-project.org/package=farver), `rgb2munsell()` and `colorQuantiles()` now uses the CIE2000 distance metric for color comparison
  
# aqp 1.18.4 (2020-01-06)
  * bug fix (rare) when setting / replacing horizon attributes (https://github.com/ncss-tech/aqp/issues/105)
  * bug fix in `colorQuantiles` until `farver` 2.0.2 is available on CRAN
  * `SoilProfileCollection` object gains new slot: `@restrictions`, fix old objects with `rebuildSPC()`

# aqp 1.18.3 (2019-12-19)
  * `evalMissingData()` gets new argument for relative vs. absolute evaluation of missing data
  * `horizonColorIndices()`, `harden.rubification()`, `harden.melanization()`, `thompson.bell.darkness()` and associated functions in soilColorIndices.R
  * fix for https://github.com/ncss-tech/aqp/issues/44
  * fix for https://github.com/ncss-tech/aqp/issues/66
  * new example data: `rowley2019`
  * dropped some dependencies

# aqp 1.17.10 (2019-10-30)
  * removing imported functions from `Hmisc` (`hdquantile`), loading `aqp` is now much faster
     + `slab()` now uses `stats::quantile()` as the default slab function
     + details at: https://github.com/ncss-tech/aqp/issues/79
     + previous behavior of `slab()` can be activated via argument: `slab.fun = aqp:::.slab.fun.numeric.HD`

# aqp 1.17.08 (2019-10-03)
  * `generalize.hz()` gets a \dots argument for passing additional arguments to `grep()`, e.g. `perl=TRUE`
  * `addVolumeFraction()` can now accept a vector of colors, as many as number of horizons
  * new sample data: `jacobs2000`, pending documentation
  * `aggregateColor()` faster and more accurate, using delta-E00 for quantized colors c/o {farver} (see https://github.com/ncss-tech/aqp/issues/98)
  * new functions `contrastChart()` and `soilPalette()`
  * aqp 1.18 scheduled for next CRAN release

# aqp 1.17.06 (2019-07-15)
   * `plotSPC()` gains a new argument for relative positioning: relative.pos
   * relative positioning helper function: `fixOverlap()`, see manual page for examples
   * `explainPlotSPC()`, `addDiagnosticBracket()`, and `addVolumeFraction()` updated accordingly

# aqp 1.17.05 (2019-05-18)
   * new functions colorContrast(), contrastClass(), and huePosition()

# aqp 1.17.02 (2019-03-15)
   * fixed nasty bug in [-indexing when SPC contains a single profile (https://github.com/ncss-tech/aqp/issues/85)
   * new function validSpatialData() to test for place-holder contents of @sp
   * rebuildSPC() will now correct invalid, place-holder SpatialPoints in @sp

# aqp 1.17.01 (2019-02-07)
   * new version number system
   * `plotSPC()` will now attempt to create multi-row legends when using a factor > n.legend levels
   * `plotSPC()` will now generate >  10 colors for legends associated with a factor (https://github.com/ncss-tech/aqp/issues/9)
   * `munsell2rgb()` will now return CIE LAB coordinates if requested (https://github.com/ncss-tech/aqp/issues/69)

# aqp 1.17 (2018-12-28)
   * CRAN release
   * `aggregateColor()` gets a new feature, similar colors can be grouped via cluster::pam()
   * new functions: `previewColors()`, `colorQuantiles()`, `plotColorQuantiles()`
   * new function: `horizonDepths()<-`, edit top/bottom names after SPC init
   * new function: `profile_id()<-`, edit profile IDs after init; be careful!
   * new functions: `hzID()` and `hzID()<-`, get/set unique horizon IDs
   * new functions: `hzidname()` and `hzidname()<-`, get/set column containing unique horizon IDs
   * `rbind.SoilProfileCollection()` has been deprecated in favor of `union()`, gains new functionality: https://github.com/ncss-tech/aqp/issues/71
   * bug fixes in sanity checks within `horizonNames()<-`
   * **!!! SoilProfileCollection internal structure has changed: https://github.com/ncss-tech/aqp/issues/74**
   * see release notes for more: https://github.com/ncss-tech/aqp/releases/tag/v1.17

# aqp 1.16-6 (2018-12-12)
   * partial bug fix in test_hz_logic() related to missing top AND bottom depths, needs work: https://github.com/ncss-tech/aqp/issues/65

# aqp 1.16-5 (2018-11-21)
   * moved `explainPlotSPC` from SPC tutorial to `aqp::explainPlotSPC()`
   * `plotSPC()` is a little better at estimating "extra" vertical / horizontal space, still needs work (https://github.com/ncss-tech/aqp/issues/62)

# aqp 1.16-2 (2018-05-22)
   * fixing #53 and #54
   * bug fix for too many decimal places in legend classes (floating point -> character issue)
   * `plotSPC()` gets a new argument, `n.legend`: approximate number of classes

# aqp 1.15.5 (2018-03-14)
   * enhancement to `aggregateColor()` for cases when horizon depths are missing or top/bottom logic flipped

# aqp 1.15.4 (2018-01-29)
   * bug fix for `addVolumeFraction()` when fragment volume > 100%

# aqp 1.15.3 (2018-01-24)
   * added a new argument to `addDiagnosticBracket()` allowing for specification of column containing diagnostic feature kind
   * `addBracket()` now requires a data.frame with plotting details, see manual page

# aqp 1.15.2 (2017-11-30)
   * added `shannonEntropy()`, `confusionIndex()`, and `brierScore()`
   * bug fix in `get.ml.hz()` for horizon names that start with number or punctuation
   * `get.ml.hz()` now uses internal aqp functions for Shannon entropy and Brier scores

# aqp 1.15 (2017-11-11)
   * push to CRAN, getting ready for aqp 2.0

# aqp 1.14 (2017-09-19)
   * `missingDataGrid()` relaxing `slice()` with `strict=FALSE`

# aqp 1.12 (2017-07-05)
   * CIELAB colors are now in the `munsell` look-up table

# aqp 1.12 (2017-07-05)
   * `plotSPC()` now able to deal with missing horizon data (thanks Stephen R.)
   * this is likely the last release before aqp 2.0

# aqp 1.11 (2017-06-13)
   * `rgb2munsell()` now uses CIELAB colorspace for lookup, results are *more accurate* as compared to using sRGB colorspace

# aqp 1.10-4 (2017-05-02)
   * `texture.triangle.low.rv.high()` renamed to `textureTriangleSummary()`. The old name still works, but a message is issued
   * new argument to `textureTriangleSummary()` `texture.names`: for toggling texture class names
   * minor bug fix in `textureTriangleSummary(..., sim=TRUE)`, previous simulated compositional data was not correct because the stats::var() was being used vs. compositions::var.acomp(). the variance / covariance values were 2-5x too small.
   * new function `tauW()`, added by D.G. Rossiter: see manual page for references

# aqp 1.10 (2017-01-05)
   * fixed major bug (https://github.com/ncss-tech/aqp/issues/23) related to editing horizon-level attributes after `rbind`-ing

# aqp 1.9.13 (2016-10-20)
   * new dataset `soil_minerals` with some common soil mineral colors source: http://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/edu/?cid=nrcs142p2_054286
   * added clarification to Munsell color conversion: changed "RGB" to "sRGB". 

# aqp 1.9.10 (2016-07-01)
   * new function for estimating the closest Munsell chip: `getClosestMunsellChip()`
   * new example data set (`sp6`) based on http://www.sciencedirect.com/science/article/pii/S0016706111001972

# aqp 1.9.8 (2016-03-07)
   * `munsell2rgb()` can now accommodate neutral hues (N), and values of "2.5", see manual page for details

# aqp 1.9.7 (2016-02-25)
   * new function: `soilColorSignature()`, see manual page
   * new function: `parseMunsell()` for converting strings of '10YR 2/2' into R colors or RGB triplets
      + this is a convenience function that calls `munsell2rgb` on parsed strings of full Munsell notation

# aqp 1.9.6 (2016-02-23)
   * minor changes to `profile_compare()`, getting ready for a complete overhaul to this function
   * new function: `evalMissingData()`, see manual page

# aqp 1.9.5 (2015-12-28)
   * documentation for new transition probability functions, see ?hzTransitionProbabilities for details

# aqp 1.9.4 (2015-12-22)
   * better checking for other places where the bug fixed in rbind.SoilProfileCollection (aqp 1.9.3) could occur
   * new functions: `genhzTableToAdjMat`, `hzTransitionProbabilities`

# aqp 1.9.3 (2015-12-18)
   * fixed a nasty bug in `rbind.SoilProfileCollection()` related to object re-ordering
      + in aqp < 1.9.3: there was a chance than `profile_id(x)` and `site(x)[[idname(x)]])` would not match
      + bugs like this will no longer be a possibility after aqp 2.0, new SPC object structure planned

# aqp 1.9.1 (2015-11-18)
   * `groupedProfilePlot()` gains some new arguments and better documentation (see manual page)
   * `plot.SoilProfileCollection` gains argument: `default.color` (see manual page)

# aqp 1.9 (2015-08-26)
   * added neutral colors to `munsell2rgb()`
   * new function `guessGenHzLevels()`
   * new function `aggregateColor()` and associated helper function `sharpshootR::aggregateColorPlot()`
   * more code clean-up and fixes in examples to make `R CMD check --as-cran` happy
   * fixed some ancient examples in the `profileApply()` manual page

# aqp 1.8-5 (2015-04-17)
   * bug fixes and documentation updates
   * `digest::digest` is now imported by default
   * better documentation for unique-method of `SoilProfileCollection` objects

# aqp 1.8-3 (2015-03-02)
   * updated documentation
   * plotSPC gets a new argument: `x.idx.offset`: allows for plotting of multiple SPC objects within the same figure, see examples
   * plotSCP gets a new argument: `plot.depth.axis`: enable / disable depth axis, useful for multi-collection plots
   * new function: `profileGroupLabels`: simple annotation for groups of profiles within a profile sketch, see manual page for examples
   * new function: `plotMultipleSPC`: plot several SPC objects on the same axis, see manual page for examples

# aqp 1.7-11 (2015-01-30)
   * fixed bugs in `addBrackets()`, `panel.depth_function()`, and `plotSPC()`
   * new arguments to `addBrackets()` allow for annotation of brackets

# aqp 1.7-10 (2015-01-15)
   * names() method for SoilProfileCollection objects returns a concatenated vector of horizon + site names
   * siteNames() and horizonNames() can now be used to extract from SoilProfileCollection
   * siteNames<- and horizonNames<- can now be used to change names; experimental!
   * note: internal structure of SoilProfileCollection objects will be changing soon!!!

# aqp 1.7-8 (2014-12-08)
   * texture.triangle.low.rv.high(): new arguments, likely breaking previous usage when method='closest', see manual page for details

# aqp 1.7-7 (2014-11-06)
   * bug fix c/o Jos? Padarian: SPC objects now understand logical indexing rules
   * removed spatial_subset(): this functionality can be accomplished outside of AQP and removes dependency on rgeos package

# aqp 1.7-6 (2014-09-26)
   * bug fix c/o Jos? Padarian: when promoting coordinates from @site, drop=FALSE is required to prevent a single remaining attribute from being down-graded to a vector-- thanks!
   * get.ml.hz() now returns a "pseudo" Brier Score, using the most-likely horizon label as the "correct" value
   * new function rgb2munsell()
   * new function estimateSoilDepth()
   * new function getSoilDepthClass()
   * new documentation / tutorials

# aqp 1.6-4 (2014-02-14)
   * When applied to a single categorical variable, the results from slab() now contain an attribute 'original.levels'
  that contains the original factor levels. This is important because when casting from long->wide format, illegal
  column names are scrubbed by make.names(). This process would convert horizon designations like '2Bt' into 'X2Bt'.
  You can recover the original horizon names via attr(x, 'original.levels').

# aqp 1.6-3 (2014-02-04)
   * plotSPC() has a new argument: `label`, used to set site-level attribute containing profile labels

# aqp 1.6-2 (2014-01-10)
   * plotSPC() now registers plotting parameters in the environment aqp.env
   * new function addVolumeFraction() for annotating profile plots with volumetric information (e.g. rock fragment volume)

# aqp 1.6-1 (2013-12-31)
   * plotSPC() attempts to guess which column contains horizon designations
   * plotSPC() can now color horizons based on continuous soil properties (e.g. clay content)

# aqp 1.6 (2013-12-11)
   * rbind.SoilProfileCollection() now re-orders data according to the new set of profile IDs
      + this fixes several problems associated with assumptions in profile_compare()
   * profile_compare() will be re-written soon to work natively with SPC objects

# aqp 1.5-6 (2013-11-25)
   * new function addBracket() for annotating profile sketches with depth-brackets; useful for plotting diagnostic features

# aqp 1.5-6 (2013-11-15)
   * new function missingDataGrid() for graphically describing the presence of missing data in an SPC object

# aqp 1.5-2 (2013-04-09)
   * panel.depth_function() now prints "contributing fraction" values in the same color as corresponding depth-function lines and shaded areas
   * panel.depth_function() gains an argument 'cf.col' for manually specifying the CF color
   * panel.depth_function() gains an argument 'cf.interval' for manually specifying printed CF depths
   * prepanel.depth_function() now extends x-lim by 5% in either direction for cleaner-looking plots
   * added some details to slab() documentation
   * fixed minor bug in min() and max() methods for SoilProfileCollection objects in the presence of missing data
   * min() and max() methods for SoilProfileCollection objects now have a second argument for checking depth based on the presence of data in that column

# aqp 1.5-1 (2013-03-07)
   * removed excessive checking in SoilProfileCollection init code, this may speed things up
      + previously identified horizonation errors may no longer result in ERRORS; be sure to test
   * test_hz_logic() now more useful:
      + always checks for NA and overlapping horizons
      + no longer called with initialization of SoilProfileCollection objects
      + note that default behavior has changed since version 1.5
      + moved related documentation to test_hz_logic manual page
   * new mini-vignette on dealing with bad data (dealing-with-bad-data.html)
   * better documentation for slice()
   * new warnings related to missing data in the grouping variable passed to slab()

# aqp 1.5 (2013-02-24)
   * profileApply() can now return arbitrary objects = new subsetting/transformation possibilities
      + see ?profileApply for examples
   * panel.depth_function() can now display contributing fraction values with grouped data
      + see multi-variate example in ?slab

# aqp 1.4-7 (2013-01-24)
   * HTML version of all documentation (including images) auto-generated via knitr and available on the r-forge website
   * new function: `texture.triangle.low.rv.high()` plots texture data with low-high range defined by quantiles

# aqp 1.4-6 (2013-01-03)
   * updated documentation and SoilProfileCollection tutorial
   * plans laid for parallel processing in `slab()` and `profileApply()` via 'parallel' package
   * `plotSPC()` gets a `density` argument for shaded coloring of horizons

# aqp 1.4-5 (2012-12-29)
   * `slab()` re-written from scratch, resulting in reduced memory footprint and quicker running time
  ! older code based on slab() is likely to now broken- sorry, it won't happen again
      + the new code is much slimmer and based on aggregate() rather than ddply
      + function arguments have changed so be sure to check the manual page
      + user-supplied functions are now much simpler to write and rely on vectors rather than data.frames (= faster)
      + the next major release will contain additional examples of how to use the new features

# aqp 1.4 (2012-12-18)
   * new function `sim()` for simulation based on a template soil profile, proper documentation pending...

# aqp 1.3-3 (2012-12-06)
   * new function `subsetProfiles()` for simpler subsetting of `SoilProfileCollection` objects via site or horizon-level attributes
   * `get.ml.hz()` now returns a 'confidence' for each horizon (as a percent)
   * added experimental 'filter' argument to profile_compare()- see manual page for usage
   * added new argument to plot.SoilProfileCollection()- alt.label: used to add secondary labeling via data in @site
   * plot.SoilProfileCollection() will now do a better job of selecting the location for profile IDs based on label length, font size, and figure size

# aqp 1.3-2 (2012-11-01)
   * minor tweaks to plot.SoilProfileCollection() for improved output with small/large size collections
   * new function get.ml.hz() attempts to determine the most likely horizonation after slabbing
   * fixed bug in profileApply() where further arguments to FUN weren't passed on to FUN
   * missing data report from profile_compare() now only includes horizons with some data (e.g. Cr and R horizons are skipped)

# aqp 1.3-1 (2012-10-17)
   * speed-bump in profileApply()
      + splitProfiles() function is no longer defined/used/needed
   * bug fix in SoilProfileCollection subsetting via []
      + SoilProfiles with a single horizon confused subsetting in the presence of spatial data

# aqp 1.3 (2012-10-07)
   * added rbind.SoilProfileCollection() for concatenating SPC objects
      + this function performs basic sanity checks, but cannot combine unlike objects

# aqp 1.2-10 (2012-09-05)
   * fixed minor bug in prepanel.depth_function() where an error would occur when input data was all NA

# aqp 1.2-9 (2012-08-31)
   * new 'unique' method for SoilProfileCollection objects, returns indices of unique profiles
   * fixed bug in NAMESPACE that caused an error when 'sp' library wasn't loaded
   * added another sanity check to 'site<-' for cases where duplicates occur in the source data set
   * added some new documentation to package help page see 'help(aqp)' for details

# aqp 1.2-8 (2012-08-28)
   * the names() method for SoilProfileCollection objects now returns a named list of horizon and site column names
   * improved examples: see ?sp4 and ?ca630
   * fixed minor bug related to subsetting a SoilProfileCollection object, followed by coercion to SpatialPointsDataFrame

# aqp 1.2-7 (2012-08-23)
   * plot() method for SoilProfileCollection object no longer sets page margins, this will cause some figures to change
   * added demo 'slope_effect_hz_thickness'

# aqp 1.2-6 (2012-06-26)
   * fixed minor bug in panel.depth_function(): grouping + paneling ignored full suite of colors
   * fixed minor bug in NAMESPACE
   * minor bug fix in unroll(), was not previously respecting the max depth argument
   * minor speed-bump and simplification of profile_compare()
   * documentation updates
   * added helper message related to profile_compare() stopping when any profile contains insufficient data
   * profile_compare() now issues a notice when _some_ data are missing, leading to biased results
   * slice() now computes the fraction of variables within a slice that are missing data
! bug introduced into slab() when use.wts=TRUE [should have a fix soon]

# aqp 1.1 (2012-04-18)
   * merged documentation for soil.slot() and slab()
      + slab() should be used in all places where soil.slot() was used
      + soil.slot() will disappear from the package NAMESPACE eventually, so don't use it directly!
   * minor bug fix in panel.depth_function()
   * cleanup of documentation and examples
   * added horizon boundary distinctness code conversions and plotting functionality
      + see ?sp1 and ?random_profile for new examples

# aqp 1.0-2 (2012-03-29)
   * More NAMESPACE cleaning

# aqp 1.0-1 (2012-03-28)
   * Cleaned NAMESPACE and DESCRIPTION
   * removed un-necessary dependencies
   * join added to the plyr imports
   * added scales to the Imports as well, and removed rescaler() in the code, repaced by scales::rescale
   * hack in munsell2rgb to avoid the 'no visible binding for global variable 'munsell' at package R CMD check and make the whole thing more robust (using load and system.file rather than data(...)
   * Dylan is now the only maintainer, as per Uwe request
   * when using plyr **ply function, switched from using the .(foo) interface to "foo" - seems more foolproof

# aqp 1.0 (2012-03-26)
   * 1.0 release, still missing condensed vignettes- should be ready soon
   * see http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56 for samples
   * A small bug in profile_compare() was fixed, where slices were evaluated as 'soil' based on the bottom depth of the profile, and NOT on the presence of actual data. See ?profile_compare for details. This change will have a minor affect on profile comparisons in cases where Cr or R horizons (usually missing most characterization data) have been extended down to some arbitrary depth (usually 150 or 200 cm) AND a maximum depth of evaluation (max_d) was set beyond the actual depth of most profiles in the collection.

# aqp 0.99-9.8 (2012-03-02)
   * profile_compare() will now use site-level attributes if input is a SoilProfileCollection
      + horizon / site level data currently weighted equally
      + NOTE: this feature is new, test-first!
   * minor bug fixes in profile_compare() to remove spurious warnings
   * better error check in slice()
   * removed old code and documentation: format_slices() and plot_slices()

# aqp 0.99-9.6 (2012-02-21)
   * spurious warnings from profile_compare() caused by daisy() now suppressed
      + warnings were generated when calling daisy() on a matrix of all NA which typically occurred when using too-large a lower depth for evaluation of D
   * preliminary support for diagnostic horizons via diagnostic_hz()
   * new examples
   * error and warnings messages cleaned-up

# aqp 0.99-9.52 (2012-01-31)
   * converting all messages passed to the user from cat() to message() and warning()
   * profile_compare() and slab() now default to _no_ progress bars, see 'progress' argument to adjust
   * profileApply() now provides access to all aspects of the SoilProfileCollection object
      + that means that:
    profileApply(SPC, function(x) ... ) 
    operates on x, which is a soilProfileCollection containing only profile_i *note that this may break functionality based on aqp 0.99-9.3*

# aqp 0.99-9.4 (2012-01-09)
   * `site(SCP) <- d` now tries to merge data from SPC@site with 'd' via left join
      + this means that all new SPC objects will have a single column of @site data, containing profile IDs
   * plot-SoilProfileCollection now tries to guess the best orientation of IDs
      + override with id.style='top' or id.style='side'
   * slice-SoilProfileCollection now accepts '.' to define all columns should be returned at requested slices
      + slice(SPC, 1:50 ~ . )   # this means all vars. ID, top, bottom are automatically removed

# aqp 0.99-9.1 (2011-12-27)
   * pedonPC, NASIS, and SDA functions moved *out* of aqp, and *into* new package: soilDB
      + removed associated, suggested packages from aqp
      + soilDB will be on CRAN shortly
   * new argument to plot(SoilProfileCollection, ..., divide.hz=TRUE|FALSE) that can optionally suppress plotting of dividing lines between horizons (suggested by Ludwig Hilger)
   * new function profileApply() for applying functions by profile

# aqp 0.99-9 (2011-12-22)
   * almost ready for AQP 1.0, vignettes are the last missing piece :)
   * slice(SPC, ...) is now 10-100x faster, scales linearly
   * slice(SPC, ...) can simultaneously slice categorical and continuous variables
   * converted ID column in sample data to character class
   * fixed a bug in slice() where NA would be returned when IDs were factors
   * added test to depths()<- such that factor IDs are converted into character IDs and a warning is issued
   * added join conditions for most functions using plyr::join(), resulting in less chatter on the console 

# aqp 0.99-8.58 (2011-12-21)
   * WARNING: soil.slot.multiple() renamed to slab()
   * fixed minor bug in slice(), when numerical IDs are used: ddply() and cast() implicitly re-order data...
   * added `strict` argument to many functions, to gracefully account for bad horizon data
      + enforce quality control with strict=TRUE
   * munsell2rgb() is now 100x faster thanks to plyr::join()
      + note that factors cannot be used in the conversion, see: https://github.com/hadley/plyr/issues/43
    + there is a check and conversion within the function that should prevent segfaults
   * documentation updates, demo(aqp) updates, more examples
   * bug fix in slice(), now works with numeric OR categorical vars, but NOT both...
   * matrix-style subsetting functional:
      + s[i, j] ---> i is the profile index, and j is the horizon/slice index
      + s[i, ] returns a SoilProfileCollection
      + s[, j] returns a SpatialPointsDataFrame when a single slice is used
   * spatial_subset() can be used to extract members of a SoilProfileCollection with a geometry (uses rgeos)
   * smarter $<- methods so that site/horizon data slots are auto-selected

# aqp 0.99-8.50 (2011-11-29)
   * added "site(SoilProfileCollection) <-" method for data.frame
   * started VERY basic AQP Introduction vignette... still experimental
   * adding coercion methods for common objects and sp-class objects
   * matrix-style subsetting almost works:
      + s[i, j] ---> i is the profile index, and j is the horizon/slice index
   * added a 'slice' method for SoilProfileCollection objects
      + returns selected variables along depth slice(s) (i.e. no aggregation)
   * added NAMESPACE file for R >= 2.14 compatibility
   * stabilized S4 class, working on S4-izing all related functions
   * preliminary documentation on the new S4 classes/methods
   * logistic power peak (LPP) can now be used to generate synthetic depth functions
   * added contributing fraction annotation in panel.depth_function()

# aqp 0.99-8.4 (2011-10-04)
   * minor bug fixes and enhancements in PedonPC function
   * S4 interface to soil.slot.multiple()

# aqp 0.99-8.2 (2011-09-21)
   * added S4 class/methods for `SoilProfileCollection`
      + this supersedes the (now removed) S3 `SoilProfileList` classes
      + basic accessors/setters are in place, subject to change
   * profile_plot() now uses the `SoilProfileCollection` class
      + *initSoilProfileList() is no longer supported; see depths() for similar functionality*
      + *expect some tumultuous times ahead in the API... should be ironed out by 1.0 release*

# aqp 0.99-8 (2011-09-14)
   * soil.slot() will now accept boundaries defining a 'slab' over which aggregates are computed
   * soil.slot.multiple() now cleanly wraps soil.slot(), accepting all arguments
      + these two changes make it possible to ask:  "what is the wt. mean value of some property within this slab, and among these groups?"
      + soil.slot.multiple() now uses a formula interface: NOTE that this will break existing scripts (sorry)
    
# aqp 0.99-7 (2011-09-01)
   * new functions for getting data out of PedonPC (MS Access) databases [windows only for now]
      + get_site_data_from_pedon_db() : site and pedon aggregate data
      + get_hz_data_from_pedon_db() : horizon level data
      + get_colors_from_pedon_db() : formats and mixes multiple colors / horizon
  + implemented in mix_and_clean_colors()

   * test_hz_logic() : basic function for testing horizon logic, returns TRUE/FALSE by ID
   * parallel operations now NON-functional, while we wait for plyr to support doSMP...
   * new ID plotting style for profile_plot() : handy when plotting large collections and/or long IDs

# aqp 0.99-4 (2011-08-15)
   * code and documentation clean-up
   * Soil Sata Access (SDA) query functions have been added
      + `mapunit_geom_by_ll_bbox()` : get map unit geometry by bounding box
      + `MUKEYS_by_ll_bbox()` : get map unit keys by bounding box
      + `SDA_query()` : retrieve soil tabular data via query written in SQL
   * additional customization added to `plotSPC`
   * two new sample data sets + examples

# aqp 0.99-1 (2011-01-26)
   * soil.slot() and profile_compare() now have support for parallel computation thanks to bug-fixes in plyr 1.4
      + plyr >= 1.4, foreach, and doMC packages are required
      + specific examples are not yet documented, but should be soon
      + this feature is still experimental! testing is advised
   * `profile_compare()` now calculates slice-wise dissimilarity matrices in 1/3 the time (thanks llply!)

# aqp 0.98-4 (2010-12-15)
   * fixed minor bug in soil.slot() when computing probabilities from profiles that had missing horizons and that had only a single class within the variable to be aggregated

# aqp 0.98-1 (2010-11-23)
   * added basic demo: demo(aqp)
   * soil.slot() now computes aggregate probabilities over user-defined segments
   * re-write of weighted profile aggregation functions
      + aqp package now requires Hmisc package
      + using Hmisc::wtd.{mean,var,quantile} to compute values
   * better adjustment of weights when computing weighted SD
   * fixes long-standing bugs with wt. mean/SD when NA present in x_i and not in wt_i
   * weighted quantiles now computed
      + addition of new (experimental) S4 classes and methods
      + these are now in the aqp_S4 branch
   * added n.depth.ticks option to profile_plot()
   * profile_compare() algorithm stabilized, new default settings
   * removed code in profile_compare() that is now obsolete
      + note that this will cause changes to numerical classification of soil profiles

# aqp 0.97-1 (2010-10-06)
   * verified that weighted standard deviations are correct when seg_size > 1
   * removed notices about possible problems with sd calculations

# aqp 0.97 (2010-09-22)
   * soil.slot() streamlined and functionality restored to pre-0.95 condition

# aqp 0.96-1 (2010-09-21)
   * fixed weighted mean / SD calculation when using 1 unit segments in soil.slot()
      + wt. mean / SD still disabled for user-defined segment sizes

# aqp 0.96 (2010-09-20)
   * temporarily disabled parallel computation, will be re-added in next release
   * bug fixes in panel.depth_function where 'groups' was not defined
   * bug fixes and major re-factoring of soil.slot() when called with user-defined segment size, or segmenting vector:
      + SD values are probably too low due to an inflated 'n' in the calculation
      + calculation of weighted mean and sd is currently disabled

# aqp 0.95 (2010-09-15)
   * added VERY experimental support for parallel processing based on the latest versions of plyr, doMC, and foreach packages
   * updated vignette with technical details on aggregation and dissimilarity computation
   * new options to profile_plot()

# aqp 0.94 (2010-08-11)
   * fixed bug in soil.slot() when aggregating a categorical variable, with a user-supplied segmenting vector
   * soil.slot() now requires that categorical variables be encoded as factors

# aqp 0.93 (2010-08-07)
   * bug fixes, new documentation + examples
   * AQP now requires R >= 2.9.0

# aqp 0.92 (2010-08-03)
   * including a new column in the results from soil.slot(), denoting number of profiles used in slice-wise aggregation
   * added note to soil.slot() manual page highlighting possible problems with SD calculation

# aqp 0.90 (2010-07-13)
   * better error checking on profile aggregation and classification functions
   * most functions now require that horizon depths are integers

# aqp 0.89 (2010-07-10)
   * added plot_distance_graph() function for visualizing between-profile dissimilarity
   * updated documentation: extended manual is now accessible as a vignette
   * major improvements in profile_compare() when add_soil_flag=TRUE and replace_na=TRUE
      + appears to create much more realistic groupings when there are both shallow and deep soils in the collection
      + new functionality requires further testing
   * plotting under dendrograms generated by ape::plot.phylo() may need some manual adjustments

# aqp 0.88 (2010-07-06)
   * new version of plyr (1.0) should speed up most functions in aqp package 
   * added support for user-defined aggregate functions in soil.slot and soil.slot.multiple
   * planning addition of PCA by depth slice
   * planning addition of equal-area spline fitting
