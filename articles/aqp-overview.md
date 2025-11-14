# An Overview of the aqp Package

Soil morphology, observed properties, and geomorphic context represent a
complex package of interrelated information that can be difficult to
analyze and communicate as a whole. Graphical methods such as soil
profile sketches and cross-sections represent a few of the possible
methods commonly used to report on these kind of data. The Algorithms
for Quantitative Pedology (AQP) project encompasses several related R
packages tailored to this style of work. A specialized data structure
(SoilProfileCollection) maintains linkages between soil horizons,
diagnostic features, above-ground data, and geomorphic context.
SoilProfileCollection objects can be filtered, subset, resampled (over
new depth intervals), and re-ordered; all while preserving links to
above and below-ground, linked data. Functions are provided for the
conversion of soil colors to and from Munsell notation and several other
color space coordinates. Graphical methods for the SoilProfileCollection
provide a simple but flexible framework for the design and layout of
soil profile sketches, aligned to x and or y axes defined by linked
data.

## Related Tutorials by Topic

### Soil Morphology

- [Soil Profile
  Sketches](https://ncss-tech.github.io/AQP/aqp/sketches.html)
- [Visualization of Horizon
  Boundaries](https://ncss-tech.github.io/AQP/aqp/hz-boundaries.html)
- [Assigning Generalized Horizon
  Labels](https://ncss-tech.github.io/AQP/aqp/gen-hz-assignment.html)
- [Dealing With Troublesome
  Data](https://ncss-tech.github.io/AQP/aqp/dealing-with-bad-data.html)
- [Simulation of Soil Morphology by
  Perturbation](https://ncss-tech.github.io/AQP/aqp/perturb.html)
- [Soil Texture Visualization
  Ideas](https://ncss-tech.github.io/AQP/aqp/soiltexture-vizualization-ideas.html)
- [SPC Sketch
  Tips](https://ncss-tech.github.io/AQP/aqp/SPC-plotting-ideas.html)
- [Vertical vs. Perpendicular Horizon
  Depths](https://ncss-tech.github.io/AQP/aqp/vertical_vs_perpendicular.html)

### Soil Color

- [Mixing Munsell
  Colors](https://ncss-tech.github.io/AQP/aqp/mix-colors.html)
- [Soil Color
  Contrast](https://ncss-tech.github.io/AQP/aqp/color-contrast.html)
- [Some ideas on summarizing soil
  color](https://ncss-tech.github.io/AQP/sharpshootR/aggregate-soil-color.html)
- [Soil Color
  Tiles](https://ncss-tech.github.io/AQP/aqp/color-tiles.html)
- [Investigating Soil
  Color](https://ncss-tech.github.io/AQP/aqp/investigating-soil-color.html)
- [Some Ideas on Soil Color
  Signatures](https://ncss-tech.github.io/AQP/aqp/soil-color-signatures.html)
- [Soil Color Transition
  Probability](https://ncss-tech.github.io/AQP/aqp/series-color-TP-graph.html)

### Numerical Classification

- [Pair-Wise Distances by Generalized Horizon
  Labels](https://ncss-tech.github.io/AQP/aqp/genhz-distance-eval.html)
- [Pair-Wise Distance Between Profiles and Mixed Variable
  Types](https://ncss-tech.github.io/AQP/aqp/profile-compare-factors.html)

### Soil Hydrology

- [Theoretical Water Retention
  Curves](https://ncss-tech.github.io/AQP/aqp/water-retention-curves.html)
- [Water Retention Curve Development from KSSL
  Data](https://ncss-tech.github.io/AQP/soilDB/fetchKSSL-VG-demo.html)

### Geomorphology

- [Hydrologic Ordering of Geomorphic
  Proportions](https://ncss-tech.github.io/AQP/sharpshootR/geomorphic-summaries-and-ordering.html)
- [Exploring Geomorphic
  Summaries](https://ncss-tech.github.io/AQP/soilDB/exploring-geomorph-summary.html)

### Soil Taxonomy

- [What does a subgroup look
  like?](https://ncss-tech.github.io/AQP/soilDB/subgroup-series.html)
- [Querying Soil Series
  Data](https://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html)

### Multivariate Summaries of Soil Property Data

- [L1 Profiles](https://ncss-tech.github.io/AQP/aqp/L1-profiles.html)
- [Getting and Comparing KSSL
  Data](https://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)

### Soil Spectroscopy

- [Soil Colors from VNIR
  Spectra](https://ncss-tech.github.io/AQP/aqp/VNIR-spectra-to-color.html)

## Function Index by Topic

### SoilProfileCollection Objects

#### Creation

- [`depths()`](https://ncss-tech.github.io/aqp/reference/depths.md):
  init an SPC from `data.frame`
- [`site()`](https://ncss-tech.github.io/aqp/reference/site.md): set or
  add site-level attributes of an SPC  
- [`quickSPC()`](https://ncss-tech.github.io/aqp/reference/quickSPC.md):
  quickly build an SPC from simple text templates
- [`random_profile()`](https://ncss-tech.github.io/aqp/reference/random_profile.md):
  generate random SPC from suite of depth functions

#### Metadata

- [`hzdesgnname()`](https://ncss-tech.github.io/aqp/reference/hzdesgnname.md):
  get/set column containing horizon designations
- [`hzDesgn()`](https://ncss-tech.github.io/aqp/reference/hzDesgn.md):
  get vector of horizon designations
- [`hztexclname()`](https://ncss-tech.github.io/aqp/reference/hztexclname.md):
  get/set column containing horizon texture class
- [`metadata()`](https://ncss-tech.github.io/aqp/reference/metadata.md):
  get/set SPC metadata (list)
- [`hzID()`](https://ncss-tech.github.io/aqp/reference/hzID.md): get
  vector of horizon IDs
- [`hzidname()`](https://ncss-tech.github.io/aqp/reference/hzidname.md):
  get/set column containing horizon IDs
- [`horizonDepths()`](https://ncss-tech.github.io/aqp/reference/horizonDepths.md)
  get/set columns containing horizon top and bottom depths

#### Properties

- [`length()`](https://ncss-tech.github.io/aqp/reference/length.md):
  number of profiles in a SPC
- [`nrow()`](https://ncss-tech.github.io/aqp/reference/nrow.md): number
  of horizons in a SPC
- [`names()`](https://ncss-tech.github.io/aqp/reference/names.md): list
  of horizon and site names
- [`siteNames()`](https://ncss-tech.github.io/aqp/reference/siteNames.md):
  site-level column names
- [`horizonNames()`](https://ncss-tech.github.io/aqp/reference/horizonNames.md):
  horizon-level column names

#### Subset

- [`glom()`](https://ncss-tech.github.io/aqp/reference/glom.md): extract
  horizons based on overlap criteria defined by point or interval
- [`trunc()`](https://rdrr.io/r/base/Round.html): truncate SPC to given
  depth interval
- [`subset()`](https://ncss-tech.github.io/aqp/reference/subset-SoilProfileCollection-method.md):
  subset profiles based on logical expressions
- [`subsetHz()`](https://ncss-tech.github.io/aqp/reference/subsetHz-SoilProfileCollection-method.md):
  subset horizons based on logical expressions
- `[`: `data.frame`-like subsetting of profiles (i-index) and/or
  horizons (j-index)
- `[[`: access site or horizon-level columns by name
- `k-index expressions`: `.FIRST`, `.LAST`, `.HZID`, `.NHZ`

#### Depth

- [`min()`](https://ncss-tech.github.io/aqp/reference/min.md): minimum
  bottom depth within a SPC
- [`max()`](https://ncss-tech.github.io/aqp/reference/max.md): maximum
  bottom depth within a SPC
- [`depthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md):
  generalized “depth to” based on REGEX matching
- [`minDepthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md):
  special case of
  [`depthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md)
- [`maxDepthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md):
  special case of
  [`depthOf()`](https://ncss-tech.github.io/aqp/reference/depthOf.md)
- [`getSoilDepthClass()`](https://ncss-tech.github.io/aqp/reference/getSoilDepthClass.md):
  estimate soil depth based on REGEX matching applied to horizon
  designation and associated depth class
- [`aggregateSoilDepth()`](https://ncss-tech.github.io/aqp/reference/aggregateSoilDepth.md):
  statistical estimation of soil depth (REGEX matching of horizon
  designation) within groups of profiles

#### Utility

- [`combine()`](https://ncss-tech.github.io/aqp/reference/combine-SoilProfileCollection-method.md),
  [`c()`](https://ncss-tech.github.io/aqp/reference/combine-SoilProfileCollection-method.md):
  combine multiple SPCs into a single SPC
- [`duplicate()`](https://ncss-tech.github.io/aqp/reference/duplicate.md):
  duplicate profiles within a SPC
- [`perturb()`](https://ncss-tech.github.io/aqp/reference/perturb.md):
  randomly adjust horizon thickness or depths to simulate from a
  template SPC
- [`warpHorizons()`](https://ncss-tech.github.io/aqp/reference/warpHorizons.md):
  expand / contract horizon thickness
- [`harmonize()`](https://ncss-tech.github.io/aqp/reference/harmonize-SoilProfileCollection-method.md):
  create new profiles within a SPC based sets of related horizon-level
  data
- [`hzAbove()`](https://ncss-tech.github.io/aqp/reference/hzOffset.md),
  [`hzBelow()`](https://ncss-tech.github.io/aqp/reference/hzOffset.md):
  locate horizons above or below some criteria
- [`unique()`](https://ncss-tech.github.io/aqp/reference/unique.md):
  determine uniqueness among profiles of an SPC via MD5 hash
- [`split()`](https://rdrr.io/r/base/split.html): split SPC into list of
  SPCs based on grouping factor
- [`site()`](https://ncss-tech.github.io/aqp/reference/site.md): get
  site data as `data.frame`
- [`horizons()`](https://ncss-tech.github.io/aqp/reference/horizons.md):
  get horizon data as `data.frame`
- `replaceHorizons()`: replace horizon data
- [`diagnostic_hz()`](https://ncss-tech.github.io/aqp/reference/diagnostic_hz.md):
  get/set diagnostic features
- [`restrictions()`](https://ncss-tech.github.io/aqp/reference/restrictions.md):
  get/set restrictions
- [`denormalize()`](https://ncss-tech.github.io/aqp/reference/denormalize.md):
  convert site-level data into horizon-level data via replication
- [`compositeSPC()`](https://ncss-tech.github.io/aqp/reference/compositeSPC.md):
  downgrade an SPC to list of site and horizon-level data

#### Iteration

- [`profileApply()`](https://ncss-tech.github.io/aqp/reference/profileApply.md):
  apply a function to each profile within an SPC (slow but simple
  interface)
- [`summarizeSPC()`](https://ncss-tech.github.io/aqp/reference/summarizeSPC.md):
  perform group-wise summaries over profiles within an SPC
- [`transform()`](https://rdrr.io/r/base/transform.html): modify a SPC
  using expressions that operation on site or horizon-level data

#### Change of Support

- [`dice()`](https://ncss-tech.github.io/aqp/reference/dice-SoilProfileCollection-method.md):
  convert SPC to 1 depth-unit intervals by replication
- [`slab()`](https://ncss-tech.github.io/aqp/reference/slab.md): apply
  an aggregate function over groups within a “dice()-ed” SPC
- [`spc2mpspline()`](https://ncss-tech.github.io/aqp/reference/spc2mpspline-SoilProfileCollection-method.md):
  interface to equal-area spline fitting from mpspline2 package
- [`segment()`](https://ncss-tech.github.io/aqp/reference/hz_segment.md):
  generate segment labels for depth-weighted aggregation
- [`L1_profiles()`](https://ncss-tech.github.io/aqp/reference/L1_profiles.md):
  create representative profiles via multivariate median (L1 estimator)
- [`slicedHSD()`](https://ncss-tech.github.io/aqp/reference/slicedHSD.md):
  apply Tukey’s HSD over groups within a “dice()-ed” SPC

#### Horizon Depth Logic

- [`accumulateDepths()`](https://ncss-tech.github.io/aqp/reference/accumulateDepths.md):
  fix horizon depths when old-style O horizon notation has been used
- [`fillHzGaps()`](https://ncss-tech.github.io/aqp/reference/fillHzGaps.md):
  fill topological gaps in horizon depth
- [`repairMissingHzDepths()`](https://ncss-tech.github.io/aqp/reference/repairMissingHzDepths.md):
  attempt fixing missing or duplicated horizon bottom depths
- [`flagOverlappingHz()`](https://ncss-tech.github.io/aqp/reference/flagOverlappingHz.md):
  flag horizons with perfect overlap
- [`checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.md):
  apply battery of horizon depth topological tests
- [`splitLogicErrors()`](https://ncss-tech.github.io/aqp/reference/splitLogicErrors.md):
  split an SPC according to variety of possibly horizon depth errors
- [`HzDepthLogicSubset()`](https://ncss-tech.github.io/aqp/reference/HzDepthLogicSubset.md):
  remove profiles from an SPC if any depth logic errors are present

#### Data QC

- [`evalMissingData()`](https://ncss-tech.github.io/aqp/reference/evalMissingData.md):
  report metrics of missing data by profile within SPC
- [`missingDataGrid()`](https://ncss-tech.github.io/aqp/reference/missingDataGrid.md):
  visual indication of missing data
- [`profileInformationIndex()`](https://ncss-tech.github.io/aqp/reference/profileInformationIndex.md):
  experimental indices of “information content” by profile

#### Object Coercion

- `as(SPC, 'list')`: convert SPC to `list`
- `as(SPC, 'data.frame')`: convert site and horizon data to `data.frame`
- `as(SPC, 'sf')`: convert site and spatial data to sf object

#### Spatial Data

- [`prj()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-crs.md):
  get/set coordinate reference system (CRS) metadata
- `initSpatial()`: set (site-level) column names containing coordinates
- [`getSpatial()`](https://ncss-tech.github.io/aqp/reference/initSpatial.md):
  get spatial data (site + coordinates) from an SPC

#### Internal Consistency

- [`checkSPC()`](https://ncss-tech.github.io/aqp/reference/checkSPC.md):
  check SPC for internal consistency
- [`rebuildSPC()`](https://ncss-tech.github.io/aqp/reference/rebuildSPC.md):
  re-make an SPC from a previous version of aqp (rarely required)

### Soil Profile Sketches

- [`plotSPC()`](https://ncss-tech.github.io/aqp/reference/SoilProfileCollection-plotting-methods.md):
  create sketches of soil profiles from an SPC
- [`explainPlotSPC()`](https://ncss-tech.github.io/aqp/reference/explainPlotSPC.md):
  explain various elements of a soil profile sketch
- [`groupedProfilePlot()`](https://ncss-tech.github.io/aqp/reference/groupedProfilePlot.md):
  create grouped sketches of soil profiles
- [`plotMultipleSPC()`](https://ncss-tech.github.io/aqp/reference/plotMultipleSPC.md):
  create sketches from multiple, possibly non-conformal SPCs
- [`addBracket()`](https://ncss-tech.github.io/aqp/reference/addBracket.md):
  add vertical brackets beside soil profile sketches
- [`addDiagnosticBracket()`](https://ncss-tech.github.io/aqp/reference/addDiagnosticBracket.md):
  add vertical brackets beside soil profile sketches based on diagnostic
  features
- [`addVolumeFraction()`](https://ncss-tech.github.io/aqp/reference/addVolumeFraction.md):
  add visual explanation of coarse fragment volume to soil profile
  sketches
- [`alignTransect()`](https://ncss-tech.github.io/aqp/reference/alignTransect.md):
  align soil profiles to an external gradient such as topographic
  sequence

### Soil Color / Color Science

#### Color Conversion

- [`col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.md):
  convert various color notations to Munsell notation
- [`munsell2rgb()`](https://ncss-tech.github.io/aqp/reference/munsell2rgb.md):
  convert Munsell notation to sRGB or CIELAB color coordinates
- [`parseMunsell()`](https://ncss-tech.github.io/aqp/reference/parseMunsell.md):
  parse and optionally convert a munsell color
- [`spec2Munsell()`](https://ncss-tech.github.io/aqp/reference/spec2Munsell.md):
  estimate the closest Munsell color given reflectance spectra in the
  visible range
- [`getClosestMunsellChip()`](https://ncss-tech.github.io/aqp/reference/getClosestMunsellChip.md):
  estimate a reasonably close Munsell color given non-standard notation
- [`estimateSoilColor()`](https://ncss-tech.github.io/aqp/reference/estimateSoilColor.md):
  estimate moist soil color from dry soil color (and vice versa)

#### Comparison

- [`colorContrast()`](https://ncss-tech.github.io/aqp/reference/colorContrast.md):
  pair-wise color contrast and CIE2000 (dE00) based on colors in Munsell
  notation
- [`colorContrastPlot()`](https://ncss-tech.github.io/aqp/reference/colorContrastPlot.md):
  visual explanation of soil color contrast and dE00
- [`contrastChart()`](https://ncss-tech.github.io/aqp/reference/contrastChart.md):
  Munsell color book style explanation of soil color contrast and dE00
- [`soilColorSignature()`](https://ncss-tech.github.io/aqp/reference/soilColorSignature.md):
  derive soil color signatures for profiles within an SPC

#### Aggregation

- [`colorChart()`](https://ncss-tech.github.io/aqp/reference/colorChart.md):
  Munsell color book representation of color frequency
- [`aggregateColor()`](https://ncss-tech.github.io/aqp/reference/aggregateColor.md):
  estimate color proportions within an SPC according within groups of
  horizons
- [`colorQuantiles()`](https://ncss-tech.github.io/aqp/reference/colorQuantiles.md):
  marginal and L1 quantiles of color in CIELAB coordinates

#### Utility

- [`huePosition()`](https://ncss-tech.github.io/aqp/reference/huePosition.md):
  generate an ordered factor of the standard Munsell hues
- [`huePositionCircle()`](https://ncss-tech.github.io/aqp/reference/huePositionCircle.md):
  graphical representation of the standard Munsell hues, with optional
  simulation of common color vision deficiency
- [`simulateColor()`](https://ncss-tech.github.io/aqp/reference/simulateColor.md):
  simulate a range of Munsell colors given measures of central tendency
  and spread
- [`previewColors()`](https://ncss-tech.github.io/aqp/reference/previewColors.md):
  graphical preview of colors as a grid or via nMDS
- [`soilPalette()`](https://ncss-tech.github.io/aqp/reference/soilPalette.md):
  generate swatch-like arrangements of colors and labels
- [`equivalentMunsellChips()`](https://ncss-tech.github.io/aqp/reference/equivalentMunsellChips.md):
  for a specified Munsell color, identify other Munsell colors with a
  very lower CIE2000 color contrast difference

#### Simulation of Mixtures

- [`mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.md):
  simulate an subtractive mixture of pigments specified in Munsell
  notation
- [`plotColorMixture()`](https://ncss-tech.github.io/aqp/reference/plotColorMixture.md):
  simulate a subtractive mixture of pigments, display reflectance
  spectra

### Numerical Classification of Soil Profiles

- [`NCSP()`](https://ncss-tech.github.io/aqp/reference/NCSP.md):
  numerical classification of soil profiles, within a
  `SoilProfileCollection`
- [`compareSites()`](https://ncss-tech.github.io/aqp/reference/compareSites.md):
  pair-wise comparison of site-level data from a `SoilProfileCollection`

### Pedology

- [`allocate()`](https://ncss-tech.github.io/aqp/reference/allocate.md):
  perform one of several classification systems to soil property data
- [`estimateAWC()`](https://ncss-tech.github.io/aqp/reference/estimateAWC.md):
  estimate plant-available water holding capacity
- [`correctAWC()`](https://ncss-tech.github.io/aqp/reference/correctAWC.md):
  apply rock fragment or soluble salt corrections to AWC estimates
- [`hzDistinctnessCodeToOffset()`](https://ncss-tech.github.io/aqp/reference/hzDistinctnessCodeToOffset.md):
  convert horizon boundary distinctness codes to vertical offset
- [`hzTopographyCodeToLineType()`](https://ncss-tech.github.io/aqp/reference/hzTopographyCodeToLineType.md):
  convert horizon boundary topography codes to line types
- [`hzTopographyCodeToOffset()`](https://ncss-tech.github.io/aqp/reference/hzTopographyCodeToOffset.md):
  convert horizon boundary topography codes to offset
- [`ph_to_rxnclass()`](https://ncss-tech.github.io/aqp/reference/reaction.md):
  convert pH to reaction class
- [`rxnclass_to_ph()`](https://ncss-tech.github.io/aqp/reference/reaction.md):
  convert reaction class to pH range
- [`ReactionClassLevels()`](https://ncss-tech.github.io/aqp/reference/reaction.md):
  ordered factor of reaction classes
- [`horizonColorIndices()`](https://ncss-tech.github.io/aqp/reference/horizonColorIndices.md):
  compute various soil color-based indices to horizons within an SPC

#### Soil Texture

- [`textureTriangleSummary()`](https://ncss-tech.github.io/aqp/reference/textureTriangleSummary.md):
  graphical summary of sand, silt, clay fractions on a soil texture
  triangle  
- [`bootstrapSoilTexture()`](https://ncss-tech.github.io/aqp/reference/bootstrapSoilTexture.md):
  simulation of realistic compositions (sand, silt, clay) from a small
  set of example data
- [`SoilTextureLevels()`](https://ncss-tech.github.io/aqp/reference/SoilTextureLevels.md):
  ordered factor of soil texture classes
- [`texcl_to_ssc()`](https://ncss-tech.github.io/aqp/reference/texture.md):
  convert soil texture classes to sand, silt, clay centroids
- [`ssc_to_texcl()`](https://ncss-tech.github.io/aqp/reference/texture.md):
  convert sand, silt, clay values to soil texture class
- [`texture_to_taxpartsize()`](https://ncss-tech.github.io/aqp/reference/texture.md):
  convert soil texture to Soil Taxonomy particle size class

#### Coarse Fragments

- [`fragmentSieve()`](https://ncss-tech.github.io/aqp/reference/fragmentSieve.md):
  classify coarse fragments by fragment diameter
- [`texmod_to_fragvoltot()`](https://ncss-tech.github.io/aqp/reference/texture.md):
  estimate ranges in coarse fragment volume based on a soil texture
  modifier
- [`texture_to_texmod()`](https://ncss-tech.github.io/aqp/reference/texture.md)
- [`fragvol_to_texmod()`](https://ncss-tech.github.io/aqp/reference/texture.md)
- [`fragmentClasses()`](https://ncss-tech.github.io/aqp/reference/fragmentClasses.md):
  coarse fragment diameter thresholds used by USDA-NRCS

#### Soil Taxonomy

- [`getArgillicBounds()`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.md):
  estimate the upper and lower boundaries of an argillic horizon
- [`getCambicBounds()`](https://ncss-tech.github.io/aqp/reference/getCambicBounds.md):
  estimate the upper and lower boundaries of a cambic horizon
- [`getSurfaceHorizonDepth()`](https://ncss-tech.github.io/aqp/reference/getSurfaceHorizonDepth.md)
- [`getMineralSoilSurfaceDepth()`](https://ncss-tech.github.io/aqp/reference/getSurfaceHorizonDepth.md)
- [`getPlowLayerDepth()`](https://ncss-tech.github.io/aqp/reference/getSurfaceHorizonDepth.md)
- [`hasDarkColors()`](https://ncss-tech.github.io/aqp/reference/hasDarkColors.md)
- [`estimatePSCS()`](https://ncss-tech.github.io/aqp/reference/estimatePSCS.md)

#### Generalized Horizon Labels (GHL)

- [`generalize.hz()`](https://ncss-tech.github.io/aqp/reference/generalize.hz.md):
  apply REGEX rules to group horizon designations into a reduced set of
  “generalized horizon labels”
- [`evalGenHZ()`](https://ncss-tech.github.io/aqp/reference/evalGenHZ.md):
  evaluate internal consistency of assigned GHL
- [`genhzTableToAdjMat()`](https://ncss-tech.github.io/aqp/reference/genhzTableToAdjMat.md):
  convert a cross-tabulation of GHL vs. original horizon designations to
  adjacency matrix
- [`get.ml.hz()`](https://ncss-tech.github.io/aqp/reference/get.ml.hz.md):
  extract most likely horizon boundary depths from probability depth
  functions
- [`guessGenHzLevels()`](https://ncss-tech.github.io/aqp/reference/guessGenHzLevels.md):
  estimate the correct ordering of GHL given horizon depths
- [`GHL()`](https://ncss-tech.github.io/aqp/reference/GHL.md): get/set
  GHL metadata for a `SoilProfileCollection`

### Misc.

- [`invertLabelColor()`](https://ncss-tech.github.io/aqp/reference/invertLabelColor.md):
  automatic adjustment of label color for maximum contrast, based on
  specified background color
- [`hzTransitionProbabilities()`](https://ncss-tech.github.io/aqp/reference/hzTransitionProbabilities.md):
  derive transition probability matrix from horizon level data
- [`mostLikelyHzSequence()`](https://ncss-tech.github.io/aqp/reference/hzTransitionProbabilities.md):
  use Markov Chains to predict the most likely sequence of horizons

#### Accuracy and Uncertainty

- [`shannonEntropy()`](https://ncss-tech.github.io/aqp/reference/shannonEntropy.md):
  Shannon entropy
- [`brierScore()`](https://ncss-tech.github.io/aqp/reference/brierScore.md):
  Brier’s score
- [`tauW()`](https://ncss-tech.github.io/aqp/reference/tauW.md):
  weighted tau statistic

#### Overlapping Annotation

- [`findOverlap()`](https://ncss-tech.github.io/aqp/reference/overlapMetrics.md):
  identify overlap within a vector of positions based on a given
  threshold
- [`overlapMetrics()`](https://ncss-tech.github.io/aqp/reference/overlapMetrics.md):
  metrics of overlap within a vector or positions based on a given
  threshold
- [`fixOverlap()`](https://ncss-tech.github.io/aqp/reference/fixOverlap.md):
  attempt the minimum of adjustments to vector of positions such that a
  given distance threshold is enforced

### Example Data

- `sp1`
- `sp2`
- `sp3`
- `sp4`
- `sp5`
- `sp6`
- `sierraTransect`
- `wilson2022`
- `rowley2019`
- `jacobs2000`
- `osd`
- `SPC.with.overlap`
- `us.state.soils`
- `soil_minerals`
- `munsell`
- `equivalent_munsell`
- `munsellHuePosition`
- `munsell.spectra`
- `spectral.reference`
- `ROSETTA.centroids`
- `reactionclass`
- `soiltexture`
