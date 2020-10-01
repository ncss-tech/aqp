
# Thought: how could a future version of `aqp::sim` better incorporate `aqp::generalize.hz`/related-workflows -- to facilitate calculation of layer-level variance parameters?
#
#   While one can measure the SD of a layer's thickness for a _single_ profile/trench -- that is pretty rarely done in most of our data. The only commonly-available way to derive the SD parameters for `sim` is to summarize _more than one_ profile (site) into common layers, and use the variance of those layers on a prototype profile (with as many layers as you have SD).
#
# For this reason, I think that `permute_profile` and `sim` occupy similar but different niches. They function pretty differently internally, and therefore are not that conducive to being merged. As discussed in the docs, `permute_profile` is somewhat "easier" to parameterize from a single pedon source (b/c of inferred logical connection to hz distinctness), but necessarily "narrower" scope in terms of the "population" being simulated.
#
# It would be very neat to be able to easily:
#  - `fetchOSD` [or select a prototype profile "component pedon" or similar]
#  - tag some generalized horizons
#  - "simplify" the OSD geometry
#  - pull in a "data" SPC containing field/lab data generalized to same labels as the OSD
#  - calculate layer-specific SD thickness.
#
# There are two main needs in {aqp} to do above workflow:
#
# 1) To be able to use a generalized prototype in `sim` easily, we need a method for profile _geometric simplification_ based on common (adjacent) horizon attributes -- this requires dropping/merging/aggregation of numeric and categorical (depth-weighted mean and mode?) horizon data. The method to assign the hz labels is irrelevant to this simplification -- they could be duplicated horizon IDs from joins/input data, horizon designations, or any other categorical label. This method could support other bugsquashing/QC for many:1 horizon relationships in {aqp} and {soilDB}.
#
# 2) A summarize genhz method could be implemented to facilitate calculating a matrix of generalized values per profile of which a thickness (by label) would be part of the standard panel. It would not rely on modification of profile geometry, slice/slab/glom or any similar routines. Simply provide an easy way to tabulate thicknesses (and related properties) of horizon-level groups by profile. I have helped several students implement these routines for Stats for Soil Survey projects so I know simplifying this particular process would be useful.
#

# psimplify <- function(p, ...) {
#
# }
