test_that("metadata and attributes are persistent", {

  data("sp3")
  
  depths(sp3) <- id ~ top + bottom
  
  depth_units(sp3) <- "fathoms"
  metadata(sp3)$something_custom <- "all your profiles are belong to us"
  attr(sp3, 'attribution') <- "the most attributable of all attributes"
  
  # thickness standard deviation for perturb()
  sp3$thickness_attr <- rep(1, nrow(sp3))
  
  cols <- c(
    "aqp_df_class",
    "aqp_group_by",
    "aqp_hzdesgn",
    "aqp_hztexcl",
    "depth_units",
    "stringsAsFactors",
    "something_custom"
  )
  
  .check_metadata_and_attribution <- function(obj, src, check_attr = TRUE) {
    # check metadata() other than original.order
    expect_equal(metadata(src)[cols], metadata(obj)[cols])
    # check attribute persistence via .transfer.attributes.aqp
    if (check_attr) expect_equal(attr(src, 'attribution'),  attr(obj, 'attribution'))
  }
  
  # compare when subsetting
  obj <- sp3[1,]
  .check_metadata_and_attribution(obj, sp3)
  
  # compare when combined
  obj <- combine(sp3[1,], sp3[2,])
  # NOTE: do not expect attributes to be transferred after combination operation
  .check_metadata_and_attribution(obj, sp3, check_attr = FALSE)
  
  # compare when rebuilt
  obj <- rebuildSPC(sp3)
  .check_metadata_and_attribution(obj, sp3)
  
  # compare when L1_profile-d
  obj <- L1_profiles(sp3[1:3, ], id ~ clay + ph + tc + cec, method = "simple")
  .check_metadata_and_attribution(obj, sp3)
  
  # compare when perturbed
  obj <- perturb(sp3[1,], thickness.attr = "thickness_attr")
  .check_metadata_and_attribution(obj, sp3)
})
