if (!isGeneric("spc2mpspline"))
  setGeneric("spc2mpspline", function(object,
                                      var_name = NULL,
                                      pattern = "R|Cr|Cd|qm",
                                      hzdesgn = guessHzDesgnName(object), ...)
    standardGeneric("spc2mpspline"))

#' @title Missing-data-safe, SPC-wide wrapper around mpspline2::mpspline "continuous" 1cm output
#'
#' @description Facilitate safe use of just about any numeric SPC horizon attribute, from any SPC, with \code{mpspline2::mpspline}. Currently only works with a single attribute.This function will automatically filter profiles with \code{NA} in attribute of interest which may be more conservative filtering than you expect. The intention here is that a SPC of related profile instances could be splined, and then the spline results aggregated over the full interval where data was available.
#'
#' Data completeness is assessed and the input SPC is filtered and truncated to create a container for the 1cm results from \code{mpspline2::mpspline}.
#'
#' @param object A SoilProfileCollection
#' @param var_name Column name in \code{@horizons} slot of \code{object} containing numeric values to spline
#' @param pattern Regex pattern to match for bottom of profile (passed to estimateSoilDepth) default: "R|Cr|Cd|qm"
#' @param hzdesgn Column name in \code{@horizons} slot of \code{object} containing horizon designations default: \code{aqp::guessHzDesgnName(object)}
#' @param ... Additional arguments to \code{mpspline2::mpspline}
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection with 1cm slices. Spline variables are in columns prefixed with "spline_" and RMSE/RMSE_IQR are in colums prefixed with "rmse_". If any profiles were removed from the collection, their profile IDs are stored in attr(result, 'removed').
#'
#' @export spc2mpspline,SoilProfileCollection-method
#' @aliases spc2mpspline
#'
#' @examples
#'
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#'
#' res <- spc2mpspline(sp1, "prop")
#'
#' plotSPC(res[1:5,], color = "prop_spline", divide.hz = FALSE)
#'
setMethod("spc2mpspline", signature(object = "SoilProfileCollection"),
          function(object, var_name = NULL,
                           pattern = "R|Cr|Cd|qm",
                           hzdesgn = guessHzDesgnName(object),
                           ...) {

  if (!requireNamespace('mpspline2'))
    stop("package `mpspline2` is required", call. = FALSE)

  if (is.null(var_name) | !(var_name %in% horizonNames(object)))
    stop("argument `var_name` must specify a single horizon-level variable", call. = FALSE)

  hztop <- horizonDepths(object)[1]
  hzbot <- horizonDepths(object)[2]

  # glom to "available interval" in each profile
  # NOTE: we will handle warnings (profiles with no data at all) at end
  spc.sub <- suppressWarnings(glomApply(object, function(p) {
    i <- which(diff(c(0, cumsum(!is.na(p[[var_name]])))) == 1)
    h <- horizons(p)
    # basically this excludes NA values at top and bottom of profile
    # (O horizons, bedrock) but wont check missing values inbetween
    # need at least two horizons to make a spline
    if (length(i) < 2)
      return(c(0, 0))
    top_depth <- h[[hztop]][i[1]]
    bot_depth <- h[[hzbot]][i[length(i)]]
    return(c(top_depth, bot_depth))
  }))

  # debug : inspect  horizon values for var_name
  #plot(spc.sub[1:10,], color=var_name)

  # only take profiles that have 100% data coverage in above interval
  #  i.e. if a single horizon is missing data, remove whole profile
  spc.sub$nona <- profileApply(spc.sub, function(p) any(is.na(p[[var_name]])))
  spc.sub <- spc.sub[which(!spc.sub$nona),]

  # calculate the deepest top depth and shallowest bottom depth
  mindepth <- max(profileApply(spc.sub, function(p) p[,1][[hztop]]))
  maxdepth <- min(profileApply(spc.sub, estimateSoilDepth, p = pattern, name = hzdesgn))

  # we will only make interpolations that the "whole SPC supports"
  # the thought is that these 1cm slices will be further aggregated downstream
  spc.sub <- glomApply(spc.sub, function(p) c(mindepth, maxdepth), truncate = TRUE)

  # do the splines
  res <- mpspline2::mpspline(horizons(spc.sub)[c(idname(spc.sub),
                                                 horizonDepths(spc.sub),
                                                 var_name)],
                             var_name = var_name, ...)

  # concatenate results for re-insertion
  res2 <- do.call('c', lapply(profile_id(spc.sub), function(pid) {
    drange <- mindepth:maxdepth
    zero.idx <- drange == 0
    if (any(zero.idx))
      drange <- drange[-which(zero.idx)]
    return(res[[pid]]$est_1cm[drange])
    # this returns the 1cm estimate which conforms with sliced spc
    #
    # debug: prove that mass is preserved in output by returning block estimates
    # return(res[[pid]]$est_icm)
  }))

  # get the RMSE
  reserr <- do.call('c', lapply(profile_id(spc.sub), function(pid) {
    return(res[[pid]]$est_err)
  }))

  # make 1:1 with site
  reserr_iqr <- reserr[names(reserr) == "RMSE_IQR"]
  reserr <- reserr[names(reserr) == "RMSE"]

  # inspect
  #reserr_iqr
  #reserr

  # single horizon results cannot be splined, filter those out
  spc.sub <- subApply(spc.sub, function(p) nrow(p) > 1)

  # adjustment for aqp::slice index logic versus glom interval logic
  if (mindepth == 0) {
    maxdepth <- maxdepth - 1
  }

  # create slices 1cm thick to insert spline result
  spc.spl <- aqp::slice(spc.sub, formula(sprintf("%s:%s ~ %s",
                                                 mindepth, maxdepth,
                                                 var_name)))

  # create new "spline_"+var_name variable
  spc.spl[[paste0(var_name,"_spline")]] <- res2

  # create new "rmse_"+var_name as site level attributes
  spc.spl[[paste0(var_name,"_rmse")]] <- reserr
  spc.spl[[paste0(var_name,"_rmse_iqr")]] <- reserr_iqr

  # determine what profiles were removed
  removed <- profile_id(object)[!profile_id(object) %in% profile_id(spc.spl)]

  # add an attribute with removed profile IDs. there are three steps
  # that possibly remove data:
  #  - profiles removed by glomApply have no var_name data at all.
  #  - 100% coverage filtering step -- conservative filter to keep from making splines from bad data
  #  - mpspline itself will remove profiles with e.g. just one horizon
  attr(spc.spl, "removed") <- unique(removed)

  return(spc.spl)
})
