# if (!isGeneric("spc2mpspline"))
  setGeneric("spc2mpspline", function(object,
                                      var_name = NULL,
                                      method = c("est_1cm", "est_icm", "est_dcm"),
                                      pattern = "R|Cr|Cd|qm",
                                      hzdesgn = NULL, ...)
    standardGeneric("spc2mpspline"))

#' @title SoilProfileCollection wrapper for `mpspline2::mpspline()` 
#'
#' @description Generate mass-preserving splines for any numeric attribute in a SoilProfileCollectuion using `mpspline2::mpspline()`. mpspline2 implements the method described in Bishop et al. (1999). Currently this function only works with a single `var_name` at a time. 
#' 
#' @details This function now relies on the missing data checks provided by the mpspline2 package. See `attr(..., 'removed')` to see whole profiles that were removed from the set. Horizons containing `NA` in the property of interest are dropped with a message.
#'
#' Data completeness is assessed and the input SoilProfileCollection is filtered and truncated to create a container for the results from `mpspline2::mpspline()`.
#'
#' @param object A SoilProfileCollection
#' @param var_name Column name in `@horizons` slot of `object` containing numeric values to spline
#' @param pattern Regex pattern to match for bottom of profile (passed to `minDepthOf()`) default: "R|Cr|Cd|qm"; only used if `hzdesgn` is specified
#' @param hzdesgn Column name in `@horizons` slot of `object` containing horizon designations default: `NULL`
#' @param method Options include "est_1cm" (default; 1cm estimates), "est_icm" (estimates over original layer boundaries), "est_dcm" (estimates over constant interval, specified with `d` argument to `mpspline3::mpspline()`). Default value for `d` is `c(0, 5, 15, 30, 60, 100, 200)`.
#' @param ... Additional arguments to `mpspline2::mpspline()`
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection with 1cm slices. Spline variables are in columns prefixed with "spline_" and RMSE/RMSE_IQR are in columns prefixed with "rmse_". If any profiles were removed from the collection, their profile IDs are stored in `attr(result, 'removed')`.
#'
#' @export spc2mpspline,SoilProfileCollection-method
#' @aliases spc2mpspline
#' @references T.F.A. Bishop, A.B. McBratney, G.M. Laslett (1999)   Modelling soil attribute depth functions with equal-area quadratic smoothing splines.  Geoderma 91(1–2), pp. 27-45. \doi{https://doi.org/10.1016/S0016-7061(99)00003-8}
#' 
#' O'Brien, Lauren (2022). mpspline2: Mass-Preserving Spline Functions for Soil Data. R package version 0.1.6. \url{https://cran.r-project.org/package=mpspline2}
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
          function(object, 
                   var_name = NULL,
                   method = c("est_1cm", "est_icm", "est_dcm"),
                   pattern = "R|Cr|Cd|qm",
                   hzdesgn = NULL,
                   ...) {
            .NHZ <- NULL
            .LAST <- NULL 
            .HZID <- NULL
            
            if (!requireNamespace('mpspline2'))
              stop("package `mpspline2` is required", call. = FALSE)
            
            if (is.null(var_name) | !(var_name %in% horizonNames(object)))
              stop("argument `var_name` must specify a single horizon-level variable", call. = FALSE)

            method <- match.arg(method[1], c("est_1cm", "est_icm", "est_dcm"))
            
            hztop <- horizonDepths(object)[1]
            hzbot <- horizonDepths(object)[2]
            
            # glom to "available interval" in each profile
            spc.sub <- glom(object, object[[hztop]][object[, 1, .HZID]], 
                                    object[[hzbot]][object[, , .LAST, .HZID]])
            
            # remove any horizons that have 0 or NA thickness (no mass)
            .sameTopBottom <- NULL
            
            # handle any gaps at the surface (e.g. truncated data)
            spc.sub$.sameTopBottom <- spc.sub[[hztop]] == spc.sub[[hzbot]]
            spc.sub$.sameTopBottom <- spc.sub$.sameTopBottom | is.na(spc.sub$.sameTopBottom)
            spc.sub <- subsetHz(spc.sub, !.sameTopBottom)
            
            spc.sub$.mindepth_orig <- spc.sub[, 1][[hztop]]
            
            # handle any gaps at the surface (e.g. truncated data)
            spc.sub <- fillHzGaps(spc.sub, to_top = 0, to_bottom = NULL)
            
            # calculate the top depth and bottom depth for each profile
            spc.sub$.mindepth <- spc.sub[, 1][[hztop]]

            # optionally constrained by some pattern matching
            if (!missing(hzdesgn) && !is.null(hzdesgn)) {
              hzpatdep <- minDepthOf(
                spc.sub,
                pattern = pattern,
                hzdesgn = hzdesgn,
                no.contact.assigned = Inf
              )[[hztop]]
            } else {
              hzpatdep <- rep(Inf, length(spc.sub))
            }
            
            # either the bottom depth of last horizon or the matched pattern top depth
            spc.sub$.maxdepth <- pmin(hzpatdep, spc.sub[, , .LAST][[hzbot]], na.rm = TRUE)
            
            # truncate using vectors of top and bottom
            spc.sub <- trunc(spc.sub, spc.sub$.mindepth, spc.sub$.maxdepth)

            # do the splines
            res <- mpspline2::mpspline(horizons(spc.sub)[c(idname(spc.sub),
                                                           horizonDepths(spc.sub),
                                                           var_name)],
                                       var_name = var_name, ...)
            
            # concatenate results for re-insertion
            pid <- profile_id(spc.sub)
            res2 <- do.call('c', lapply(seq_along(pid), function(i) {
              switch(method,
                "est_1cm" = {  
                  drange <- spc.sub$.mindepth[i]:spc.sub$.maxdepth[i]
                  zero.idx <- drange == 0
                  if (any(zero.idx))
                    drange <- drange[-which(zero.idx)]
                  return(res[[pid[i]]]$est_1cm[drange])
                }, {
                  return(res[[pid[i]]][[method]])
                }
              )
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
            
            # profiles removed (NA in all horizons)
            spc.sub <- spc.sub[profile_id(spc.sub) %in% names(res),]
            
            # create slices 1cm thick to insert spline result
            switch(method, 
                   "est_1cm" = {
                     spc.spl <- suppressMessages(aqp::dice(spc.sub))
                   },
                   "est_icm" = {
                     spc.spl <- spc.sub
                   },
                   "est_dcm" = {
                     .new_d_horizons <- function(x, ...) {
                       if (is.null(list(...)$d)) {
                         d <- c(0, 5, 15, 30, 60, 100, 200)
                       } else {
                         d <- list(...)$d
                       }                  
                       newhzd <- data.frame(
                         id = profile_id(x),
                         top = do.call('c', lapply(d[1:(length(d) - 1)], rep, length(x))),
                         bottom = do.call('c', lapply(d[2:length(d)], rep, length(x)))
                       )
                       colnames(newhzd) <- c(idname(x), horizonDepths(x))
                       depths(newhzd) <- colnames(newhzd)
                       newhzd
                     }

                     spc.spl <- .new_d_horizons(spc.sub, ...)
                   })
                   
            # create new "spline_"+var_name variable
            spc.spl[[paste0(var_name, "_spline")]] <- res2
            
            # create new "rmse_"+var_name as site level attributes
            spc.spl[[paste0(var_name, "_rmse")]] <- reserr
            spc.spl[[paste0(var_name, "_rmse_iqr")]] <- reserr_iqr
            
            # determine what profiles were removed
            removed <- profile_id(object)[!profile_id(object) %in% profile_id(spc.spl)]
            
            # add an attribute with removed profile IDs
            attr(spc.spl, "removed") <- unique(removed)
            
            if (method != "est_dcm") {
              spc.spl <- trunc(spc.spl, spc.sub$.mindepth_orig, spc.sub$.maxdepth)
            }
            
            return(spc.spl)
          })
