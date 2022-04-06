# if (!isGeneric("spc2mpspline"))
  setGeneric("spc2mpspline", function(object,
                                      var_name = NULL,
                                      pattern = "R|Cr|Cd|qm",
                                      hzdesgn = NULL, ...)
    standardGeneric("spc2mpspline"))

#' @title Missing-data-safe, SPC-wide wrapper around `mpspline2::mpspline()` "continuous" 1cm output
#'
#' @description Facilitate safe use of just about any numeric SPC horizon attribute, from any SPC, with `mpspline2::mpspline()`. Currently only works with a single attribute at a time. 
#' 
#' @details This function now relies on the missing data checks provided by mpspline2 package. See `attr(..., 'removed')` to see whole profiles that were removed from the set. Horizons containing `NA` in the property of interest are dropped with a message.
#'
#' Data completeness is assessed and the input SPC is filtered and truncated to create a container for the 1cm results from `mpspline2::mpspline()`.
#'
#' @param object A SoilProfileCollection
#' @param var_name Column name in `@horizons` slot of `object` containing numeric values to spline
#' @param pattern Regex pattern to match for bottom of profile (passed to `minDepthOf()`) default: "R|Cr|Cd|qm"; only used if `hzdesgn` is specified
#' @param hzdesgn Column name in `@horizons` slot of `object` containing horizon designations default: `NULL`
#' @param ... Additional arguments to `mpspline2::mpspline()`
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection with 1cm slices. Spline variables are in columns prefixed with "spline_" and RMSE/RMSE_IQR are in columns prefixed with "rmse_". If any profiles were removed from the collection, their profile IDs are stored in `attr(result, 'removed')`.
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
                   hzdesgn = NULL,
                   ...) {
            .NHZ <- NULL
            .LAST <- NULL 
            .HZID <- NULL
            
            if (!requireNamespace('mpspline2'))
              stop("package `mpspline2` is required", call. = FALSE)
            
            if (is.null(var_name) | !(var_name %in% horizonNames(object)))
              stop("argument `var_name` must specify a single horizon-level variable", call. = FALSE)

            hztop <- horizonDepths(object)[1]
            hzbot <- horizonDepths(object)[2]
            
            # glom to "available interval" in each profile
            spc.sub <- glom(object, object[[hztop]][object[, 1, .HZID]], 
                                    object[[hzbot]][object[, , .LAST, .HZID]])
            
            # remove any horizons that have 0 or NA thickness (no mass)
            .sameTopBottom <- NULL
            spc.sub$.sameTopBottom <- spc.sub[[hztop]] == spc.sub[[hzbot]]
            spc.sub$.sameTopBottom <- spc.sub$.sameTopBottom | is.na(spc.sub$.sameTopBottom)
            spc.sub <- subsetHz(spc.sub, !.sameTopBottom)
            
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
              drange <- spc.sub$.mindepth[i]:spc.sub$.maxdepth[i]
              zero.idx <- drange == 0
              if (any(zero.idx))
                drange <- drange[-which(zero.idx)]
              return(res[[pid[i]]]$est_1cm[drange])
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
            spc.spl <- suppressMessages(aqp::dice(spc.sub))
            
            # create new "spline_"+var_name variable
            spc.spl[[paste0(var_name, "_spline")]] <- res2
            
            # create new "rmse_"+var_name as site level attributes
            spc.spl[[paste0(var_name, "_rmse")]] <- reserr
            spc.spl[[paste0(var_name, "_rmse_iqr")]] <- reserr_iqr
            
            # determine what profiles were removed
            removed <- profile_id(object)[!profile_id(object) %in% profile_id(spc.spl)]
            
            # add an attribute with removed profile IDs
            attr(spc.spl, "removed") <- unique(removed)
            
            return(spc.spl)
          })
