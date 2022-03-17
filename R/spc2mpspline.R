# if (!isGeneric("spc2mpspline"))
  setGeneric("spc2mpspline", function(object,
                                      var_name = NULL,
                                      pattern = "R|Cr|Cd|qm",
                                      hzdesgn = guessHzDesgnName(object), ...)
    standardGeneric("spc2mpspline"))

#' @title Missing-data-safe, SPC-wide wrapper around `mpspline2::mpspline()` "continuous" 1cm output
#'
#' @description Facilitate safe use of just about any numeric SPC horizon attribute, from any SPC, with `mpspline2::mpspline()`. Currently only works with a single attribute at a time. 
#' 
#' @details This function will automatically filter profiles with `NA` in attribute of interest which may be more conservative filtering than you expect. The intention here is that a SPC of related profile instances could be splined, and then the spline results aggregated over the full interval where data was available.
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
            
            # mpspline2::mpspline contains its own NA fixing/handling code, but we 
            # need to be able to account for what it removes, and dice() can't handle it
            na.idx <- which(is.na(spc.sub[[var_name]]))
            spc.sub <- spc.sub[!profile_id(spc.sub) %in% object@horizons[[idname(spc.sub)]][na.idx]]
            # # TODO:
            # # need to remove the horizons that have NA in var_name at a minimum
            # # something like...
            # spc.sub$.missingData <- is.na(spc.sub[[var_name]])
            # spc.sub <- subsetHz(spc.sub, !.missingData)
            
            # calculate the top depth and bottom depth for each profile
            mindepth <- spc.sub[, 1][[hztop]]
            
            # optionally constrained by some pattern matching
            if (!missing(hzdesgn) && !is.null(hzdesgn)) {
              hzpatdep <- minDepthOf(spc.sub, pattern = pattern, hzdesgn = hzdesgn)[[hztop]]
            } else {
              hzpatdep <- numeric(0)
            }
            
            # either the bottom depth of last horizon or the matched pattern top depth
            maxdepth <- pmin(c(hzpatdep, spc.sub[, , .LAST][[hzbot]]), na.rm = TRUE)
            
            # truncate using vectors of top and bottom
            spc.sub <- trunc(spc.sub, mindepth, maxdepth)
            
            # do the splines
            res <- mpspline2::mpspline(horizons(spc.sub)[c(idname(spc.sub),
                                                           horizonDepths(spc.sub),
                                                           var_name)],
                                       var_name = var_name, ...)
            
            # single horizon results cannot be splined, filter those out
            spc.sub <- spc.sub[which(spc.sub[, , .NHZ] > 1),]
            
            # concatenate results for re-insertion
            pid <- profile_id(spc.sub)
            res2 <- do.call('c', lapply(seq_along(pid), function(i) {
              drange <- mindepth[i]:maxdepth[i]
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
            
            # inspect
            #reserr_iqr
            #reserr
            
            # create slices 1cm thick to insert spline result
            spc.spl <- suppressMessages(aqp::dice(spc.sub))
                                                     
            
            # create new "spline_"+var_name variable
            spc.spl[[paste0(var_name,"_spline")]] <- res2
            
            # create new "rmse_"+var_name as site level attributes
            spc.spl[[paste0(var_name,"_rmse")]] <- reserr
            spc.spl[[paste0(var_name,"_rmse_iqr")]] <- reserr_iqr
            
            # determine what profiles were removed
            removed <- profile_id(object)[!profile_id(object) %in% profile_id(spc.spl)]
            
            # add an attribute with removed profile IDs. there are three steps
            # that possibly remove data:
            #  - profiles removed by glom have no var_name data at all.
            #  - 100% coverage filtering step -- conservative filter to keep from making splines from bad data
            #  - mpspline itself will remove profiles with e.g. just one horizon
            attr(spc.spl, "removed") <- unique(removed)
            
            return(spc.spl)
          })
