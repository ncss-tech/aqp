# if (!isGeneric("spc2mpspline"))
  setGeneric("spc2mpspline", function(object,
                                      var_name = NULL,
                                      method = c("est_1cm", "est_icm", "est_dcm"),
                                      pattern = "R|Cr|Cd|qm",
                                      hzdesgn = NULL, ...)
    standardGeneric("spc2mpspline"))

#' SoilProfileCollection wrapper for `mpspline2::mpspline()`
#'
#' Generate mass-preserving splines for any numeric attribute in a
#' SoilProfileCollection using `mpspline2::mpspline()`. mpspline2 implements the
#' method described in Bishop et al. (1999). 
#' 
#' @details This function now relies on the missing data checks provided by the
#'   mpspline2 package. See `attr(..., 'removed')` to see whole profiles that
#'   were removed from the set. Horizons containing `NA` in the property of
#'   interest are dropped with a message.
#'
#'   Data completeness is assessed and the input SoilProfileCollection is
#'   filtered and truncated to create a container for the results from
#'   `mpspline2::mpspline()`.
#'
#' @param object A SoilProfileCollection
#' @param var_name Column name(s) in `@horizons` slot of `object` containing
#'   numeric values to spline
#' @param pattern Regex pattern to match for bottom of profile (passed to
#'   `minDepthOf()`) default: "R|Cr|Cd|m"; only used if `hzdesgn` is specified
#' @param hzdesgn Column name in `@horizons` slot of `object` containing horizon
#'   designations default: `NULL`
#' @param method Options include "est_1cm" (default; 1cm estimates), "est_icm"
#'   (estimates over original layer boundaries), "est_dcm" (estimates over
#'   constant interval, specified with `d` argument to `mpspline3::mpspline()`).
#'   Default value for `d` is `c(0, 5, 15, 30, 60, 100, 200)`.
#' @param ... Additional arguments to `mpspline2::mpspline()`
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection with 1cm slices. Spline variables are in
#'   columns prefixed with "spline_" and RMSE/RMSE_IQR are in columns prefixed
#'   with "rmse_". If any profiles were removed from the collection, their
#'   profile IDs are stored in `attr(result, 'removed')`.
#'
#' @export
#' @importFrom stats setNames
#' @aliases spc2mpspline
#' @references Bishop, T.F.A., McBratney, A.B., Laslett, G.M. (1999) Modelling
#'   soil attribute depth functions with equal-area quadratic smoothing splines.
#'   Geoderma 91(1–2), pp. 27-45.
#'   \doi{https://doi.org/10.1016/S0016-7061(99)00003-8}
#'
#'   O'Brien, Lauren (2025). mpspline2: Mass-Preserving Spline Functions for
#'   Soil Data. R package version 0.1.9.
#'   \url{https://cran.r-project.org/package=mpspline2}
#'
#' @examplesIf requireNamespace("mpspline2")
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#' hzdesgnname(sp1) <- "name"
#'   
#' # run on a single variable
#' res <- spc2mpspline(sp1, "prop")
#'   
#' # plot single-variable result
#' plotSPC(res[1:5, ], color = "prop_spline", divide.hz = FALSE)
#' # add a second continuous numeric variable for demonstration
#' sp1$value2 <- runif(nrow(horizons(sp1)))
#'   
#' # run on multiple variables
#' res2 <- spc2mpspline(sp1, c("prop", "value2"))
#'   
#' # plot multi-variable result
#' plotSPC(res2[1:5, ], color = "value2_spline", divide.hz = FALSE)
#'   
#' # run on multiple variables with custom depth intervals
#' res3 <- spc2mpspline(sp1,
#'                      c("prop", "value2"),
#'                      method = "est_dcm",
#'                      d = c(0, 10, 20, 50, 100))
#' plotSPC(res3[1:5, ], color = "value2_spline", divide.hz = FALSE)
#' 
setMethod("spc2mpspline", signature(object = "SoilProfileCollection"),
          function(object, 
                   var_name = NULL,
                   method = c("est_1cm", "est_icm", "est_dcm"),
                   pattern = "R|Cr|Cd|m",
                   hzdesgn = NULL,
                   ...) {
            .NHZ <- NULL
            .LAST <- NULL 
            .HZID <- NULL
            
            if (!requireNamespace('mpspline2'))
              stop("package `mpspline2` is required", call. = FALSE)
            
            if (is.null(var_name) || !all(var_name %in% horizonNames(object)))
              stop("all `var_name` must specify horizon-level variables", call. = FALSE)

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

            if (length(profile_id(spc.sub)) == 0) {
              attr(object, "removed") <- unique(profile_id(object))
              return(object[0, ])
            }
            
            res <- mpspline2::mpspline(horizons(spc.sub)[c(idname(spc.sub),
                                                           horizonDepths(spc.sub),
                                                           var_name)],
                                       var_name = var_name, ...)

            # Handle the difference in mpspline2 return structure
            # Single variable returns list with profile IDs as names
            # Multiple variables returns list with variable names as top level
            if (length(var_name) == 1 && inherits(res[[1]], "list") &&
                "id" %in% names(res[[1]])) {
              # Single variable case: convert to multi-variable structure
              res <- list(res)
              names(res) <- var_name
            }

            # Get all profile IDs that were successfully splined for at least one variable
            all.splined.pids <- unique(unlist(lapply(res, function(v_res) {
              if (inherits(v_res, "list")) {
                names(v_res)
              } else {
                NULL # Handle cases where a variable's result might be NA
              }
            })))
            
            # determine profiles that were splined and filter spc.sub
            splined.profiles <- profile_id(spc.sub)[profile_id(spc.sub) %in% all.splined.pids]
            spc.sub <- spc.sub[which(profile_id(spc.sub) %in% splined.profiles), ]
            
            # which profiles were removed?
            removed.profiles <- profile_id(object)[!profile_id(object) %in% splined.profiles]
            
            if (length(splined.profiles) == 0) {
              attr(object, "removed") <- unique(removed.profiles)
              return(object[0, ])
            }
            
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
                       d <- list(...)$d
                       if (is.null(d)) d <- c(0, 5, 15, 30, 60, 100, 200)
                       pids <- profile_id(x)
                       newhzd <- data.frame(
                         id = rep(pids, each = length(d) - 1),
                         top = rep(d[1:(length(d) - 1)], times = length(pids)),
                         bottom = rep(d[2:length(d)], times = length(pids))
                       )
                       colnames(newhzd) <- c(idname(x), horizonDepths(x))
                       depths(newhzd) <- colnames(newhzd)
                       newhzd
                     }
                     spc.spl <- .new_d_horizons(spc.sub, ...)
                   })

            # Create profile ID to index mapping for efficient subsetting
            spl_pids <- profile_id(spc.spl)
            pid_idx <- setNames(seq_along(spl_pids), spl_pids)
            
            # iterate over variables and add results to spc.spl
            for (v in var_name) {
              res.v <- res[[v]]
              current_splined_pids <- profile_id(spc.spl)
              
              res2 <- do.call('c', lapply(current_splined_pids, function(p) {
                if (p %in% names(res.v) && !is.null(res.v[[p]])) {
                  profile_res <- res.v[[p]]
                  switch(method,
                    "est_1cm" = {
                      # Map spline results to 1cm intervals created by dice()
                      p_idx <- pid_idx[p]
                      max_depth <- max(horizons(spc.spl[p_idx, ])$bottom, na.rm = TRUE)
                      vals <- profile_res$est_1cm[1:max_depth]
                      # Pad with NA if needed
                      if (length(vals) < max_depth) vals <- c(vals, rep(NA, max_depth - length(vals)))
                      vals
                    },
                    "est_icm" = profile_res$est_icm,
                    "est_dcm" = profile_res$est_dcm
                  )
                } else {
                  # Return NA vector for profiles not splined for this variable
                  if (p %in% names(pid_idx)) {
                    p_idx <- pid_idx[p]
                    num_hz <- nrow(horizons(spc.spl[p_idx, ]))
                    
                    if (num_hz > 0) {
                      rep(NA, num_hz)
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                }
              }))
              
              # Extract RMSE values (site-level attributes)
              rmse_vals <- t(sapply(profile_id(spc.spl), function(p) {
                if (p %in% names(res.v) && !is.null(res.v[[p]]) && "est_err" %in% names(res.v[[p]])) {
                  res.v[[p]]$est_err[c("RMSE", "RMSE_IQR")]
                } else {
                  c(RMSE = NA_real_, RMSE_IQR = NA_real_)
                }
              }))
              rmse_values <- rmse_vals[, "RMSE"]
              rmse_iqr_values <- rmse_vals[, "RMSE_IQR"]
              
              # Add splined values and error estimates to spc.spl
              spc.spl[[paste0(v, "_spline")]] <- res2
              site(spc.spl)[[paste0(v, "_rmse")]] <- rmse_values
              site(spc.spl)[[paste0(v, "_rmse_iqr")]] <- rmse_iqr_values
            }
            
            # add an attribute with removed profile IDs
            attr(spc.spl, "removed") <- unique(removed.profiles)
            
            if (method != "est_dcm") {
              spc.spl <- trunc(spc.spl, spc.sub$.mindepth_orig, spc.sub$.maxdepth)
            }
            
            return(spc.spl)
          })