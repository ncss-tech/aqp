#' Calculate Thickness of Horizons Matching Logical Criteria
#'
#' This function calculates the cumulative (`method="cumulative"`, default) or maximum difference between (`method="minmax"`) horizons within a profile that match a defined pattern (`pattern`) or, more generally, any set of horizon-level logical expressions encoded in a function (`FUN`).
#' 
#' @param x A _SoilProfileCollection_
#' @param pattern _character_. A pattern to match in `hzdesgn`; used with the default `FUN` definition for regular expression pattern matching on horizons.
#' @param hzdesgn _character_. A column containing horizon designations or other horizon-level character label used to identify matches; used with the default `FUN` definition.
#' @param method _character_. Either `"cumulative"` (default) or `"minmax"`. See details.
#' @param prefix _character_. Column prefix for calculated `thickvar` (and `depthvar` for `method="minmax"`) column results. Default: `""`.
#' @param thickvar _character_ Length `1`. Column name to use for calculated thickness column. Default: `"thickness"`
#' @param depthvars _character_. Length `2`. Column names to use for calculated minimum top depth and maximum bottom depth in `method="minmax"`. Default: `horizonDepths(x)`
#' @param FUN _function_. A function that returns a _logical_ vector equal in length to the number of horizons in `x`. See details.
#' @param na.rm _logical_. Omit `NA` values in summaries of thickness and in matching? Default: `FALSE`
#' @param ... Additional arguments passed to the matching function `FUN`.
#'
#' @return A _data.frame_-like object (corresponding to the `aqp_df_class()` of `x`) with one row per profile in `x`. First column is always the profile ID which is followed by `"thickness"`. In `method="minmax"` the upper and lower boundaries used to calculate `"thickness"` are also returned as `"tmin"` and `"tmax"` columns, respectively.
#' 
#' @details
#' The two thickness methods currently available are:
#'  - `method="cumulative"` (default): cumulative thickness of horizons where `FUN` returns true
#'  - `method="minmax"`: maximum bottom depth minus minimum top depth of horizons where `FUN` returns true
#' 
#' If a custom function (`FUN`) is used, it should accept arbitrary additional arguments via an ellipsis (`...`).
#' It is not necessary to do anything with arguments, but the result should match the number of horizons found in the input SoilProfileCollection `x`.
#' 
#' @export
#'
#' @examples
#' data("jacobs2000")
#' 
#' # cumulative thickness of horizon designations matching "Bt"
#' thicknessOf(jacobs2000, "Bt")
#' 
#' # maximum bottom depth minus minimum top depth of horizon designations matching "Bt"
#' thicknessOf(jacobs2000, "Bt", prefix = "Bt_", method = "minmax")
#' 
#' # cumulative thickness of horizon designations matching "A|B"
#' thicknessOf(jacobs2000, "A|B", prefix = "AorB_")
#' 
#' # maximum bottom depth minus minimum top depth of horizon designations matching "A|B"
#' thicknessOf(jacobs2000, "A|B", method = "minmax", prefix = "AorB_")
#' # note that the latter includes the thickness of E horizons between the A and the B
#' 
#' # when using a custom function (be sure to accept ... and consider the effect of NA values)
#' 
#' # calculate cumulative thickness of horizons containing >18% clay
#' thicknessOf(jacobs2000, prefix = "claygt18_", 
#'             FUN = function(x, ...) !is.na(x[["clay"]]) & x[["clay"]] > 18)
#' 
thicknessOf <- function(x, 
                        pattern = NULL, 
                        hzdesgn = guessHzDesgnName(x, required = TRUE), 
                        method = "cumulative",
                        prefix = "",
                        thickvar = "thickness",
                        depthvars = horizonDepths(x),
                        FUN = function(x, pattern, hzdesgn, ...) grepl(pattern, x[[hzdesgn]]),
                        na.rm = FALSE,
                        ...) {
  .internalTHK <- NULL
  .interalHZM <- NULL
  
  # check inputs
  method <- match.arg(tolower(gsub("/", "", method)), c("cumulative", "minmax"))
  stopifnot(length(thickvar) == 1)
  stopifnot(length(depthvars) == 2)
  
  # extract SPC column names and horizon data
  hzd <- horizonDepths(x)
  idn <- idname(x)
  h <- data.table::data.table(horizons(x))
  
  # determine horizons matching criteria of interest
  h$.internalHZM <- FUN(x, pattern = pattern, hzdesgn = hzdesgn, na.rm = na.rm, ...)
 
  # create a named list for data.table aggregation 
  lid <- list(h[[idn]])
  names(lid) <- idn
  
  if (method == "cumulative") {
    
    # sum thicknesses of all matching horizons
    h$.internalTHK <- x[[hzd[2]]] - x[[hzd[1]]]
    res <- h[, list(thickness = sum(.internalTHK[.internalHZM], na.rm = na.rm)), by = lid]
    colnames(res)[2] <- thickvar
    
  } else if (method == "minmax") {
    
    # determine minimum top depth and maximum bottom depth of matching horizons
    res <- h[, list(`min` = suppressWarnings(min(.SD[[1]][.internalHZM], na.rm = na.rm)),
                    `max` = suppressWarnings(max(.SD[[2]][.internalHZM], na.rm = na.rm))),
              by = lid, .SDcols = c(hzd, ".internalHZM")]
    
    # calculate thickness as MAX(bottom) - MIN(top)
    res$thickness <- res$`max` - res$`min`

    # if a profile has no matching horizons min/max results will be +/- infinity 
    # this means the profile has 0 thickness of matching horizons
    res$thickness[!is.finite(res$thickness)] <- 0L
    
    # use user-defined labels
    colnames(res)[2:3] <- depthvars
    colnames(res)[4] <- thickvar
  } else stop("unknown thickness method: ", shQuote(method), call. = FALSE)
  
  # add prefix
  colnames(res)[2:ncol(res)] <- paste0(prefix, colnames(res)[2:ncol(res)])
  
  # return as SPC data.frame class type
  .as.data.frame.aqp(res, aqp_df_class(x))
}

