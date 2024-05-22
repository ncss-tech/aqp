
## TODO: tests https://github.com/ncss-tech/aqp/issues/180

#' Segmenting of Soil Horizon Data by Depth Interval
#' 
#' This function segments or subdivides horizon data from a `SoilProfileCollection` or `data.frame` by depth interval (e.g. `c(0, 10)`, `c(0, 50)`, or `25:100`). This results in horizon records being split at the specified depth intervals, which duplicates the original horizon data but also adds new horizon depths. In addition, labels (i.e. `"segment_id"`) are added to each horizon record that correspond with their depth interval (e.g. `025-100`). This function is intended to harmonize horizons to a common support (i.e. depth interval) for further aggregation or summary. See the examples.
#' @param object either a `SoilProfileCollection` or `data.frame`
#' @param intervals a vector of integers over which to slice the horizon data (e.g. `c(25, 100)` or `25:100`)
#' @param trim logical, when `TRUE` horizons in `object` are truncated to the min/max specified in `intervals`. When `FALSE`, those horizons overlapping an interval are marked as such. Care should be taken when specifying more than one depth interval and `trim = FALSE`.
#' @param depthcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("top", "bottom")`), only necessary if `object` is a 
#' @param hzdepcols deprecated being replaced by depthcols.
#' 
#' @details `hz_segment()` performs no aggregation or resampling of the source data, rather, labels are added to horizon records for subsequent aggregation or summary. This makes it possible to process a very large number of records outside of the constraints associated with e.g. `slice()` or `slab()`.
#'
#' @return Either a `SoilProfileCollection` or `data.frame` with the original horizon data segmented by depth intervals. There are usually more records in the resulting object, one for each time a segment interval partially overlaps with a horizon. A new column called `segment_id` identifying the depth interval is added.
#' 
#' @author Stephen Roecker
#' 
#' @seealso [dice()], [glom()]
#' 
#' @export
#'
#' @examples
#' 
#' # example data
#' data(sp1)
#' 
#' # upgrade to SPC
#' depths(sp1) <- id ~ top + bottom
#' 
#' # segment and trim
#' z <- hz_segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE)
#' 
#' # display segment labels
#' # note that there are new horizon boundaries at segments
#' par(mar = c(0, 0, 3, 1))
#' plotSPC(z, color = 'segment_id', width = 0.3)
#' 
#' # highlight new horizon records
#' par(mar = c(0, 0, 2, 1))
#' plotSPC(z, color = NA, default.color = NA, width = 0.3, lwd = 1)
#' plotSPC(sp1, color = NA, default.color = NA, 
#' width = 0.3, lwd = 3, add = TRUE, name = NA, print.id = FALSE)
#' legend('top', horiz = TRUE, 
#' legend = c('original', 'segmented'), 
#' lwd = c(1, 3), cex = 0.85, bty = 'n')
#' 
#' \donttest{
#' # same results as slab()
#' # 10 random profiles
#' s <- lapply(1:10, random_profile, n_prop = 1, SPC = TRUE, method = 'random_walk')
#' s <- combine(s)
#' 
#' a.slab <- slab(s, fm = ~ p1, slab.structure = c(0, 10, 20, 30), slab.fun = mean, na.rm = TRUE)
#' 
#' z <- hz_segment(s, intervals = c(0, 10, 20, 30), trim = TRUE)
#' z <- horizons(z)
#' z$thick <- z$bottom - z$top
#' 
#' a.segment <- sapply(split(z, z$segment_id), function(i) {
#'   weighted.mean(i$p1, i$thick)
#' })
#' 
#' 
#' res <- data.frame(
#'   slab = a.slab$value,
#'   segment = a.segment,
#'   diff = a.slab$value - a.segment
#' )
#'
#' print(res)
#' res$diff < 0.001
#'}
#' 
#' 
#' data(sp5)
#' 
#' # segment by upper 25-cm
#' test1 <- hz_segment(sp5, intervals = c(0, 100))
#' print(test1)
#' nrow(test1)
#' print(object.size(test1), units = "Mb")
#' 
#' # segment by 1-cm increments
#' test2 <- hz_segment(sp5, intervals = 0:100)
#' print(test2)
#' nrow(test2)
#' print(object.size(test2), units = "Mb")
#' 
#' 
#' # segment and aggregate
#' test3 <- hz_segment(horizons(sp5), 
#'                  intervals = c(0, 5, 15, 30, 60, 100, 200), 
#'                  depthcols = c("top", "bottom")
#' )
#' test3$hzthk <- test3$bottom - test3$top
#' test3_agg <- by(test3, test3$segment_id, function(x) {
#'   data.frame(
#'     hzID = x$hzID[1],
#'     segment_id = x$segment_id[1],
#'     average = weighted.mean(x$clay, w = x$hzthk)
#'   )
#' })
#' test3_agg <- do.call("rbind", test3_agg)
#' 
#' head(test3_agg)
#' 
hz_segment <- function(object, intervals, trim = TRUE, depthcols = c("top", "bottom")) {

  # depth interval rules
  dep <- data.frame(
    top = intervals[-length(intervals)],
    bot = intervals[-1],
    stringsAsFactors = FALSE
  )
  n <- max(nchar(intervals))
  dep$id <- paste0(
    formatC(dep$top, width = n, flag = 0), 
    "-", 
    formatC(dep$bot, width = n, flag = 0)
  )
  
  # argument sanity check ----
  test_spc <- inherits(object, 'SoilProfileCollection')
  test_df  <- inherits(object, 'data.frame')
  test_dep <- is.numeric(dep$top) & is.numeric(dep$bot) & all(dep$top < dep$bot)
  
  
  if (!any(test_spc, test_df)) {
    stop("the input must be either a SoilProfileCollection or data.frame")
  }
  
  .check_depthcols_l(depthcols)
  
  if (!test_dep) {
    stop("intervals should be numeric and sequential (e.g. c(0, 1, 2, 3) or 0:100)")
  }
  
  
  # standardize inputs ----
  if (test_spc) {
    idcol     <- idname(object)
    hzidcol   <- hzidname(object)
    depthcols <- horizonDepths(object)
    h  <- horizons(object)
    names(h)[names(h) %in% c(idcol, hzidcol)] <- c("idcol", "hzidcol")
  } else {
    h <- object
  }
  names(h)[names(h) %in% depthcols] <- c("top", "bot")
  

  ## TODO: consider using dice()
  # filter horizons and trim ----
  .slice <- function(h, top = NULL, bot = NULL) {
    idx <- which(h$top < bot & h$bot > top)
    h <- h[idx, ]
    
    # causing errors when FALSE; fixed?
    if (trim == TRUE) {
      h$top = ifelse(h$top < top, top, h$top)
      h$bot = ifelse(h$bot > bot, bot, h$bot)
      
      # h <- h[(h$bot - h$top) > 0, ]
    }
    
    # h <- h[!is.na(h$peiid), ] 
    
    return(h)
  }
  
  # slice spc by intervals ----
  # dep$df <- lapply(1:nrow(dep), function(x) h[0, ]) # pre-allocate memory
  df_str <- cbind(h[0, ], segment_id = NA_character_[0])
  dep$df <- list(df_str)[rep(1, nrow(dep))] # pre-allocate memory faster
  h <- {
    split(dep, dep$id) ->.;
    lapply(., function(x) {
      temp <- .slice(h, top = x$top, bot = x$bot)
      if (nrow(temp) > 0) x$df[[1]] <- cbind(temp, segment_id = x$id)
      return(x$df[[1]])
    }) ->.;
    do.call("rbind", .) ->.;
    }
  names(h)[names(h) %in% c("top", "bot")] <- depthcols
  
  
  if (test_spc) {
    h <- h[order(h$idcol, h[[depthcols[1]]]), ]
    
    # merge to re-add spc with NA
    h_orig <- data.frame(idcol = names(table(horizons(object)[idcol])), stringsAsFactors = FALSE)
    h <- merge(h_orig, h, by = "idcol", all.x = TRUE, sort = FALSE)
    rm(h_orig)
    
    ## TODO: consider adding a flag to indicate "new" horizon records that have been added
    
    # rebuild SPC ----
    names(h)[names(h) == "idcol"]   <- idcol
    names(h)[names(h) == "hzidcol"] <- hzidcol
    h$hzID <- 1:nrow(h)
    
    replaceHorizons(object) <- h
    suppressMessages(hzidname(object) <- "hzID")
  }
  
  # return
  if(test_spc){
    return(suppressMessages(rebuildSPC(object))) # TODO: this is protection from missing-data/ID offset
  } else {
    return(h)
  }
  
}

#' @export 
#' @rdname hz_segment
segment <- function(object, intervals, trim = TRUE, hzdepcols = c("top", "bottom")) {
  .Deprecated("segment() will be deprecated and replaced by hz_segment()")
  hz_segment(object, intervals, trim, depthcols = hzdepcols)
}



#' @title Dissolving horizon boundaries by grouping variables
#' 
#' @description This function dissolves or combines horizons that have a common set of grouping variables. It only combines those horizon records that are sequential (e.g. share a horizon boundary). Thus, it can be used to identify discontinuities in the grouping variables along a profile and their unique depths. It is particularly useful for determining the depth to the top or bottom of horizons with a specific category, and should be simpler than previous methods that require aggregating over profiles. 
#'
#' @param object a \code{data.frame}
#' @param by character: column names, to be used as grouping variables, within the object.
#' @param idcol character: column name of the pedon ID within the object.
#' @param depthcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("top", "bottom")`).
#' @param id deprecated and replaced with idcol.
#' @param hztop deprecated and replaced by depthcols.
#' @param hzbot deprecated and replaced by depthcols.
#' @param collapse logical: indicating whether to not combine grouping variables before dissolving.
#' @param order logical: indicating whether or not to order the object by the id, hztop, and hzbot columns. 
#'
#' @details This function assumes the profiles and horizons within the object follow the logic defined by \code{checkHzDepthLogic} (e.g. records are ordered sequentially by id, hztop, and hzbot and without gaps). If the records are not ordered, set the \code{order = TRUE}.
#'
#' @return A \code{data.frame} with the original idcol, by grouping variables, and non-consecutive horizon depths. 
#' 
#' @author Stephen Roecker
#' 
#' @seealso \code{\link{checkHzDepthLogic}}
#' 
#' @export
#'
#' @examples
#' 
#' # example 1
#' data(jacobs2000)
#' spc <- jacobs2000
#' 
#' spc$dep_5 <- spc$depletion_pct >=5
#' spc$genhz <- generalize.hz(spc$name, c("A", "E", "B", "C"), c("A", "E", "B", "C")) 
#' h <- horizons(spc)
#' 
#' test <- hz_dissolve(h, by = c("genhz", "dep_5"), idcol = "id", depthcols = c("top", "bottom"))
#' 
#' vars <- c("id", "top", "bottom", "genhz", "dep_5")
#' h[h$id == "92-1", vars]
#' test[test$id == "92-1", ]
#' 
#' 
#' # example 2
#' df <- data.frame(
#'     id = 1,
#'     top    = c(0, 5,  10, 15, 25, 50), 
#'     bottom = c(5, 10, 15, 25, 50, 100),
#'     hzname = c("A1",  "A2",  "E/A", "2Bt1", "2Bt2", "2C"),
#'     genhz  = c("A",   "A",   "E",   "2Bt",  "2Bt", "2C"),
#'     texcl  = c("sil", "sil", "sil", "sl",   "sl",   "s")
#'     )
#' 
#' df
#' 
#' hz_dissolve(df, c("genhz", "texcl"))
#' hz_dissolve(df, c("genhz", "texcl"), collapse = TRUE)
#' 
#' test <- hz_dissolve(df, "genhz")
#' subset(test, value == "2Bt")
#' 


hz_dissolve <- function(object, by, idcol = "id", depthcols = c("top", "bottom"), collapse = FALSE, order = FALSE) {
  
  # id = "peiid"; hztop = "hzdept"; hzbot = "hzdepb", collapse = FALSE, order = FALSE
  
  # test inputs ----
  # argument sanity check
  # test_spc <- inherits(object, 'SoilProfileCollection')
  
  # check that object & by are the right class
  test_object <- inherits(object, "data.frame")
  if (!any(test_object)) {
    stop("the object argument must be a data.frame", call. = FALSE)
  }

  
  # check that collapse is a logical of length 1
  if (!inherits(collapse, "logical") || length(collapse) != 1) {
    stop("the collapse argument must be logical and a length of one", call. = FALSE)
  }
  
  
  # check that by is not NULL
  if (is.null(by)) stop("the by argument must not be NULL")
  
  
  # check that "by" are characters or convert
  if (any(!"character" %in% sapply(object[by], class))) {
    message("non-character grouping variables are being converted to characters")
    object[by] <- lapply(object[by], as.character)
  }
  
  
  # check that the column names exist within the object
  .check_names(object, vars = c(idcol = idcol, top = depthcols[1], bot = depthcols[2], by))
  
  
  # check if previous dissolve_id exists and overwrite
  nm  <- names(object)
  idx <- nm == "dissolve_id"
  if (any(idx)) {
    warning("object contains an existing column named 'dissolve_id', it will be overwritten") 
    object[idx] <- NULL
  }
  
  
  # standardize inputs ----
  df_std <- .standardize_inputs(object, idcol = idcol, depthcols = depthcols)
  df_conversion <- df_std$x_conversion
  df <- df_std$x; rm(df_std)
  
  
  # valid
  # vd_idx <- validate_depths(df, id = "id", hztop = "hzdept", bot = "hzdepb")
  if (order == TRUE) {
    df <- df[order(df$idcol, df$top, df$bot), ]
  }
  
  if (collapse == TRUE) {
    by_co <- paste(by, collapse = " & ")
    df[by_co] <- apply(df[by], 1, paste, collapse = " & ")
    by    <- by_co
  }
  
  
  # var thickness ----
  var_dep <- lapply(by, function(x) {
    
    con_bot <- rle(    paste(df$idcol, df[, x]))$length
    con_top <- rle(rev(paste(df$idcol, df[, x])))$length
    
    bot_idx <- cumsum(con_bot)
    top_idx <- cumsum(con_top)
    
    vd <- data.frame(
      idcol = df[bot_idx, "idcol"],
      top   = rev(rev(df$top)[top_idx]),
      bot   = df[bot_idx, "bot"],
      variable = x,
      value = df[bot_idx,    x]
    )
    # vd$dissolve_id = 1:nrow(vd)
    
    return(vd)
  })
  var_dep <- do.call("rbind", var_dep)
  
  
  # this is redundant with collapse = TRUE, and inappropriate unless the grouping by variable matches across all horizon depths, otherwise it'll generate pedons with overlapping depths
  # if (direction == "wide") {
  #   var_dep <- reshape(
  #     var_dep,
  #     direction = "wide",
  #     idvar = c("id", "top", "bot"),
  #     timevar = "variable",
  #     v.names = "value"
  #   )
  # }
  
  
  # append dissolve_id
  n <- c(
    var_dep$top, 
    var_dep$bot
    ) |>
    nchar() |>
    max(na.rm = TRUE)
  var_dep$dissolve_id <- paste0(
    var_dep$idcol,
    "_",
    formatC(var_dep$top, width = n, flag = 0), 
    "-", 
    formatC(var_dep$bot, width = n, flag = 0),
    "_",
    var_dep$variable
  )
  
  
  # reset inputs ----
  var_dep <- .reset_inputs(var_dep, df_conversion)
  
  return(var_dep)
}


#' @export 
#' @rdname hz_dissolve

dissolve_hz <- function(object, by, id = "idcol", hztop = "top", hzbot = "bottom", collapse = FALSE, order = FALSE) {
  .Deprecated("dissolve_hz() will be deprecated and replaced by hz_dissolve()")
  hz_dissolve(object, by, idcol = id, depthcols = c(hztop, hzbot), collapse, order)
  }



#' @title Intersecting horizon boundaries by horizon depths
#' 
#' @description This function intersects two horizon tables by harmonizing their depths and merging them where they overlap. This can be useful to rejoin the results of `hz_dissolve()` to it's original horizon table, and then perform an aggregation on the dissolved variables.
#'
#' @param x a \code{data.frame}
#' @param y a \code{data.frame}
#' @param idcol character: column name of the pedon ID within the object.
#' @param depthcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("top", "bottom")`).
#'
#' @details .
#'
#' @return A \code{data.frame} with harmonized depth intervals (i.e. segment_id) and columns from both of the original \code{data.frame}. If both \code{data.frame} contain the same column names, they will both be returned (with the exception of the idcol and depthcols), and appended with either x or y to indicate which \code{data.frame} they originated from. 
#' 
#' @author Stephen Roecker
#' 
#' 
#' @export
#'
#' @examples
#' 
#' h <- data.frame(
#' id = 1,
#' top    = c(0,  25, 44, 46, 50),
#' bottom = c(25, 44, 46, 50, 100),
#' by     = c("Yes", "Yes", "No", "No", "Yes"),
#' clay   = c(10, 12, 27, 35, 16)
#' )
#' 
#' h |> hz_dissolve("by")
#' 
#' h |> hz_dissolve("by") |> hz_intersect(x = _, y = h)
#' 
#' h |> 
#' hz_dissolve("by") |> 
#' hz_intersect(x = h, y = _) |>
#' aggregate(clay ~ dissolve_id, data = _, mean)
#' 



hz_intersect <- function(x, y, idcol = "id", depthcols = c("top", "bottom")) {
  
  # test inputs ----
  # argument sanity check
  
  # check that depthcols ----
  ## length == 2
  if (length(depthcols) != 2) stop("depthcols must length must equal 2")
  
  ## check for matching column names
  .check_names(x, c(idcol, depthcols))
  .check_names(y, c(idcol, depthcols))
  
  
  # check segment_id ----
  ## if it exists, overwrite it
  x_nm <- names(x)
  y_nm <- names(y)
  if (any(x_nm %in% "segment_id")) {
    warning("x includes a column named 'segment_id', it will be overwritten")
    x[x_nm == "segment_id"] <- NULL
  }
  
  if (any(y_nm %in% "segment_id")) {
    warning("y includes a column named 'segment_id', it will be overwritten")
    y[y_nm == "segment_id"] <- NULL
  }
  
  
  # standardize inputs ----
  x_std <- .standardize_inputs(x, idcol = idcol, depthcols = depthcols)
  x_conversion <- x_std$x_conversion
  x            <- x_std$x; rm(x_std)
  
  y <- .standardize_inputs(y, idcol = idcol, depthcols = depthcols)$x
  
  # intersect x & y ----
  split(x, x$idcol) ->.;
  lapply(., function(x) {
    xi <- x
    yi <- y[which(y$idcol == xi$idcol[1]), ]
    
    if (nrow(yi) > 0) {
      
      int <- c(xi$top, xi$bot, yi$top, yi$bot) |>
      sort() |>
      unique()
      
      xi_seg <- hz_segment(xi, intervals = int, depthcols = names(x_conversion[2:3]), trim = TRUE)
      yi_seg <- hz_segment(yi, intervals = int, depthcols = names(x_conversion[2:3]), trim = TRUE)
      
      return(list(x_seg = xi_seg, y_seg = yi_seg))
    }
  }) ->.;
  
  
  x_seg <- lapply(., function(x) x[["x_seg"]]) |> do.call("rbind", args = _)
  y_seg <- lapply(., function(x) x[["y_seg"]]) |> do.call("rbind", args = _)

  
  xy_int <- merge(x_seg, y_seg, by = c("segment_id", "idcol", "top", "bot"))
  
  
  # reset inputs ----
  xy_int <- .reset_inputs(xy_int, x_conversion)
  
  return(xy_int)
  }



#' @title Find lagged horizon values
#' 
#' @description This function finds adjacent values to a horizon values at lagged distances.
#'
#' @param object a \code{data.frame}
#' @param lag integer: number of horizons to lag
#' @param unit character: lag units in index or depth.
#' @param idcol character: column name of the pedon ID within the object.
#' @param depthcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("top", "bottom")`).
#' @param order logical: indicating whether or not to order the #'
#' @details .
#'
#' @return A \code{data.frame} with lagged values. 
#' 
#' @author Stephen Roecker
#' 
#' 
#' @export
#'
#' @examples
#' 
#' h <- data.frame(
#' id = 1,
#' top    = c(0,  25, 44, 46, 50),
#' bottom = c(25, 44, 46, 50, 100),
#' texcl     = c("SL", "SL", "CL", "CL", "L"),
#' clay   = c(10, 12, 27, 35, 16)
#' )
#' 
#' h |> hz_lag()
#' 
#' h |> hz_lag(-1)
#' 
#' h |> hz_lag(10:15, unit = "depth")
#' 
#' h |> 
#' hz_lag() |> 
#' cbind(h, lag = _) |>
#' transform(
#' clay_dif = lag.clay_bot.1 - clay,
#' texcl_contrast = paste0(texcl, "-", lag.texcl_bot.1))
#' 



hz_lag <- function(object, lag = 1, unit = "index", idcol = "id", depthcols = c("top", "bottom"), order = FALSE) {
  
  nm <- names(object)
  idx_std <- which(! nm %in% c(idcol, depthcols))
  vars <- nm[idx_std]
  
  
  # check arguments ----
  .check_depthcols_l(depthcols)
  .check_names(object, vars = c(idcol, depthcols, vars))
  
  
  # standardize inputs ----
  x_std <- .standardize_inputs(object, idcol = idcol, depthcols = depthcols)
  x_conversion <- x_std$x_conversion
  x            <- x_std$x; rm(x_std)
   
  
  # check depths ---
  if (unit == "depth" & max(object[[depthcols[2]]] > 1000)) {
    warning("The maximum depth is greater than 1000, which is implausible and will be removed. To avoid this action either remove the offending horizon or convert the depth units to a measure which will not exceed 1000")
    x <- x[x$bot < 1000, ]
  }
  
  test <- aggregate(top ~ idcol, data = x, length)$top |> max()
  if (unit == "index") {
    if ((test - 1) < max(lag)) {
    stop("lag can not be greater than the maximum number of horizons")
    }
  }
  
  
  # order ----
  if (order) {
    x[order(x$idcol, x$top, x$bot), ]
  }
  
  
  # lag ----
  .lag_ind <- function(x, lag = lag) {
    
    nr  <- nrow(x)
    top <- 1:nr
    if (lag >= 0) bot <- c((1 + lag):nr, rep(NA, lag))
    if (lag <  0) bot <- c(rep(NA, abs(lag)), 1:(nr + lag))
    
    test_idcol <- x$idcol[top] == x$idcol[bot] 
    lag_vars <- x[test_idcol * bot, vars]
    if (lag >= 0) names(lag_vars) <- paste0(vars, "_bot.",     lag)
    if (lag <  0) names(lag_vars) <- paste0(vars, "_top.", abs(lag))

    return(lag_vars)
  }
  
  
  .lag_dep <- function(x, lag = lag) {
    
    n <- length(x)
    x$.ID <- 1:nrow(x)
    x_seg <- hz_segment(x, intervals = min(x$top):max(x$bot), trim = TRUE, depthcols = c("top", "bot"))
    x_seg <- x_seg[1:(n + 1)]
    
    
    x_seg <- lapply(lag, function(i) {
      
      x$bot_i <- x$bot + i
      idx <- match(
        paste(x$idcol,     x$bot_i),
        paste(x_seg$idcol, x_seg$bot)
        )
      xi_seg <- x_seg[idx, ]
      xi_seg <- x[xi_seg$.ID, vars, drop = FALSE]
      xi_seg$.ID <- NULL
      
      if (i >= 0) names(xi_seg) <- paste0(names(xi_seg), "_bot.",     i)
      if (i <  0) names(xi_seg) <- paste0(names(xi_seg), "_top.", abs(i))
      
      
      return(xi_seg)
    }) |>
      do.call("cbind", args = _)
    
    return(x_seg)
  }
  
  
  if (unit == "index") {
    x_lag <- lapply(lag, function(i) {
      .lag_ind(x, i)
    }) |>
      do.call("cbind", args = _)
    x_lag <- x_lag[sort(names(x_lag))]
  }
  if (unit == "depth") {
    x_lag <- .lag_dep(x, lag)
    x_lag <- x_lag[sort(names(x_lag))]
  }
  
  
  # # reset inputs ----
  x <- .reset_inputs(cbind(x, x_lag), x_conversion)
  
   
  return(x_lag)
}



# check depthcols length
.check_depthcols_l <- function(x) {
  if (length(x) != 2 & !is.null(x)) stop("depthcols must length must equal 2")
}


## check for matching column names
.check_names <- function(x, vars) {
  
  x_nm <- names(x)
  
  if (! all(vars %in% x_nm)) {
    stop("x must contain columns with names that match the input arguments")
  }
}


# standardize inputs
.standardize_inputs <- function(x, idcol = NULL, hzidcol = NULL, depthcols = NULL) {
  
  # set new names
  var_names <- c(
    idcol   = idcol, 
    hzidcol = hzidcol, 
    top     = depthcols[1], 
    bot     = depthcols[2]
  )
  
  # find matches
  idx_x <- sapply(var_names, function(i) which(names(x) == i))
  
  # rename matching column names
  names(x)[idx_x] <- names(var_names)
  
  return(list(x = x, x_conversion = var_names))
}


.reset_inputs <- function(x, conversion) {
  
  # find original names
  idx <- which(names(x) %in% names(conversion))
  
  # reset original names
  names(x)[idx] <- conversion
  
  return(x)
}
  