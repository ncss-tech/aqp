
## TODO: tests https://github.com/ncss-tech/aqp/issues/180

#' @title Segmenting of Soil Horizon Data by Depth Interval
#' 
#' @description This function segments or subdivides horizon data from a `SoilProfileCollection` or `data.frame` by depth interval (e.g. `c(0, 10)`, `c(0, 50)`, or `25:100`). This results in horizon records being split at the specified depth intervals, which duplicates the original horizon data but also adds new horizon depths. In addition, labels (i.e. `"segment_id"`) are added to each horizon record that correspond with their depth interval (e.g. `025-100`). This function is intended to harmonize horizons to a common support (i.e. depth interval) for further aggregation or summary. See the examples.
#'
#' @param object either a `SoilProfileCollection` or `data.frame`
#' @param intervals a vector of integers over which to slice the horizon data (e.g. `c(25, 100)` or `25:100`)
#' @param trim logical, when `TRUE` horizons in `object` are truncated to the min/max specified in `intervals`. When `FALSE`, those horizons overlapping an interval are marked as such. Care should be taken when specifying more than one depth interval and \code{`trim = FALSE`}.
#' @param hzdepcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("hzdept", "hzdepb")`), only necessary if `object` is a `data.frame`.
#' 
#' @details `segment()` performs no aggregation or resampling of the source data, rather, labels are added to horizon records for subsequent aggregation or summary. This makes it possible to process a very large number of records outside of the constraints associated with e.g. \code{slice} or \code{slab}.
#'
#' @return Either a `SoilProfileCollection` or `data.frame` with the original horizon data segmented by depth intervals. There are usually more records in the resulting object, one for each time a segment interval partially overlaps with a horizon. A new column called \code{segment_id} identifying the depth interval is added.
#' 
#' @author Stephen Roecker
#' 
#' @seealso [dice()] [glom()]
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
#' z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE)
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
#' z <- segment(s, intervals = c(0, 10, 20, 30), trim = TRUE)
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
#' test1 <- segment(sp5, intervals = c(0, 100))
#' print(test1)
#' nrow(test1)
#' print(object.size(test1), units = "Mb")
#' 
#' # segment by 1-cm increments
#' test2 <- segment(sp5, intervals = 0:100)
#' print(test2)
#' nrow(test2)
#' print(object.size(test2), units = "Mb")
#' 
#' 
#' # segment and aggregate
#' test3 <- segment(horizons(sp5), 
#'                  intervals = c(0, 5, 15, 30, 60, 100, 200), 
#'                  hzdepcols = c("top", "bottom")
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
segment <- function(object, intervals, trim = TRUE, hzdepcols = NULL) {
  
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
  
  # argument sanity check
  test_spc <- inherits(object, 'SoilProfileCollection')
  test_df  <- inherits(object, 'data.frame')
  test_hd  <- !is.null(hzdepcols) & length(hzdepcols) == 2
  test_dep <- is.numeric(dep$top) & is.numeric(dep$bot) & all(dep$top < dep$bot)
  
  
  if (! any(test_spc, test_df)) {
    stop("the input must be either a SoilProfileCollection or data.frame")
  }
  
  if (!test_spc & (!test_df | !test_hd)) {
    stop("if the input is a data.frame then hzdepcols must not be NULL and length(hzdepcols) == 2")
  }
  
  if (!test_dep) {
    stop("intervals should be numeric and sequential (e.g. c(0, 1, 2, 3) or 0:100)")
  }
  
  
  # standardize inputs
  if (test_spc) {
    peid      <- idname(object)
    hzid      <- hzidname(object)
    hzdepcols <- horizonDepths(object)
    h  <- horizons(object)
    names(h)[names(h) %in% c(peid, hzid)] <- c("peid", "hzid")
  } else {
    h <- object
  }
  names(h)[names(h) %in% hzdepcols] <- c("hzdept", "hzdepb")
  
  # filter horizons and trim
  .slice <- function(h, top = NULL, bot = NULL) {
    idx <- h$hzdept <= bot & h$hzdepb >= top
    h <- h[idx, ]
    
    # causing errors when FALSE
    if (trim == TRUE) {
      h <- within(h, {
        hzdept = ifelse(hzdept < top, top, hzdept)
        hzdepb = ifelse(hzdepb > bot, bot, hzdepb)
        })
      
    h <- h[(h$hzdepb - h$hzdept) > 0, ]
    }
    
    return(h)
  }
  
  # slice spc by intervals
  # dep$df <- lapply(1:nrow(dep), function(x) h[0, ]) # pre-allocate memory
  dep$df <- list(h[0, ])[rep(1, nrow(dep))] # pre-allocate memory faster
  h <- {
    split(dep, dep$id) ->.;
    lapply(., function(x) {
      x$df[[1]] <- cbind(.slice(h, top = x$top, bot = x$bot), segment_id = x$id)
      return(x$df[[1]])
    }) ->.;
    do.call("rbind", .) ->.;
    }
  names(h)[names(h) %in% c("hzdept", "hzdepb")] <- hzdepcols
  
  
  if (test_spc) {
    h <- h[order(h$peid, h[[hzdepcols[1]]]), ]
    
    # merge to re-add spc with NA
    h_orig <- data.frame(peid = names(table(horizons(object)[peid])), stringsAsFactors = FALSE)
    h <- merge(h_orig, h, by = "peid", all.x = TRUE, sort = FALSE)
    rm(h_orig)
    
    ## TODO: consider adding a flag to indicate "new" horizon records that have been added
    
    # rebuild SPC
    names(h)[names(h) == "peid"] <- peid
    names(h)[names(h) == "hzid"] <- hzid
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


#' @title Dissolving horizon boundaries by grouping variables
#' 
#' @description This function dissolves or combines horizons that have a common set of grouping variables. It only combines those horizon records that are sequential (e.g. share a horizon boundary). Thus, it can be used to identify discontinuities in the grouping variables along a profile and their unique depths. It is particularly useful for determining the depth to the top or bottom of horizons with a specific category, and should be simpler than previous methods that require aggregating over profiles. 
#'
#' @param object a \code{data.frame}
#' @param by character: column names, to be used as grouping variables, within the object.
#' @param id character: column name of the pedon ID within the object.
#' @param hztop character: column name of the horizon top depth within the object.
#' @param hzbot character: column name of the horizon bottom depth in the object.
#' @param collapse logical: indicating whether to not combine grouping variables before dissolving.
#' @param order logical: indicating whether or not to order the object by the id, hztop, and hzbot columns. 
#' #' 
#' @details This function assumes the profiles and horizons within the object follow the logic defined by \code{checkHzDepthLogic} (e.g. records are ordered sequentially by id, hztop, and hzbot and without gaps). If the records are not ordered, set the \code{order = TRUE}.
#'
#' @return A \code{data.frame} with the original id, by grouping variables, and non-consecutive horizon depths. 
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
#' test <- dissolve_hz(h, by = c("genhz", "dep_5"), id = "id", hztop = "top", hzbot = "bottom")
#' 
#' vars <- c("id", "top", "bottom", "genhz", "dep_5")
#' h[h$id == "92-1", vars]
#' test[test$id == "92-1", ]
#' 
#' 
#' # example 2
#' df <- data.frame(
#'     peiid = 1,
#'     hzdept = c(0, 5,  10, 15, 25, 50), 
#'     hzdepb = c(5, 10, 15, 25, 50, 100),
#'     hzname = c("A1",  "A2",  "E/A", "2Bt1", "2Bt2", "2C"),
#'     genhz  = c("A",   "A",   "E",   "2Bt",  "2Bt", "2C"),
#'     texcl  = c("sil", "sil", "sil", "sl",   "sl",   "s")
#'     )
#' 
#' df
#' 
#' dissolve_hz(df, c("genhz", "texcl"))
#' dissolve_hz(df, c("genhz", "texcl"), collapse = TRUE)
#' 
#' test <- dissolve_hz(df, "genhz")
#' subset(test, value == "2Bt")
#' 


dissolve_hz <- function(object, by, id = "peiid", hztop = "hzdept", hzbot = "hzdepb", collapse = FALSE, order = FALSE) {
  
  # id = "peiid"; hztop = "hzdept"; hzbot = "hzdepb", collapse = FALSE, order = FALSE
  
  # test inputs ----
  # argument sanity check
  # test_spc <- inherits(object, 'SoilProfileCollection')
  
  # check that object & by are the right class
  test_object   <- inherits(object,   "data.frame")
  test_by <- inherits(by, "character")
  
  if (! any(test_object | test_by)) {
    stop("the object argument must be a data.frame, and by a character")
  }
  
  # check that by is not NULL
  if (is.null(by)) stop("the by argument must not be NULL")
  
  # check that collapse is a logical of length 1
  if (class(collapse) != "logical" & length(collapse) == 1) {
    stop("the collapse argument must be logical and a length of one")
  }
  
  # check that the column names exisit within the object
  var_names <- c(id = id, top = hztop, bot = hzbot, by)
  if (! all(var_names %in% names(object))) {
    stop("all arguments must match object names")
  }
  
  # check that "by" are characters or convert
  if (any(! "character" %in% sapply(object[by], class))) {
    message("non-character grouping variables are being converted to characters")
    object[by] <- lapply(object[by], as.character)
  }
  
  
  # standardize inputs ----
  df <- object
  idx_names <- sapply(var_names[1:3], function(x) which(names(df) == x))
  names(df)[idx_names] <- names(var_names)[1:3]
  
  # valid
  # vd_idx <- validate_depths(df, id = "id", hztop = "hzdept", bot = "hzdepb")
  if (order == TRUE) {
    df <- df[order(df$id, df$top, df$bot), ]
  }
  
  if (collapse == TRUE) {
    by_co <- paste(by, collapse = " & ")
    df[by_co] <- apply(df[by], 1, paste, collapse = " & ")
    by    <- by_co
  }
  
  
  # var thickness ----
  var_dep <- lapply(by, function(x) {
    
    con_bot <- rle(    paste(df$id, df[, x]))$length
    con_top <- rle(rev(paste(df$id, df[, x])))$length
    
    bot_idx <- cumsum(con_bot)
    top_idx <- cumsum(con_top)
    
    vd <- data.frame(
      id  = df[bot_idx, "id"],
      top = rev(rev(df$top)[top_idx]),
      bot = df[bot_idx, "bot"],
      variable = x,
      value    = df[bot_idx,    x]
    )
    
    return(vd)
  })
  var_dep <- do.call("rbind", var_dep)
  
  # undo standardization ----
  names(var_dep)[1:3] <- var_names[1:3]
  
  
  return(var_dep)
}
