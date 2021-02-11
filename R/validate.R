#' @title Check horizon depths for errors
#' 
#' @description This function inspects a series of horizon depths for common errors:
#' 
#'   1. bottom depth is shallower than the top depth
#'   2. equal top and bottom depths
#'   3. missing top or bottom depths (e.g. `NA`)
#'   4. gap or overlap between adjacent horizons
#'   
#' @param object data.frame or SoilProfileCollection object to check
#' 
#' @param id pedon id column name, only necessary if object is a data.frame. Default: `peiid`
#' 
#' @param top horizon top depth column name, only necessary if object is a data.frame. Default: `hzdept`
#' 
#' @param bot horizon bottom depth column name, only necessary if object is a data.frame. Default: `hzdepb`
#' 
#' @param append_checks logical indicating whether the validation check should be appended to the input object. Default: `TRUE`
#' 
#' @param pad_bot logical indicating whether to pad the bottom horizon with the top + 1-cm if the `top == bot` or the bot is `NA`, prior to checking for errors and applying `rmHzError`. Default: `FALSE`
#'
#' @param rmHzErrors logical indicating whether to remove pedons with horizon errors from the output. Default: `FALSE`
#' 
#' @details This function performs several validations listed above to check horizon depths for errors. Compared to \code{checkHzDepthLogic}, it is slightly faster, reports more intuitive results (e.g. TRUE = pass), reports the results at the horizon level rather than the pedon, appends the results to the horizon table, exports the results `aqp.env`, and incorporates options to pad bottom horizon depths with +1cm, and `rmHzErrors`. It will be useful to prune a lot of house cleaning code that occurs in `soilDB`.
#'
#' @return A `data.frame` or `SoilProfileCollection` appended with new `check_` columns appended if `append_check = TRUE`.
#' 
#'  - `valid_dep_order`: `TRUE` if `top < bot`
#'  - `valid_dep_diff`: `TRUE` if `top != bot`
#'  - `valid_dep_complete`: `TRUE` if `complete.cases(top, bot)`
#'  - `valid_dep_overlap`: `TRUE` if no gaps or overlaps in adjacent horizons
#'  - `valid_dep_all`: `TRUE` if all valid_ are `TRUE`
#' 
#' @export
#' @author S.M. Roecker, D.E. Beaudette, A.G. Brown
#' @examples
#' 
#' ## sample data
#' 
#' data(sp3)
#'
#' # these data should be clean
#' depths(sp3) <- id ~ top + bottom
#' sp3_val <- validate_depths(sp3)
#' 
#' head(sp3_val)
#' 
#' # sample data with errors
#' test <- data.frame(peiid  = 1:6, 
#'                    hzdept = c(0, 2, 10, 25, 50, 100), 
#'                    hzdepb = c(0, 11, 25, 50, 100, NA)
#'                    )
#' 
#' test_val <- validate_depths(test)
#' 
validate_depths <- function (object, id = "peiid", top = "hzdept", bot = "hzdepb", append_checks = TRUE, pad_bot = FALSE, rmHzErrors = FALSE) {
  
  # test inputs ----
  # argument sanity check
  test_spc <- inherits(object, 'SoilProfileCollection')
  test_df  <- inherits(object, 'data.frame')

  if (! any(test_spc, test_df)) {
    stop("the input must be either a SoilProfileCollection or data.frame")
  }
  
  # standardize object ----
  if (test_spc) {
    df   <- horizons(object)
    id   <- idname(object)
    hzid <- hzidname(object)
    top  <- horizonDepths(object)[1]
    bot  <- horizonDepths(object)[2]
  } else {
    df <- object
  }
  
  
  # more tests
  stopifnot(is.numeric(df[[top]]) & is.numeric(df[[bot]]))
  
  # vars <- c(id = "pedon_key", top = "hzn_top", bot = "hzn_bot")
  var_names <- c(id = id, top = top, bot = bot)
  if (! all(var_names %in% names(df))) {
    stop("all arguments must match df names")
  }
  
  
  # standardize df ----
  idx_names <- sapply(var_names, function(x) which(names(df) == x))
  names(df)[idx_names] <- names(var_names)
  
  df$rn   <- 1:nrow(df)
  # df$id   <- as.character(df$id)
  # df$top  <- as.integer(df$top)
  # df$bot  <- as.integer(df$bot)
  
  df <- df[order(df$id, df$top, df$bot), ]
  
  
  # pad bot where top == bottom ----
  valid_dep_diff <- as.integer(df$top) != as.integer(df$bot) # should be TRUE
  if (pad_bot == TRUE & any(!valid_dep_diff, na.rm = TRUE)) {
    
    df$bot <- ifelse(! valid_dep_diff, df$bot + 1, df$bot)
    
    message(paste('top/bottom depths equal, adding 1cm to bottom depth ... [', sum(valid_dep_diff, na.rm = TRUE), ' horizons]', sep = '')
    )
  }
  
  
  # pad bottom where bot is missing ----
  NA_dep_bottom <- !is.na(df$top) & is.na(df$bot) # should be TRUE
  if (pad_bot == TRUE & any(NA_dep_bottom, na.rm = TRUE)) {
    
    df$bot <- ifelse(NA_dep_bottom, df$top + 1, df$bot)
    
    message(paste('replacing missing lower horizon depths with top depth + 1cm ... [',
                  sum(NA_dep_bottom, na.rm = TRUE), ' horizons]', sep='')
    )
  }
  
  
  # check horizon depths ----
  df <- transform(df,
    valid_dep_order    = top < bot,                          # should be TRUE
    valid_dep_diff     = as.integer(top) != as.integer(bot), # should be TRUE
    valid_dep_complete = complete.cases(top, bot),           # should be TRUE
    NA_dep_bottom      = !is.na(top) & is.na(bot)            # should be TRUE
  )
  df$valid_dep_overlap <- 
    (as.integer(df$top) == as.integer(c(NA, df$bot[-nrow(df)]))) == 
    ifelse(as.character(df$id) == as.character(c(NA, df$id[-nrow(df)])), TRUE, NA) # should be TRUE
  df$valid_dep_all <- df$valid_dep_order & df$valid_dep_diff & df$valid_dep_complete & (df$valid_dep_overlap | is.na(df$valid_dep_overlap))
  
  
  # rmHzErrors ----
  if (rmHzErrors == TRUE & any(!df$valid_dep_all, na.rm = TRUE)) {
    
    ids      <- unique(df$id)
    bad_ids  <- unique(df$id[df$valid_dep_all == FALSE])
    good_ids <- ids[!ids %in% bad_ids] 
    
    message(paste("removing",  length(bad_ids), "pedons", "with", sum(!df$valid_dep_all, na.rm = TRUE), "horizon errors"))
    
    df <- df[df$id %in% good_ids, ]
    
    if (test_spc) {
      idx <- which(site(object)[[id]] %in% good_ids)
      object <- object[idx, ]
    }
  }
  
  
  # append_checks ----
  assign('bad.pedon.ids', 
         value = unique(df$id[!df$valid_dep_all]), 
         envir = aqp.env
  )
  assign("bad.horizons",  
         value = df[!df$valid_dep_all, c("id", "top", "bot")], 
         envir = aqp.env
  )
  assign('top.bottom.equal', 
         value = unique(df$id[!df$valid_dep_diff]), 
         envir = aqp.env
  )
  assign("missing.horizon.depths",
         value = unique(df$id[!df$valid_dep_complete]),
         envir = aqp.env
  )
  
  
  if (append_checks == FALSE) {
    
    df$valid_dep_order    <- NULL
    df$valid_dep_diff     <- NULL
    df$valid_dep_complete <- NULL
    df$valid_dep_overlap  <- NULL
    df$valid_dep_all      <- NULL
  }
  
  
  # undo standardization ----
  names(df)[idx_names] <- var_names
  
  df    <- df[order(df$rn), ]
  df$rn            <- NULL
  df$NA_dep_bottom <- NULL
  
  
  # rebuild SPC
  if (test_spc) {
    
    horizons(object) <- df[c(id, hzid, "valid_dep_order", "valid_dep_diff", "valid_dep_complete", "valid_dep_overlap", "valid_dep_all")]
  }
  
  # return output ----
  if (test_spc) {
    return(object) # TODO: this is protection from missing-data/ID offset
  } else {
    return(df)
  }
  
}
