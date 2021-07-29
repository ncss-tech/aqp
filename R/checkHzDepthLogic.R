#' Check a SoilProfileCollection object for errors in horizon depths.
#'
#' @description This function inspects a SoilProfileCollection object, looking for four common errors in horizon depths:
#'
#'   1. bottom depth shallower than top depth
#'   2. equal top and bottom depth
#'   3. missing top or bottom depth (e.g. `NA`)
#'   4. gap or overlap between adjacent horizons
#'
#' @param x `SoilProfileCollection` or `data.frame` object to check
#' @param hzdepths SoilProfileCollection uses `horizonDepths(x)` Default: `NULL`; if `x` is a data.frame, character vector of column names of top and bottom depths
#' @param idname SoilProfileCollection uses `idname(x)` Default: `NULL`; if `x` is a data.frame, character vector with column name of unique profile ID;
#' @param fast If details about specific test results are not needed, the operation can allocate less memory and run approximately 5x faster. Default: `FALSE`
#' @param byhz Apply logic tests to profiles or individual horizons?
#'
#' @return A `data.frame` containing profile IDs, validity boolean (`valid`) and test results if `fast = FALSE`.
#'
#' The `data.frame` will have as many rows as profiles in `x` (`length(x)`).
#'
#'  - `id` : Profile IDs, named according to `idname(x)`
#'  - `valid` : boolean, profile passes all of the following tests
#'    - `depthLogic` : boolean, errors related to depth logic
#'    - `sameDepth` : boolean, errors related to same top/bottom depths
#'    - `missingDepth` : boolean, NA in top / bottom depths
#'    - `overlapOrGap` : boolean, gaps or overlap in adjacent horizons
#'
#' @export
#' @author D.E. Beaudette, A.G. Brown, S.M. Roecker
#' @examples
#'
#' ## sample data
#'
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#'
#' # these data should be clean
#' res <- checkHzDepthLogic(sp3)
#'
#' head(res)
#'
#' # less memory if only concerned about net validity
#' res <- checkHzDepthLogic(sp3, fast = TRUE)
#'
#' head(res)
#'
checkHzDepthLogic <- function(x,
                              hzdepths = NULL,
                              idname = NULL,
                              fast = FALSE,
                              byhz = FALSE) {

  stopifnot(inherits(x, 'SoilProfileCollection') |
              inherits(x, 'data.frame'))

  if (inherits(x, 'SoilProfileCollection')) {
    h <- data.table::as.data.table(horizons(x))
    hzd <- horizonDepths(x)
    idn <- idname(x)
  } else {
    h <- data.table::as.data.table(x)

    # must have horizon top and bottom depth column
    stopifnot(length(hzdepths) == 2 &
                is.character(hzdepths) &
                all(hzdepths %in% colnames(h)))
    hzd <- hzdepths

    # must have id name column
    stopifnot(length(idname) == 1 &
                is.character(idname) &
                all(idname %in% colnames(h)))
    idn <- idname
  }

  hby <- substitute(idn)
  res <- NULL

  # data.table R CMD check
  tests <- NULL

  if (!fast) {
    if (!byhz) {
      res <- h[, list(tests = list(tests = data.frame(t(hzDepthTests(.SD))))), .SDcols = hzd, by = c(eval(hby))][,
                 list(tests = tests, valid = all(!tests[[1]])), by = c(eval(hby))]
    } else {
      res <- h[, list(tests = list(tests = data.frame(t(hzDepthTests(.SD))))), .SDcols = hzd, by = list(.hzID = seq_len(nrow(h)))][,
                 list(tests = tests, valid = all(!tests[[1]])), by = list(.hzID = seq_len(nrow(h)))]
      if (inherits(x, 'SoilProfileCollection')) {
        res[[hzidname(x)]] <- hzID(x)
      } else {
        res$hzID <- res$.hzID
      }
      res$.hzID <- NULL
    }
    res <- cbind(res, data.table::rbindlist(res$tests))
    res$tests <- NULL

  } else {

    if (!byhz) {
      res <- h[, all(!hzDepthTests(.SD)), .SDcols = hzd, by = c(eval(hby))]
      colnames(res) <- c(idn, "valid")
    } else {
      res <- h[, all(!hzDepthTests(.SD)), .SDcols = hzd, by = list(.hzID = seq_len(nrow(h)))]
      if (inherits(x, 'SoilProfileCollection')) {
        res[[hzidname(x)]] <- hzID(x)
        colnames(res) <- c(".hzID", "valid", hzidname(x))
      } else {
        res$hzID <- res$.hzID
        colnames(res) <- c(".hzID", "valid", "hzID")
      }
      res$.hzID <- NULL
    }
  }
  # add profile ID and top/bottom depth for byhz==TRUE
  if (byhz) {
    res <- cbind(h[, .SD, .SDcols = c(idn, hzd)], res)
  }
  return(as.data.frame(res))

  #
  # # used inside / outside of scope of .check()
  # htb <- horizonDepths(x)
  # idn <- idname(x)
  #
  # .check <- function(i) {
  #   # extract pieces
  #   h <- horizons(i)
  #
  #   # convenience vars
  #   ID.i <- h[[idn]][1]
  #   .top <- h[[htb[1]]]
  #   .bottom <- h[[htb[2]]]
  #
  #   # hzTests takes two numeric vectors and returns named logical
  #   test <- hzDepthTests(.top, .bottom)
  #
  #   # pack into DF, 1 row per profile
  #   res <- data.frame(
  #     .id=ID.i,
  #     depthLogic=test[1],
  #     sameDepth=test[2],
  #     missingDepth=test[3],
  #     overlapOrGap=test[4],
  #     stringsAsFactors = FALSE
  #   )
  #
  #   # re-name .id -> idname(x)
  #   names(res)[1] <- idn
  #
  #   return(res)
  # }
  #
  # # iterate over profiles, result is safely packed into a DF ready for splicing into @site
  # res <- profileApply(x, .check, simplify = FALSE, frameify = TRUE)
  #
  # # add 'valid' flag for simple filtering
  # res[['valid']] <- !apply(res[, -1], 1, any)
  #
  # return(res)
}

#' @title Tests of horizon depth logic
#'
#' @description Function used internally by `checkHzDepthLogic()`, `glom()` and various other functions that operate on horizon data from single soil profiles and require a priori depth logic checks. Checks for bottom depths less than top depth / bad top depth order ("depthLogic"), bottom depths equal to top depth ("sameDepth"), overlaps/gaps ("overlapOrGap") and missing depths ("missingDepth"). Use `names(res)[res]` on result `res` of `hzDepthTest()` to to determine type of logic error(s) found -- see examples below.
#'
#' @param top A numeric vector containing horizon top depths. Or a `data.frame` with two columns (first containing top depths, second containing bottom)
#' @param bottom A numeric vector containing horizon bottom depths.
#'
#' @return A named logical vector containing TRUE for each type of horizon logic error found in the given data.
#' @author Andrew G. Brown & Dylan E. Beaudette
#' @examples
#'
#' # no logic errors
#' res <- hzDepthTests(top = c(0,10,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#'
#' # bottom < top
#' hzDepthTests(top = c(10,20,30,50), bottom = c(0,10,20,30))
#' names(res)[res]
#'
#' # bottom == top
#' hzDepthTests(top = c(10,20,30,50), bottom = c(0,20,20,30))
#' names(res)[res]
#'
#' # overlap
#' hzDepthTests(top = c(0,5,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#'
#' # gap
#' hzDepthTests(top = c(0,15,20,30), bottom = c(10,20,30,50))
#' names(res)[res]
#'
#' # missing
#' hzDepthTests(c(0,15,NA,30),c(10,NA,30,50))
#' names(res)[res]
#'
#' @rdname hzDepthTests
#' @export hzDepthTests
hzDepthTests <- function(top, bottom = NULL) {

  if(inherits(top, 'data.frame') && ncol(top) >= 2) {
    bottom <- top[[2]]
    top <- top[[1]]
  }

  top <- as.numeric(top)
  bottom <- as.numeric(bottom)

  stopifnot(is.numeric(top) && is.numeric(bottom))

  n <- length(top)

  # sanity checks, since this will be exported provide a little checking
  #   for most internal usesF these errors will never trigger...
  # but in case of corrupted hz data or bad inputs... anything can happen
  if (length(top) != length(bottom)) {
    stop("cannot evaluate horizon depth logic: vectors do not have same length")
  }

  # bottom depth < top depth? or horizons not in top-depth order?
  test.1 <- any(bottom < top, na.rm = TRUE) | any(suppressWarnings(sort(top) != top))

  if (is.na(test.1)) {
    # test.1 is NA if test.3 is true for both top and bottom depth
    test.1 <- TRUE
  }

  # bottom depth == top depth
  test.2 <- any(top == bottom, na.rm = TRUE)

  # NA depths
  test.3 <- any(is.na(top) | is.na(bottom), na.rm = TRUE)

  # bottom != next top
  test.4 <- any(bottom[-n] != top[-1], na.rm = TRUE)

  res <- as.logical(c(test.1, test.2, test.3, test.4))

  names(res) <- c("depthLogic","sameDepth","missingDepth","overlapOrGap")
  return(res)
}
