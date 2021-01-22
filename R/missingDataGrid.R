# s: soil profile collection object
# s.fm: slicing formula, including variables requested for missing data test
# cols: vector of colors palette

#' Missing Data Grid
#'
#' Generate a levelplot of missing data from a SoilProfileCollection object.
#'
#' This function evaluates a `missing data fraction` based on slice-wise
#' evaulation of named variables in a \code{SoilProfileCollection} object.
#'
#' @param s a SoilProfileCollection object
#' @param max_depth integer specifying the max depth of analysis
#' @param vars character vector of column names over which to evaluate missing
#' data
#' @param filter.column a character string naming the column to apply the
#' filter REGEX to
#' @param filter.regex a character string with a regular expression used to
#' filter horizon data OUT of the analysis
#' @param cols a vector of colors
#' @param \dots additional arguments passed on to \code{levelplot}
#' @return A \code{data.frame} describing the percentage of missing data by
#' variable.
#' @note A lattice graphic is printed to the active output device.
#' @author D.E. Beaudette
#' @seealso \code{\link{slice}}
#' @keywords hplots
#' @examples
#'
#' # 10 random profiles
#' set.seed(10101)
#' s <- lapply(as.character(1:10), random_profile)
#' s <- do.call('rbind', s)
#'
#' # randomly sprinkle some missing data
#' s[sample(nrow(s), 5), 'p1'] <- NA
#' s[sample(nrow(s), 5), 'p2'] <- NA
#' s[sample(nrow(s), 5), 'p3'] <- NA
#'
#' # set all p4 and p5 attributes of `soil 1' to NA
#' s[which(s$id == '1'), 'p5'] <- NA
#' s[which(s$id == '1'), 'p4'] <- NA
#'
#' # upgrade to SPC
#' depths(s) <- id ~ top + bottom
#'
#' # plot missing data via slicing + levelplot
#' missingDataGrid(
#'   s,
#'   max_depth = 100,
#'   vars = c('p1', 'p2', 'p3', 'p4', 'p5'),
#'   main='Missing Data Fraction'
#' )
#'
missingDataGrid <- function(s, max_depth, vars, filter.column = NULL, filter.regex = NULL, cols = NULL, ...) {

  # default color scheme
  if(is.null(cols)) {
    cols <- c("#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
              "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F")
  }


  # make color pallete and define number of cuts
  cols.palette <- colorRampPalette(cols)
  ncuts <- 20

  ## TODO: use horizon designation from SPC if possible
  # optionally filter horizon data in original object and replace
  if(!is.null(filter.column) & !is.null(filter.regex)) {
    h <- horizons(s)
    idx <- grep(filter.regex, h[, filter.column], invert=TRUE)
    slot(s, 'horizons') <- h[idx, ]
    rm(h)
  }

  ## TODO: this is not efficient
  # get a list of horizon boundary depths for latter annotation of sliced data
  obd <- profileApply(s, simplify=FALSE, FUN=function(i) {
    hd <- horizonDepths(i)
    h <- horizons(i)
    hz.boundaries <- unique(c(h[[hd[1]]], h[[hd[2]]]))
  })


  # compute percent missing data by pedon/variable
  # this is only used as a summary and returned by function

  ## TODO: abstract this, useful in other contexts
  pct_missing <- profileApply(s, frameify = TRUE, FUN = function(i, v = vars) {
    h <- horizons(i)

    frac.missing <- sapply(
      h[, v, drop = FALSE], function(j) {
        length(which(is.na(j)))
      }) / nrow(h)

    res <- data.frame(
      .id = profile_id(i)[1],
      t(round(frac.missing * 100)),
      stringsAsFactors = FALSE
    )

    names(res)[1] <- idname(i)
    return(res)
  })



  # slice according to rules
  s.fm <- as.formula(paste('0:', max_depth, ' ~ ', paste(vars, collapse=' + '), sep=''))
  # setting strict = FALSE, no need for robust hz detection
  ss <- slice(s, s.fm, strict = FALSE)

  # get sliced horizon depth names
  hd <- horizonDepths(ss)

  # extract horizons from sliced data
  h <- horizons(ss)

  # get slice mid-points
  h$mid <- (h[[hd[1]]] + h[[hd[2]]]) / 2


  # NOTE: since we are converting profile IDs to a factor,
  # we need to explicitly set levels to match the original ordering of profiles
  forced.levels <- paste("c('", paste(profile_id(ss), collapse="','"), "')", sep='')

  # construct levelplot formula using horizon top boundaries
  f <- as.formula(paste('.pctMissing',  ' ~ ', 'factor(', idname(ss), ', levels=', forced.levels, ') * mid', sep=''))

  # ylab adjustments
  ylab <- paste('Depth ', '(', depth_units(ss), ')', sep='')

  # depth-range adjustments
  ylim <- c(max(h$mid) + 5, -3)

  # plot missing data fraction
  lp <- levelplot(f, data=h, ylim=ylim, col.regions=cols.palette(ncuts), cuts=ncuts-1, ylab=ylab, xlab='', scales=list(x=list(rot=90), y=list(tick.number=10)), ..., panel=function(...) {
    panel.levelplot(...)
    panel.abline(v=1:(length(ss)+1)-0.5)
    panel.grid(h=-1, v=FALSE, lty=2, col=grey(0.25))
    for(i in 1:length(obd)) {
      panel.segments(i-0.5, obd[[i]], i+0.5, obd[[i]], col='black')
    }
  })


  # return figure missing data percentages by pedon
  return(list(fig = lp, summary = pct_missing))

}
