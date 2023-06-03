## 2019-07-16: moved util functions to `sketch-utils.R`

# TODO: behavior not defined for horizons with an indefinite lower boundary
# TODO: move some of the processing outside of the main loop: column names, etc.

#' @title Create Soil Profile Sketches
#' @name plotSPC
#' @rdname SoilProfileCollection-plotting-methods
#' @docType methods
#' @aliases plot,SoilProfileCollection,ANY-method,plot.SoilProfileCollection
#'
#' @description Generate a diagram of soil profile sketches from a `SoilProfileCollection` object. The [Introduction to SoilProfileCollection Objects Vignette](http://ncss-tech.github.io/aqp/articles/Introduction-to-SoilProfileCollection-Objects.html) contains many examples and discussion of the large number of arguments to this function. The [Soil Profile Sketches](https://ncss-tech.github.io/AQP/aqp/sketches.html) tutorial has longer-form discussion and examples pertaining to suites of related arguments.
#'
#' Options can be used to conveniently specify sets of arguments that will be used in several calls to `plotSPC()` within a single R session. For example, arguments can be specified in a named list (`.a`) and set using: `options(.aqp.plotSPC.args = .a)`. Reset these options via `options(.aqp.plotSPC.args = NULL)`. Arguments explicitly passed to `plotSPC()` will override arguments set via [options()].
#'
#'
#' @param x a `SoilProfileCollection` object
#'
#' @param color quoted column name containing R-compatible color descriptions, or numeric / categorical data to be displayed thematically; see details
#'
#' @param width scaling of profile widths (typically 0.1 - 0.4)
#'
#' @param name quoted column name of the (horizon-level) attribute containing horizon designations or labels, if missing `hzdesgnname(x)` is used. Suppress horizon name printing by setting `name = NA` or `name = ''`.
#'
#' @param name.style one of several possible horizon designations labeling styles: `c('right-center', 'left-center', 'left-top', 'center-center', 'center-top')`
#'
#' @param label quoted column name of the (site-level) attribute used to identify profile sketches
#' 
#' @param raggedBottom quoted column name of the (site-level) attribute (logical) used to mark profiles with a truncated lower boundary
#'
#' @param hz.depths logical, annotate horizon top depths to the right of each sketch (`FALSE`)
#' 
#' @param hz.depths.offset numeric, user coordinates for left-right adjustment for horizon depth annotation; reasonable values are usually within 0.01-0.05 (default: 0)
#' 
#' @param hz.depths.lines logical, draw segments between horizon depth labels and actual horizon depth; this is useful when including horizon boundary distinctness and/or `fixLabelCollisions = TRUE`
#'
#' @param alt.label quoted column name of the (site-level) attribute used for secondary annotation
#'
#' @param alt.label.col color used for secondary annotation text
#'
#' @param cex.names baseline character scaling applied to all text labels
#'
#' @param cex.depth.axis character scaling applied to depth scale
#'
#' @param cex.id character scaling applied to `label`
#'
#' @param font.id font style applied to `label`, default is 2 (bold)
#' 
#' @param srt.id rotation applied to `label`, only when `id.style = 'top'`
#'
#' @param print.id logical, print `label` above/beside each profile? (`TRUE`)
#'
#' @param id.style `label` printing style: 'auto' (default) = simple heuristic used to select from: 'top' = centered above each profile, 'side' = 'along the top-left edge of profiles'
#'
#' @param plot.order integer vector describing the order in which individual soil profiles should be plotted
#'
#' @param relative.pos vector of relative positions along the x-axis, within \{1, n\}, ignores `plot.order` see details
#'
#' @param add logical, add to an existing figure
#'
#' @param scaling.factor vertical scaling of profile depths, useful for adding profiles to an existing figure
#'
#' @param y.offset numeric vector of vertical offset for top of profiles in depth units of `x`, can either be a single numeric value or vector of length = `length(x)`. A vector of y-offsets will be automatically re-ordered according to `plot.order`.
#'
#' @param x.idx.offset integer specifying horizontal offset from 0 (left-hand edge)
#'
#' @param n integer describing amount of space along x-axis to allocate, defaults to `length(x)`
#'
#' @param max.depth suggested lower depth boundary of plot, profiles are also truncated at this depth
#'
#' @param n.depth.ticks suggested number of ticks in depth scale
#'
#' @param shrink logical, reduce character scaling for 'long' horizon by 80%
#'
#' @param shrink.cutoff character length defining 'long' horizon names
#' 
#' @param shrink.thin integer, horizon thickness threshold for shrinking horizon names by 80%, only activated when `shrink = TRUE` (`NULL` = no shrinkage)
#'
#' @param abbr logical, abbreviate `label`
#'
#' @param abbr.cutoff suggested minimum length for abbreviated `label`
#'
#' @param divide.hz logical, divide horizons with line segment? (`TRUE`), see details
#'
#' @param hz.distinctness.offset `NULL`, or quoted column name (horizon-level attribute) containing vertical offsets used to depict horizon boundary distinctness (same units as profiles), see details and [hzDistinctnessCodeToOffset()]; consider setting `hz.depths.lines = TRUE` when used in conjunction with `hz.depths = TRUE`
#' 
#' @param hz.topography.offset `NULL`, or quoted column name (horizon-level attribute) containing offsets used to depict horizon boundary topography (same units as profiles), see details and [hzTopographyCodeToOffset()]
#'
#' @param hz.boundary.lty quoted column name (horizon-level attribute) containing line style (integers) used to encode horizon topography
#'
#' @param axis.line.offset horizontal offset applied to depth axis (default is -2.5, larger numbers move the axis to the right)
#'
#' @param plot.depth.axis logical, plot depth axis? (default is `TRUE`)
#'
#' @param density fill density used for horizon color shading, either a single integer or a quoted column name (horizon-level attribute) containing integer values (default is `NULL`, no shading)
#'
#' @param show.legend logical, show legend? (default is `TRUE`)
#'
#' @param col.label thematic legend title
#'
#' @param col.palette color palette used for thematic sketches (default is `rev(brewer.pal(10, 'Spectral'))`)
#'
#' @param col.palette.bias color ramp bias (skew), see [colorRamp()]
#'
#' @param col.legend.cex scaling of thematic legend
#'
#' @param n.legend approximate number of classes used in numeric legend, max number of items per row in categorical legend
#'
#' @param lwd line width multiplier used for sketches
#'
#' @param lty line style used for sketches
#'
#' @param default.color default horizon fill color used when `color` attribute is `NA`
#' 
#' @param fixLabelCollisions use [fixOverlap()] to attempt fixing hz depth labeling collisions, will slow plotting of large collections; enabling fixes also sets `hz.depths.lines = TRUE`. Additional arguments to [fixOverlap()] can be passed as options.
#' 
#' @param fixOverlapArgs a named list of arguments to [fixOverlap()]. Overlap adjustments are attempted using electrostatic simulation with arguments: `list(method = 'E', q = 1)`. Alternatively, select adjustment by simulated annealing via `list(method = 'S')`. See [electroStatics_1D()] and [SANN_1D()] for details.
#'
#' @param \dots other arguments passed into lower level plotting functions
#'
#'
#' @details
#' Depth limits (`max.depth`) and number of depth ticks (`n.depth.ticks`) are *suggestions* to the [pretty()] function. You may have to tinker with both parameters to get what you want.
#'
#' The 'side' `id.style` is useful when plotting a large collection of profiles, and/or, when profile IDs are long.
#'
#' If the column containing horizon designations is not specified (the `name` argument), a column (presumed to contain horizon designation labels) is guessed based on regular expression matching of the pattern 'name'--this usually works, but it is best to manual specify the name of the column containing horizon designations.
#'
#' The `color` argument can either name a column containing R-compatible colors, possibly created via [munsell2rgb()], or column containing either numeric or categorical (either factor or character) values. In the second case, values are converted into colors and displayed along with a simple legend above the plot. Note that this functionality makes several assumptions about plot geometry and is most useful in an interactive setting.
#'
#' Adjustments to the legend can be specified via `col.label` (legend title), `col.palette` (palette of colors, automatically expanded), `col.legend.cex` (legend scaling), and `n.legend` (approximate number of classes for numeric variables, or, maximum number of legend items per row for categorical variables). Currently, `plotSPC` will only generate two rows of legend items. Consider reducing the number of classes if two rows isn't enough room.
#'
#' Profile sketches can be added according to relative positions along the x-axis (vs. integer sequence) via `relative.pos` argument. This should be a vector of positions within \{1,n\} that are used for horizontal placement. Default values are `1:length(x)`. Care must be taken when both `plot.order` and `relative.pos` are used simultaneously: `relative.pos` specifies horizontal placement after sorting. [addDiagnosticBracket()] and [addVolumeFraction()] use the `relative.pos` values for subsequent annotation.
#'
#' Relative positions that are too close will result in overplotting of sketches. Adjustments to relative positions such that overlap is minimized can be performed with `fixOverlap(pos)`, where `pos` is the original vector of relative positions.
#'
#' The `x.idx.offset` argument can be used to shift a collection of pedons from left to right in the figure. This can be useful when plotting several different `SoilProfileCollection` objects within the same figure. Space must be pre-allocated in the first plotting call, with an offset specified in the second call. See examples below.
#' 
#' Horizon depths (e.g. cm) are converted to figure y-coordinates via: y = (depth * scaling.factor) + y.offset. 
#'
#'
#' @note A new plot of soil profiles is generated, or optionally added to an existing plot.
#'
#' @author D.E. Beaudette
#'
#' @references Beaudette, D.E., Roudier P., and A.T. O'Geen. 2013. Algorithms for Quantitative Pedology: A Toolkit for
#' Soil Scientists. Computers & Geosciences. 52:258 - 268.
#'
#' @keywords hplots
#' @export
#'
#' @seealso [fixOverlap()], [explainPlotSPC()], [SoilProfileCollection-class], [pretty()], [hzDistinctnessCodeToOffset()], [addBracket()], [profileGroupLabels()]
#'
#' @examples
#'
#' # example data
#' data(sp1)
#' # usually best to adjust margins
#' par(mar = c(0,0,3,0))
#'
#' # add color vector
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' 
#' # init horizon designation 
#' hzdesgnname(sp1) <- 'name'
#' 
#' # plot profiles
#' plotSPC(sp1, id.style = 'side')
#'
#' # title, note line argument:
#' title('Sample Data 1', line = 1, cex.main = 0.75)
#'
#' # plot profiles without horizon-line divisions
#' plotSPC(sp1, divide.hz = FALSE)
#'
#' # diagonal lines encode horizon boundary distinctness
#' sp1$hzD <- hzDistinctnessCodeToOffset(sp1$bound_distinct)
#' plotSPC(sp1, hz.distinctness.offset = 'hzD', name.style = 'center-center')
#'
#' # plot horizon color according to some property
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' hzdesgnname(sp4) <- 'name'
#' plotSPC(sp4, color = 'clay')
#'
#' # another example
#' data(sp2)
#' depths(sp2) <- id ~ top + bottom
#' hzdesgnname(sp2) <- 'name'
#' site(sp2) <- ~ surface
#'
#' # some of these profiles are very deep, truncate plot at 400cm
#' # label / re-order with site-level attribute: `surface`
#' plotSPC(sp2, label = 'surface', plot.order = order(sp2$surface), 
#' max.depth = 400)
#'
#' # example using a categorical attribute
#' plotSPC(sp2, color = "plasticity", 
#' max.depth = 400)
#'
#' # plot two SPC objects in the same figure
#' par(mar = c(1,1,1,1))
#' 
#' # plot the first SPC object and
#' # allocate space for the second SPC object
#' plotSPC(sp1, n = length(sp1) + length(sp2))
#' 
#' # plot the second SPC, starting from the first empty space
#' plotSPC(sp2, x.idx.offset = length(sp1), add = TRUE)
#'
#'
#' ##
#' ## demonstrate horizon designation shrinkage
#' ##
#' 
#' data("jacobs2000")
#' 
#' # shrink "long" horizon names
#' plotSPC(
#'   jacobs2000, 
#'   name = 'name',
#'   name.style = 'center-center', 
#'   shrink = TRUE, 
#'   cex.names = 0.8
#' )
#' 
#' # shrink horizon names in "thin" horizons
#' plotSPC(
#'   jacobs2000, 
#'   name = 'name',
#'   name.style = 'center-center', 
#'   shrink = TRUE, 
#'   shrink.thin = 15,
#'   cex.names = 0.8,
#' )
#' 
#'
#' ##
#' ## demonstrate adaptive legend
#' ##
#'
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#'
#' # make some fake categorical data
#' horizons(sp3)$fake.data <- sample(letters[1:15], size = nrow(sp3), replace=TRUE)
#'
#' # better margins
#' par(mar=c(0,0,3,1))
#'
#' # note that there are enough colors for 15 classes (vs. previous limit of 10)
#' # note that the legend is split into 2 rows when length(classes) > n.legend argument
#' plotSPC(sp3, color='fake.data', name='fake.data', cex.names=0.8)
#'
#' # make enough room in a single legend row
#' plotSPC(sp3, color='fake.data', name='fake.data', cex.names=0.8, n.legend=15)
#' 
#' 
#' ##
#' ## demonstrate y.offset argument
#' ## must be of length 1 or length(x)
#' ##
#' 
#' # example data and local copy
#' data("jacobs2000")
#' x <- jacobs2000
#' hzdesgnname(x) <- 'name'
#' 
#' # y-axis offsets, simulating a elevation along a hillslope sequence
#' # same units as horizon depths in `x`
#' # same order as profiles in `x`
#' y.offset <- c(-5, -10, 22, 65, 35, 15, 12)
#' 
#' par(mar = c(0, 0, 2, 2))
#' 
#' # y-offset at 0
#' plotSPC(x, color = 'matrix_color', cex.names = 0.66)
#' 
#' # constant adjustment to y-offset
#' plotSPC(x, color = 'matrix_color', cex.names = 0.66, y.offset = 50)
#' 
#' # attempt using invalid y.offset
#' # warning issued and default value of '0' used
#' # plotSPC(x, color = 'matrix_color', cex.names = 0.66, y.offset = 1:2)
#' 
#' # variable y-offset
#' # fix overlapping horizon depth labels
#' par(mar = c(0, 0, 1, 0))
#' plotSPC(
#'   x, 
#'   y.offset = y.offset, 
#'   color = 'matrix_color', 
#'   cex.names = 0.75, 
#'   shrink = TRUE,
#'   hz.depths = TRUE, 
#'   hz.depths.offset = 0.05,
#'   fixLabelCollisions = TRUE,
#'   name.style = 'center-center'
#' )
#' 
#' 
#' 
#' # random y-axis offsets
#' yoff <- runif(n = length(x), min = 1, max = 100)
#' 
#' # random gradient of x-positions
#' xoff <- runif(n = length(x), min = 1, max = length(x))
#' 
#' # note profiles overlap
#' plotSPC(x, 
#'         relative.pos = xoff, 
#'         y.offset = yoff, 
#'         color = 'matrix_color', 
#'         cex.names = 0.66, 
#'         hz.depths = TRUE, 
#'         name.style = 'center-center'
#' )
#' 
#' 
#' # align / adjust relative x positions
#' set.seed(111)
#' pos <- alignTransect(xoff, x.min = 1, x.max = length(x), thresh = 0.65)
#' 
#' # y-offset is automatically re-ordered according to
#' # plot.order
#' 
#' par(mar = c(0.5, 0.5, 0.5, 0.5))
#' plotSPC(x, 
#'         plot.order = pos$order, 
#'         relative.pos = pos$relative.pos, 
#'         y.offset = yoff, 
#'         color = 'matrix_color', 
#'         cex.names = 0.66, 
#'         hz.depths = TRUE, 
#'         name.style = 'center-center'
#' )
#' 
#' box()
#' 
plotSPC <- function(
    x,
    color = 'soil_color',
    width = ifelse(length(x) < 2, 0.15, 0.25),
    name = hzdesgnname(x),
    name.style = 'right-center',
    label = idname(x),
    raggedBottom = NULL,
    hz.depths = FALSE,
    hz.depths.offset = ifelse(fixLabelCollisions, 0.03, 0),
    hz.depths.lines = fixLabelCollisions,
    alt.label = NULL,
    alt.label.col = 'black',
    cex.names = 0.5,
    cex.depth.axis = cex.names,
    cex.id = cex.names + (0.2 * cex.names),
    font.id = 2,
    srt.id = 0, 
    print.id = TRUE,
    id.style = 'auto',
    plot.order = 1:length(x),
    relative.pos = 1:length(x),
    add = FALSE,
    scaling.factor = 1,
    y.offset = rep(0, times = length(x)),
    x.idx.offset = 0,
    n = length(x),
    max.depth = ifelse(is.infinite(max(x)), 200, max(x)),
    n.depth.ticks = 5,
    shrink = FALSE,
    shrink.cutoff = 3,
    shrink.thin = NULL,
    abbr = FALSE,
    abbr.cutoff = 5,
    divide.hz = TRUE,
    hz.distinctness.offset = NULL,
    hz.topography.offset = NULL,
    hz.boundary.lty = NULL,
    axis.line.offset = -2.5,
    plot.depth.axis = TRUE,
    density = NULL,
    show.legend = TRUE,
    col.label = color,
    col.palette = c("#5E4FA2", "#3288BD", "#66C2A5","#ABDDA4", "#E6F598",
                             "#FEE08B","#FDAE61", "#F46D43", "#D53E4F","#9E0142"),
                             col.palette.bias = 1,
    col.legend.cex = 1,
    n.legend = 8,
    lwd = 1,
    lty = 1,
    default.color = grey(0.95),
    fixLabelCollisions = FALSE,
    fixOverlapArgs = list(method = 'E', q = 1),
    ...
) {
  
  
  ############################
  ## make R CMD check happy ##
  ############################
  .LAST <- NULL
  .BOTTOM <- NULL
  
  
  ############################
  ## arguments from options ##
  ############################
  
  # specified as: options(.aqp.plotSPC.args = list())
  
  # get any options set, result is a list
  # NULL otherwise
  .opArgs <- getOption(".aqp.plotSPC.args", default = NULL)
  
  # process options
  if(!is.null(.opArgs)) {
    
    # e.g. some of: names(formals('plotSPC'))
    # not all arguments can be set via options
    .argSet <- c("color", "width", "name", "name.style", "label", "raggedBottom", "hz.depths", 
                 "hz.depths.offset", "hz.depths.lines", "alt.label", "alt.label.col", 
                 "cex.names", "cex.depth.axis", "cex.id", "font.id", "srt.id", 
                 "print.id", "id.style", "plot.order", "relative.pos", "add", 
                 "scaling.factor", "y.offset", "x.idx.offset", "n", "max.depth", 
                 "n.depth.ticks", "shrink", "shrink.cutoff", "shrink.thin", "abbr", 
                 "abbr.cutoff", "divide.hz", "hz.distinctness.offset", "hz.topography.offset", 
                 "hz.boundary.lty", "axis.line.offset", "plot.depth.axis", "density", 
                 "show.legend", "col.label", "col.palette", "col.palette.bias", 
                 "col.legend.cex", "n.legend", "lwd", "lty", "default.color", 
                 "fixLabelCollisions", "fixOverlapArgs"
    )
    
    # iterate over all possible arguments that can be modified in this way
    for(.arg in .argSet) {
      
      # test to see if the current argument has been explicitly provided
      .missing <- do.call(missing, list(.arg))
      
      # consider only:
      # specified via options AND NOT (XOR) given by function arguments
      if(!is.null(.opArgs[[.arg]]) && .missing) {
        # debug
        # print(sprintf("using options: %s", .arg))
        # set to local variable in place of default argument
        assign(.arg, .opArgs[[.arg]])
      }
    }
    
  }
  
  
  ###################
  ## sanity checks ##
  ###################
  
  # `x` should be a SoilProfileCollection
  if (!inherits(x, 'SoilProfileCollection')) {
    stop("Object `x` must be a SoilProfileCollection", call. = FALSE) 
  }
  
  # don't plot an empty collection
  if (nrow(x) == 0) {
    stop("SoilProfileCollection `x` contains no horizon data", call. = FALSE)
  }
  
  # horizon name style
  if(! name.style %in% c('right-center', 'left-center', 'left-top', 'center-center', 'center-top')) {
    warning('invalid `name.style`', call. = FALSE)
    name.style <- 'right-center'
  }
  
  # horizon distinctness offset column name must be valid
  if(!missing(hz.distinctness.offset)) {
    # valid name
    if(! hz.distinctness.offset %in% horizonNames(x))
      stop('invalid `hz.distinctness.offset` column name', call. = FALSE)
    
    # must be numeric
    if(! is.numeric(x[[hz.distinctness.offset]]))
      stop('`hz.distinctness.offset` must be numeric', call. = FALSE)
    
  } else {
    # no horizon distinctness offset specified, use 0
    hz.distinctness.offset <- '.hdo'
    horizons(x)[[hz.distinctness.offset]] <- 0
  }
  
  # horizon topography offset column name must be valid
  if(!missing(hz.topography.offset)) {
    # valid name
    if(! hz.topography.offset %in% horizonNames(x))
      stop('invalid `hz.topography.offset` column name', call. = FALSE)
    
    # must be numeric
    if(! is.numeric(x[[hz.topography.offset]]))
      stop('`hz.topography.offset` must be numeric', call. = FALSE)
    
  } else {
    # no horizon topography offset specified, use 0
    hz.topography.offset <- '.hto'
    horizons(x)[[hz.topography.offset]] <- 0
  }
  
  # horizon line type
  if(!missing(hz.boundary.lty)) {
    # valid name
    if(! hz.boundary.lty %in% horizonNames(x))
      stop('invalid `hz.boundary.lty` column name', call. = FALSE)
    
    # must be numeric
    if(! is.numeric(x[[hz.boundary.lty]]))
      stop('`hz.boundary.lty` must be numeric', call. = FALSE)
  }
  
  # ragged bottom flag
  if(!missing(raggedBottom)) {
    # valid name
    if(! raggedBottom %in% siteNames(x))
      stop('invalid `raggedBottom` column name', call. = FALSE)
    
    # must be logical
    if(! is.logical(x[[raggedBottom]]))
      stop('`raggedBottom` must be logical', call. = FALSE)
  }
  
  # length of y.offset must length(x) or 1
  if(length(y.offset) == 1) {
    y.offset <- rep(y.offset, times = length(x))
  }
  
  if(length(y.offset) != length(x)) {
    warning('length of `y.offset` must be `length(x)` or 1, using `0`', call. = FALSE)
    y.offset <- rep(0, times = length(x))
  }
  
  # fixOverlap arguments
  if(is.null(fixOverlapArgs)) {
    fixOverlapArgs <- list()
  }
  
  
  #################################
  ## SPC truncation and flagging ##
  #################################
  
  # keep track of truncation by max.depth via special site-level attr
  site(x)$.isTruncated <- FALSE
  
  if(!missing(max.depth)) {
    
    # keep track of original profile bottom depths
    .pb0 <- x[, , .LAST, .BOTTOM]
    
    # truncate
    x <- trunc(x, 0, max.depth)
    
    # truncated bottom depths
    .pb1 <- x[, , .LAST, .BOTTOM]
    
    # flag
    x$.isTruncated[which(.pb0 != .pb1)] <- TRUE
  }
  
  # accommodate other flag specified in raggedBottom
  if(!missing(raggedBottom)) {
    truncation_flag <- x[['.isTruncated']] | x[[raggedBottom]]
  } else {
    truncation_flag <- x[['.isTruncated']]
  }
  
  # ragged line characteristics depend on vertical scale
  .raggedOffsets <- scaling.factor * max.depth * c(-0.01,  0.03) / 2
  
  
  ###################
  ## SPC cleanup ? ##
  ###################
  
  # tempting, but I don't like the auto-magical nature of this
  # easy to miss important horizon logic errors
  # creates the possibility for new errors based on unexpected results
  # x <- repairMissingHzDepths(x)
  
  
  ###################
  ## fudge factors ##
  ###################
  
  # TODO: base calculations on strwidth() AFTER plot() has been called
  
  # padding along x-axis, prevents crowding
  # dynamic adjustment must also taking into account figure size
  # roughly 10% of length(x)
  extra_x_space <- n * 0.1
  
  # multiplier (width * x_left_spac_mult) used to set left-side space along x-axis
  x_left_space_mult <- 2
  
  # add a little extra x-space when n <= 5
  if(n <= 5 & n > 2) {
    extra_x_space <- extra_x_space + 0.25
    x_left_space_mult <- 2
    
  }
  
  # special cases
  if(n == 2) {
    extra_x_space <- extra_x_space + 0.5
    x_left_space_mult <- 2.75
    
  }
  
  # special cases
  if (n == 1) {
    extra_x_space <- extra_x_space + 0.5
    x_left_space_mult <- 3.5
  }
  
  # padding above profiles, ~ 15 is about right for n in {1,25} and max depth near 150cm
  # a sketch of shalllow profiles could benefit from ~ 5
  if(max.depth <= 50)
    extra_y_space <- 5
  if(max.depth > 50 & max.depth <= 100)
    extra_y_space <- 10
  if(max.depth > 100)
    extra_y_space <- 15
  
  # get profile IDs
  pIDs <- profile_id(x)
  
  # re-order y-offset according to plot.order
  y.offset <- y.offset[plot.order]
  
  # get horizons
  h <- horizons(x)
  
  # get column names from horizon data
  nm <- names(h)
  
  
  #########################
  ## horizon designation ##
  #########################
  
  # hzdesgnname() does most of the work here, with sensible defaults when metadata are missing
  
  # code can safely run with all input except NULL
  if (is.null(name)) {
    # set to empty string
    name <- ''
  }
  
  
  
  ####################
  ## horizon colors ##
  ####################
  
  # .interpretHorizonColors() expects numeric | categorical data
  # convert logical to factor
  if (is.logical(h[[color]])) {
    h[[color]] <- factor(h[[color]])
  }
  
  # results contain:
  # vector of horizon colors, row-order preserved
  # legend data if relevant, otherwise NULL
  hz.color.interpretation <- .interpretHorizonColor(h, color, default.color, col.palette, col.palette.bias, n.legend)
  h[['.color']] <- hz.color.interpretation$colors
  
  # sketch parameters for follow-up overlay / inspection
  lsp <- list('width' = width,
              'plot.order' = plot.order,
              'x0' = relative.pos + x.idx.offset,
              'pIDs' = pIDs[plot.order],
              'idname' = idname(x),
              'y.offset' = y.offset,
              'scaling.factor' = scaling.factor,
              'max.depth' = max.depth,
              'n' = n,
              'extra_x_space' = extra_x_space,
              'extra_y_space' = extra_y_space,
              'hz.depth.LAI' = rep(NA_real_, n),
              'legend.colors' = hz.color.interpretation$colors,
              'legend.data' = hz.color.interpretation$color.legend.data
  )
  
  
  ####################
  ## horizon depths ##
  ####################
  
  # get top/bottom column names
  IDcol <- idname(x)
  hzDepthCols <- horizonDepths(x)
  tcol <- hzDepthCols[1]
  bcol <- hzDepthCols[2]
  
  # get profile labels from @site
  pLabels <- site(x)[[label]]
  
  ## this should probably use strwidth() AFTER plot() has been called
  # if profile style is auto, determine style based on font metrics
  if(id.style == 'auto') {
    sum.ID.str.width <- sum(sapply(pLabels, strwidth, units='inches', cex=cex.id, font=2))
    plot.width <- par('pin')[1]
    ID.width.ratio <- sum.ID.str.width  / plot.width
    #   	print(ID.width.ratio)
    
    if(ID.width.ratio > 0.7)
      id.style <- 'side'
    else
      id.style <- 'top'
  }
  
  
  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to = max.depth, by = 1), n = n.depth.ticks)
  
  # init plotting region, unless we are appending to an existing plot
  # note that we are using some fudge-factors to get the plotting region just right
  if(!add) {
    # margins are set outside of this function
    
    ## TODO: this seems to extend too far... review
    # y-limits also include y.offset range
    ylim.range <- c(
      max(depth_axis_intervals) + max(y.offset), 
      -extra_y_space
    )
    
    plot(x = 0, y = 0, type = 'n', 
         xlim = c(width * x_left_space_mult, n + (extra_x_space)),
         ylim = ylim.range,
         axes = FALSE, xlab = '', ylab = ''
    )
  }
  
  
  # calculate width of a single character on current plot device
  one.char.width <- strwidth('W')
  
  ## TODO dynamically adjust `width` based on strwidth(longest.hz.name)
  ## TODO abstract single profile sketch code into a single function
  ## TODO skip sketch rendering when i == `n` outside of length(SPC) (depths are NA)
  
  ## TODO: NO DATA template for empty profiles (now possible)
  
  ## iterate over profile index from 1 -> n
  ## note: there may not be `n` profiles
  for(i in 1:n) {
    # convert linear sequence into plotting order
    profile_i <- plot.order[i]
    
    # get truncation flag
    truncation_flag_i <- truncation_flag[profile_i]
    
    # fill NA with FALSE:
    #  - if n > length(x)
    #  - NA present in x$raggedBottom
    truncation_flag_i[is.na(truncation_flag_i)] <- FALSE
    
    # extract the current profile's horizon data
    this_profile_label <- pLabels[profile_i]
    this_profile_id <- pIDs[profile_i]
    
    this_profile_data <- h[h[[IDcol]] == this_profile_id, ]
    
    # extract column names
    cn <- names(this_profile_data)
    
    # extract / generate horizon color
    # note: the ".color" horizon attribute is auto-generated above
    # missing and NA colors have already been dealt with above
    this_profile_colors <- this_profile_data$.color
    
    # extract / generate horizon fill density
    if(! missing(density)) {
      # if a single number was given, then recylce it over all horizons
      if(is.numeric(density))
        this_profile_density <- density
      # otherwise we have a column name
      else {
        m <- match(density, cn)
        if(! is.na(m))
          this_profile_density <- this_profile_data[[m]]
        else # user-defined column is missing
          this_profile_density <- NULL
      }
    } else {
      # no user-defined density column
      this_profile_density <- NULL
    }
    
    
    # extract / generate horizon name
    m <- match(name, cn)
    if(! is.na(m)) {
      this_profile_names <- as.character(this_profile_data[[m]])
    } else {
      # otherwise use an empty string
      this_profile_names <- ''
    }
    
    
    
    ########################################
    ## generate baseline horizon geometry ##
    ########################################
    
    ## center of each sketch
    # 2019-07-15: added relative position feature, could use some more testing
    #             y.offset is indexed to plot.order
    #
    # x0 <- x.idx.offset + i
    x0 <- x.idx.offset + relative.pos[i]
    
    # get vectors of horizon boundaries, and scale
    y0 <- (this_profile_data[[bcol]] * scaling.factor) + y.offset[i]
    y1 <- (this_profile_data[[tcol]] * scaling.factor) + y.offset[i]
    
    
    
    ##############################
    ## create horizons + colors ##
    ##############################
    
    # horizons are parallelograms
    # offset described by hz.distinctness.offset
    # horizontal mid-points described by hz.topography.offset
    
    # iterate over horizons
    nh <- length(y0)
    
    # extract current distinctness offset
    hdo <- this_profile_data[[hz.distinctness.offset]]
    
    # apply sketch-wide scaling factor
    hdo <- hdo * scaling.factor
    
    # extract current topography offset
    hto <- this_profile_data[[hz.topography.offset]]
    
    # apply sketch-wide scaling factor
    hto <- hto * scaling.factor
    
    # extract current set of line types if provided
    if(! is.null(hz.boundary.lty)) {
      ht.lty <- this_profile_data[, hz.boundary.lty]
    } else {
      # use constant value
      ht.lty <- rep(lty, times = nh)
    }
    
    
    
    # empty list for storing y-coordinates
    coords.list <- vector(mode = 'list', length = nh)
    
    # iterate over horizons
    for(j in 1:nh) {
      
      ## rectangle for reference
      # xx <- c(x0 - width, x0 + width, x0 + width, x0 - width)
      # yy <- c(y0[j], y0[j], y1[j], y1[j])
      
      # parallelogram geometry: x-coords are fixed, y-coords vary based on horizon sequence
      # y0 are bottom depths
      # y1 are top depths
      #
      # vertex order: ll, lc, lr, ur, uc, ul
      x.ll <- x0 - width
      x.lr <- x0 + width
      x.ur <- x0 + width
      x.ul <- x0 - width
      # mid-points for horizon topography
      x.lc <- x0
      x.uc <- x0
      xx <- c(x.ll, x.lc, x.lr, x.ur, x.uc, x.ul)
      
      # make polygons based on 1st, 2nd to j-1, last horizon
      
      # yikes! https://github.com/ncss-tech/aqp/issues/189
      # this doesn't work when: 
      # *  there is only a single horizon [now fixed below]
      # *  there is a "gap" between adjacent horizons (overlapOrGap = TRUE)
      
      if(j == 1 & nh == 1){
        ## first horizon of a single-horizon profile ##
        
        # ragged bottom place-holder
        .r <- NULL
        
        # 
        y.ll <- pmin(y0[j] + hdo[j], y0[j]) # cannot exceed y.ll of next horizon
        y.lr <- pmax(y0[j] - hdo[j], y1[j]) # cannot exceed y.ur of this horizon
        y.ur <- y1[j] # use upper-right verbatim
        y.ul <- y1[j] # use upper-left verbatim
        # mid-points for horizon topography
        y.lc <- pmax(y0[j] - hto[j], y1[j]) # cannot exceed top depth of current horizon
        y.uc <- y1[j] # use upper-center verbatim
        
        ## truncated bottom
        # this is only possible when horizon geometry is available
        # e.g. NOT:
        #  - horizon-less SPC
        #  - n > length(SPC)
        if(truncation_flag_i & !all(is.na(xx))) {
          
          # must be an even number of oscillations
          # computed as function of number of profiles
          # adjusted to width (n.osc increases with width)
          # min value of 4
          .raggedN <- pmax(4, round((2.5 * width) * 32 / (n / 2)) * 2)
          
          # ragged bottom line segment: lr -> ll ordering
          .r <- .raggedLines(x1 = x.ll, x2 = x.lr, y = y0[j], o = .raggedOffsets, n = .raggedN)
          
          # modify ragged x and y to include upper right, center, left coords
          xx <- c(.r[, 1], x.ur, x.uc, x.ul)
          yy <- c(.r[, 2], y.ur, y.uc, y.ul)
        } else {
          # place-holder for ragged bottom
          .r <- NULL
          
          # assemble standard y-coords
          yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
        }
        
        # plot first horizon polygon, without borders
        polygon(x = xx, y = yy, col = this_profile_colors[j], border = NA, density = this_profile_density[j], lend = 1)
        
      } else if(j == 1 & nh > 1){
        ## first horizon, of several ##
        
        # ragged bottom place-holder
        .r <- NULL
        
        # 
        y.ll <- pmin(y0[j] + hdo[j], y0[j+1]) # cannot exceed y.ll of next horizon
        y.lr <- pmax(y0[j] - hdo[j], y1[j]) # cannot exceed y.ur of this horizon
        y.ur <- y1[j] # use upper-right verbatim
        y.ul <- y1[j] # use upper-left verbatim
        # mid-points for horizon topography
        y.lc <- pmax(y0[j] - hto[j], y1[j]) # cannot exceed top depth of current horizon
        y.uc <- y1[j] # use upper-center verbatim
        
        # assemble y-coords and plot first horizon polygon, without borders
        yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
        polygon(x = xx, y = yy, col = this_profile_colors[j], border = NA, density = this_profile_density[j], lend = 1)
        
      } else if(j < nh) {
        ## next horizons, except bottom-most horizon ##
        
        # ragged bottom place-holder
        .r <- NULL
        
        # 
        y.ll <- pmin(y0[j] + hdo[j], y0[j+1], na.rm = TRUE) # cannot exceed y.ll of next horizon
        y.lr <- pmax(y0[j] - hdo[j], y0[j-1]) # cannot exceed y.lr of previous horizon
        y.ur <- pmax(y1[j] - hdo[j-1], y1[j-1]) # cannot exceed y.ur of previous horizon
        y.ul <- pmin(y1[j] + hdo[j-1], y0[j]) # cannot exceed y.ul of previous horizon
        # mid-points for horizon topography
        y.lc <- pmax(y0[j] - hto[j], y1[j]) # cannot exceed top depth of current horizon
        y.uc <- pmax(y1[j] - hto[j-1], y1[j-1]) # cannot exceed top depth of previous horizon
        
        # assemble y-coords and plot next n horizon's polygon, without borders
        yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
        polygon(x = xx, y = yy, col=this_profile_colors[j], border=NA, density=this_profile_density[j], lend=1)
        ## debugging
        # polygon(x = xx, y = yy, col=NA, border='red', density=this_profile_density[j], lwd=lwd, lty=lty)
        
      } else {
        ## last horizon ##
        
        # ragged bottom place-holder
        .r <- NULL
        
        #
        y.ll <- y0[j] # user lower-left verbatim
        y.lr <- y0[j] # use lower-right verbatim
        y.ur <- pmax(y1[j] - hdo[j-1], y1[j-1]) # cannot exceed y.ur of previous horizon
        y.ul <- pmin(y1[j] + hdo[j-1], y0[j]) # cannot exceed lower depth of profile
        # mid-points for horizon topography
        y.lc <- y0[j] # truncate to lower depth of last horizon
        y.uc <- pmax(y1[j] - hto[j-1], y1[j-1]) # cannot exceed top of previous horizon
        
        # https://github.com/ncss-tech/aqp/issues/189
        # NA lower horizon depths ll, lc, lr to all be NA
        # this will break divide.hz functionality
        
        ## truncated bottom
        # this is only possible when horizon geometry is available
        # e.g. NOT:
        #  - horizon-less SPC
        #  - n > length(SPC)
        if(truncation_flag_i & !all(is.na(xx))) {
          
          # must be an even number of oscillations
          # computed as function of number of profiles
          # adjusted to width (n.osc increases with width)
          # min value of 4
          .raggedN <- pmax(4, round((2.5 * width) * 32 / (n / 2)) * 2)
          
          # ragged bottom line segment: lr -> ll ordering
          .r <- .raggedLines(x1 = x.ll, x2 = x.lr, y = y0[j], o = .raggedOffsets, n = .raggedN)
          
          # modify ragged x and y to include upper right, center, left coords
          xx <- c(.r[, 1], x.ur, x.uc, x.ul)
          yy <- c(.r[, 2], y.ur, y.uc, y.ul)
        } else {
          # place-holder for ragged bottom
          .r <- NULL
          
          # assemble standard y-coords
          yy <- c(y.ll, y.lc, y.lr, y.ur, y.uc, y.ul)
        }
        
        # plot last horizon polygon, without borders
        polygon(x = xx, y = yy, col = this_profile_colors[j], border = NA, density = this_profile_density[j], lend = 1)
        
        ## debugging
        # polygon(x = xx, y = yy, col = NA, border = 'red', density=this_profile_density[j], lwd = 2)
      } # end selecting horizon sequence class
      
      # save current iteration of coordinates and line type
      # also save ragged bottom coordinates, usually NULL
      coords.list[[j]] <- list(xx = xx, yy = yy, lty = ht.lty[j], ragged = .r)
      
    } # end looping over horizons
    
    
    ## note: have to do this after the polygons, otherwise the lines are over-plotted
    # optionally divide horizons with line segment
    if(divide.hz) {
      
      # iterate over coordinates
      # coordinate logic: ll, lc, lr, ur, uc, ul
      # line style included
      lapply(coords.list, function(seg) {
        
        # lower left -> lower center
        # segments(x0 = seg$xx[1], y0 = seg$yy[1], x1 = seg$xx[2], y1 = seg$yy[2], lwd = lwd, lty =  seg$lty, lend = 1)
        # lower center -> lower right
        # segments(x0 = seg$xx[2], y0 = seg$yy[2], x1 = seg$xx[3], y1 = seg$yy[3], lwd = lwd, lty = seg$lty, lend = 1)
        
        # add line segments in order
        lines(seg$xx, seg$yy, lwd = lwd, lty = seg$lty, lend = 1)
        
      })
    }
    
    
    ## final border left-side, top, right-side
    # note: when manually specifying n > length(SPC)
    # x0,x1,y0,y1 are NA
    # using `na.rm = TRUE` in the following calls to min() or max() will generate warnings
    
    # left-side, top -> bottom
    suppressWarnings(segments(
      x0 = x0 - width, 
      x1 =  x0 - width, 
      y0 = min(y1, na.rm = TRUE), 
      y1 = max(y0, na.rm = TRUE), 
      lwd = lwd,
      lty = lty,
      lend = 2
    ))
    
    # top, left -> right
    suppressWarnings(segments(
      x0 = x0 - width, 
      x1 =  x0 + width, 
      y0 = min(y1, na.rm = TRUE), 
      y1 = min(y1, na.rm = TRUE), 
      lwd = lwd,
      lty = lty,
      lend = 2
    ))
    
    # right-side, top -> bottom
    suppressWarnings(segments(
      x0 = x0 + width, 
      x1 =  x0 + width, 
      y0 = min(y1, na.rm = TRUE), 
      y1 = max(y0, na.rm = TRUE), 
      lwd = lwd,
      lty = lty,
      lend = 2
    ))
    
    
    ## final bottom-most line or ragged edge
    # no line when divide.hz = TRUE
    # simple line when divide.hz = FALSE
    # ragged edge when profiles are truncated
    
    # bottom, left -> right
    if(!divide.hz) {
      
      # use ragged bottom for truncated profiles
      if(truncation_flag_i) {
        
        # derive from last element of coords.list
        lines(
          x = coords.list[[nh]]$ragged[, 1], 
          y = coords.list[[nh]]$ragged[, 2], 
          lwd = lwd, 
          lty = coords.list[[nh]]$lty, 
          lend = 1
        )
        
      } else {
        
        suppressWarnings(segments(
          x0 = x0 - width,
          x1 =  x0 + width,
          y0 = max(y0, na.rm = TRUE),
          y1 = max(y0, na.rm = TRUE),
          lwd = lwd,
          lty = lty,
          lend = 2
        ))  
        
      }
      
    }
    
    
    
    ##################################
    ## horizon designations (names) ##
    ##################################
    switch(
      name.style,
      'right-center' = {
        # standard annotation
        # offset from right-hand side
        hzname.x0 <- x0 + width + (one.char.width * 0.1)
        # horizon depth mid-point
        hzname.y0 <- ( y1 + y0 ) / 2
        # left-hand / vertical center justification
        hzname.adj <- c(0, 0.5)
        hzname.col <- par('fg') # use whatever the foreground color is
      },
      'left-center' = {
        # inset from left-hand side
        hzname.x0 <- (x0 - width) + (one.char.width * 0.1)
        # horizon depth mid-point
        hzname.y0 <- ( y1 + y0 ) / 2
        # left-hand / vertical center justification
        hzname.adj <- c(0, 0.5)
        # high-contrast labels
        hzname.col <- invertLabelColor(this_profile_colors)
      },
      'left-top' = {
        # soilweb style
        # inset from upper-left corner
        hzname.x0 <- (x0 - width) + (one.char.width * 0.1)
        hzname.y0 <- y1
        # left-hand / vertical top justification
        hzname.adj <- c(0, 1)
        # high-contrast labels
        hzname.col <- invertLabelColor(this_profile_colors)
      },
      'center-center' = {
        # inset from upper-left corner
        hzname.x0 <- x0
        hzname.y0 <- (y1 + y0) / 2
        # center just
        hzname.adj <- c(0.5, 0.5)
        # high-contrast labels
        hzname.col <- invertLabelColor(this_profile_colors)
      },
      'center-top' = {
        # inset from upper-left corner
        hzname.x0 <- x0
        hzname.y0 <- y1
        # center just
        hzname.adj <- c(0.5, 1)
        # high-contrast labels
        hzname.col <- invertLabelColor(this_profile_colors)
      }
      
    )
    
    
    
    ## TODO: use find/fixOverlap() to adjust horizon designations in the presence of collisions (PARDEE)    
    ## See hz depth annotation code below
    
    # optionally shrink the size of names if they are longer than a given thresh
    if(shrink) {
      
      # identify affected horizon names
      names.wide <- which(nchar(this_profile_names) > shrink.cutoff)
      cex.names.shrunk <- rep(cex.names, length(this_profile_data[[tcol]]))
      cex.names.shrunk[names.wide] <- cex.names.shrunk[names.wide] * 0.8
      
      # optionally shrink based on horizon thickness threshold
      if(!is.null(shrink.thin)) {
        
        # identify affected horizon names
        names.thin <- which((y0 - y1) < shrink.thin)
        # only apply shrinkage to those not already shrunk above
        names.thin <- names.thin[which(! names.thin %in% names.wide)]
        # apply shrinkage
        cex.names.shrunk[names.thin] <- cex.names.shrunk[names.thin] * 0.8
      }
      
      # add horizon names
      text(
        x = hzname.x0, 
        y = hzname.y0, 
        labels = this_profile_names, 
        cex = cex.names.shrunk, 
        adj = hzname.adj, 
        col = hzname.col
      )
      
    } else {
      # standard printing of names, all at the same size
      text(
        x = hzname.x0, 
        y = hzname.y0, 
        labels = this_profile_names, 
        cex = cex.names, 
        adj = hzname.adj, 
        col = hzname.col
      )
      
    }
    
    
    ##################################
    ## horizon depth annotation     ##
    ##################################
    if(hz.depths) {
      
      # scaling factor for hz depths
      hz.depths.cex <- cex.names * 0.9
      
      # extra space between profile sketch and left-justified label
      # seems to scale with cex.names
      hz.depths.xfuzz <- strwidth('5', cex = hz.depths.cex) / 2
      
      # device coordinates: scaling / offset applied
      # along with user adjustment
      hzd.txt.x <- x0 + width + hz.depths.xfuzz + hz.depths.offset
      
      ## top-most horizon
      # top depth: vertical alignment is close to "top"
      hzd.txt <- this_profile_data[[tcol]][1]
      text(
        x = hzd.txt.x, 
        y = y1[1], 
        labels = hzd.txt, 
        cex = hz.depths.cex, 
        font = 1, 
        adj = c(0, 0.8)
      )
      
      
      ## in-between horizons, if present: vertical align is "center"
      if(nh > 1) {
        # horizon depth annotation text (no scaling / offset applied here)
        hzd.txt <- this_profile_data[[tcol]][-1]
        
        # device coordinates: scaling / offset applied
        hzd.txt.y <- y1[-1]
        
        ## collision detection / fix
        if(fixLabelCollisions) {
          
          # reasonable threshold for label collision detection
          # depends on aesthetic weighting / graphics device / hz.depths.cex
          y.thresh <- 1.125 * abs(strheight('0', cex = hz.depths.cex))
          
          # must include top + bottom depths for collision detection
          # account for the fact that top-most and bottom-most horizon depths are inset
          # based on native units (e.g. cm)
          
          # top vertical buffer should not exceed bottom of first hz
          .vbf <- 0.8
          .vertical_buffer_top <- y1[1] + pmin((.vbf * scaling.factor), min(y1[-1]))
          
          # bottom vertical buffer must be truncated by max.depth
          # this can result in bottom anchor < previous elements in pos
          # remove in next steps
          .vertical_buffer_bottom <- pmin(
            ((max.depth - .vbf) * scaling.factor) + y.offset[i], 
            y0[nh] - (.vbf * scaling.factor)
          )
          
          # add synthetic top / bottom anchors
          .pos <- c(
            .vertical_buffer_top,
            y1[-1],
            .vertical_buffer_bottom
          )
          
          # keep only positions that are < max.depth, after y.offset and scaling
          # these are scaled positions
          # print(y.thresh)
          .pos <- .pos[which(.pos < ((max.depth * scaling.factor) + y.offset[i]))]
          
          # setup arguments to fixOverlap()
          fixOverlapArgs[['x']] <- .pos
          fixOverlapArgs[['thresh']] <- y.thresh
          
          # find / fix overlap using electrostatic simulation
          # this includes top/bottom anchor points
          hzd.txt.y.fixed <- suppressMessages(
            do.call('fixOverlap', fixOverlapArgs)
          )
          
          # remove top AND bottom anchors
          hzd.txt.y.fixed <- hzd.txt.y.fixed[-c(1, length(hzd.txt.y.fixed))]
          
          ## this is the Label Adjustment Index (LAI)
          # how much shuffling was performed?
          .LAI <- (hzd.txt.y - hzd.txt.y.fixed)
          # normalize
          .LAI <- sum(abs(.LAI), na.rm = TRUE) / y0[nh]
          
          # keep track of overlap metric for each profile
          lsp[['hz.depth.LAI']][i] <- .LAI
          
          # make changes to annotation label locations
          hzd.txt.y <- hzd.txt.y.fixed
        }
        
        
        # annotate
        text(
          x = hzd.txt.x, 
          y = hzd.txt.y, 
          labels = hzd.txt, 
          cex = hz.depths.cex, 
          font = 1, 
          adj = c(0, 0.5)
        ) 
      }
      
      ## bottom-most horizon
      # bottom depth: vertical alignment is "bottom"
      hzd.txt <- this_profile_data[[bcol]][nh]
      text(
        x = hzd.txt.x, 
        y = y0[nh], 
        labels = hzd.txt, 
        cex = hz.depths.cex, 
        font = 1, 
        adj = c(0, 0)
      )
      
      
      ##############################
      ## hz depth connector lines ##
      ##############################
      if(hz.depths.lines) {
        
        # .x0, .y0: staring coordinates of spike
        # .x1, .y1: ending coordinates of spike + staring coordinates for diagonal member
        # .xfuzz: horizontal offset for labels
        # .txt.x: annotation x-coordinate
        # .txt.y: annotation y-coordinate
        .connectorLines <- function(.x0, .y0, .x1, .y1, .xfuzz, .txt.x, .txt.y) {
          
          # horizontal spike (skipping top + bottom labels)
          # right-edge, at depth -> padded, left-edge/center of annotation (no collision fix)
          segments(
            x0 = .x0, 
            y0 = .y0, 
            x1 = .x1 + (.xfuzz / 2), 
            y1 = .y1
          )
          
          # connector member (skipping top + bottom labels)
          # diagonal when collision fix has been applied
          #
          # right-end of spike member ->
          # padded, left-edge/center of annotation (after collision fix)
          segments(
            x0 = .x0 + (.xfuzz / 2), 
            y0 = .y1,
            x1 = .txt.x - (.xfuzz / 3), 
            y1 = .txt.y, 
          ) 
          
        }
        
        
        ## middle horizon labels, if present
        if(nh > 1) {
          
          .connectorLines(
            .x0 = x0 + width,
            .y0 = y1[-1],
            .x1 = x0 + width,
            .y1 = y1[-1],
            .xfuzz = hz.depths.xfuzz,
            .txt.x = hzd.txt.x,
            .txt.y = hzd.txt.y
          )
        }
        
        
        ## top horizon label
        .connectorLines(
          .x0 = x0[1] + width,
          .y0 = y1[1],
          .x1 = x0[1] + width,
          .y1 = y1[1],
          .xfuzz = hz.depths.xfuzz,
          .txt.x = hzd.txt.x,
          .txt.y = y1[1] - (strheight('0', cex = hz.depths.cex) / 4)
        )
        
        
        ## bottom horizon label
        .connectorLines(
          .x0 = x0[1] + width,
          .y0 = y0[nh],
          .x1 = x0[1] + width,
          .y1 = y0[nh],
          .xfuzz = hz.depths.xfuzz,
          .txt.x = hzd.txt.x,
          .txt.y = y0[nh] + (strheight('0', cex = hz.depths.cex) / 3)
        )
        
      } # end depth annotation lines
      
    } # end horizon depth annotation
    
    
    
    
    
    
    #################
    ## profile IDs ##
    #################
    
    # profile is placed relative to the y-offset vector
    
    if(print.id) {
      # optionally abbreviate
      if(abbr)
        id.text <- abbreviate(as.character(this_profile_label), abbr.cutoff)
      
      # no abbreviations of th ID
      else
        id.text <- as.character(this_profile_label)
      
      # add the text: according to style
      if(id.style == 'top') {
        text(x = x0, y = y.offset[i], id.text, pos = 3, font = font.id, cex = cex.id, srt = srt.id)
      }
      
      if(id.style == 'side') {
        text(x0 - (width + 0.025), y.offset[i], id.text, adj = c(1, -width), font = font.id, cex = cex.id, srt = 90)
      }
      
    }
    
  } # end looping over profiles
  
  
  ################
  ## depth axis ##
  ################
  
  # suppress when there are multiple y.offsets
  if(length(unique(y.offset)) > 1) {
    plot.depth.axis <- FALSE 
  }
  
  if(plot.depth.axis) {
    depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset[1]
    depth_axis_labels <- paste(depth_axis_intervals, depth_units(x))
    
    axis(side=4, line=axis.line.offset, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis, col.axis=par('fg'))
  }
  
  
  
  ######################
  ## alternate labels ##
  ######################
  if(!missing(alt.label)) {
    al <- site(x)[[alt.label]]
    al <- al[plot.order]
    text(x = 1:length(x), y = y.offset + 3, labels = al, srt = 90, adj = c(1, 0.5), font = 2, cex = cex.id * 1.5, col = alt.label.col)
  }
  
  
  ########################################
  ## legend for thematic profile sketch ##
  ########################################
  
  # optionally show legend
  if(show.legend) {
    # extract from interpretation of horizon colors
    cld <- hz.color.interpretation$color.legend.data
    
    if(! is.null(cld)) {
      # if no title given, set col.label to name of column containing thematic information
      mtext(side=3, text=col.label, font=2, line=1.6)
      
      # gracefully handle all-NA in thematic variable
      if(length(cld$legend) > 0) {
        
        # possibly split legend across multiple rows
        if(cld$multi.row.legend) {
          
          # compute max space required for legend items
          # better formatting
          # note: must be called AFTER high level plot()
          leg.text.width <- (max(strwidth(cld$legend, cex = col.legend.cex)))
          
          # row 1
          legend('bottom', inset = c(0, 0.99),
                 legend = cld$legend[cld$leg.row.indices$row.1],
                 col = cld$col[cld$leg.row.indices$row.1],
                 text.width = leg.text.width,
                 bty = 'n', pch = 15, horiz = TRUE, xpd = TRUE, cex = col.legend.cex, x.intersp = 1
          )
          
          # row 2
          legend('bottom', inset = c(0, 0.94),
                 legend = cld$legend[cld$leg.row.indices$row.2],
                 col = cld$col[cld$leg.row.indices$row.2],
                 text.width = leg.text.width,
                 bty = 'n', pch = 15, horiz = TRUE, xpd = TRUE, cex = col.legend.cex, x.intersp = 1
          )
          
        } else {
          # standard invocation
          legend(
            'bottom', 
            legend = cld$legend, 
            col = cld$col,
            bty = 'n',
            pch = 15,
            horiz = TRUE,
            xpd = TRUE,
            inset = c(0, 0.99),
            cex = col.legend.cex,
            x.intersp = 1
          )
        }
      }
      
    }
  }
  
  # save to aqp env
  assign('last_spc_plot', lsp, envir = aqp.env)
  
  
}

#' generic plot method for \code{SoilProfileCollection} objects
#' @name plot
#' @param y (not used)
#' @aliases plot,SoilProfileCollection,ANY-method
#' @rdname SoilProfileCollection-plotting-methods
#' @export
setMethod("plot", signature(x = "SoilProfileCollection",
                            y = "ANY"),
          definition = {function(x, y, ...) plotSPC(x, ...)})
