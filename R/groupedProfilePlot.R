#' @title Grouped Soil Profile Plot
#'
#' @description Plot a collection of soil profiles, sorted by group.
#'
#' The left-right ordering of groups can be adjusted by converting
#' \code{groups} into a factor and explicitly setting factor levels.
#' Alpha-numeric ordering is used for all other types.
#'
#' @param x a `SoilProfileCollection` object
#' 
#' @param groups the name of a site-level attribute that defines groups, factor
#' levels will influence plotting order
#' 
#' @param group.name.offset vertical offset for group names, single numeric
#' value or vector of offsets
#' 
#' @param group.name.cex font size for group names
#' 
#' @param group.line.col color for line that splits groups
#' 
#' @param group.line.lwd width of line that splits groups
#' 
#' @param group.line.lty style of line that splits groups
#' 
#' @param break.style style of group boundaries: "line", "arrow", "both"
#' 
#' @param break.offset horizontal offset used to place vertical breaks and/or arrows, shifted slightly to the right of default when `hz.depths=TRUE` is passed to `plotSPC()`
#' 
#' @param arrow.offset vertical offset for "arrow" style boundaries, single
#' numeric value or vector of offsets
#' 
#' @param arrow.length value passed to `arrows` to define arrow head size
#' 
#' @param \dots further arguments to `plotSPC`
#' 
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords hplots
#' 
#' @examples
#'
#' # sample data
#' data(sp1)
#' # convert colors from Munsell to hex-encoded RGB
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' # add a groups
#' sp1$group.2 <- sprintf("%s-%s", rev(LETTERS[1:3]), sp1$group)
#'
#' # convert fake groupt to factor with new levels
#' sp1$group.3 <- factor(sp1$group.2, levels=c('C-2', 'B-2', 'A-2', 'C-1', 'B-1', 'A-1'))
#'
#' # plot profiles, sorted and annotated by 'group' (integers)
#' par(mar=c(1,1,1,1))
#' groupedProfilePlot(sp1, groups='group', max.depth=150, group.name.offset = -5, id.style='side')
#'
#' # plot profiles, sorted and annotated by 'group.2' (characters)
#' par(mar=c(1,1,1,1))
#' groupedProfilePlot(sp1, groups='group.2', max.depth=150, group.name.offset = -5, id.style='side')
#'
#' # plot profiles, sorted and annotated by 'group.3' (characters)
#' par(mar=c(1,1,1,1))
#' groupedProfilePlot(sp1, groups='group.3', max.depth=150, group.name.offset = -5, id.style='side')
#'
#'
#' # make fake site-level attribute and adjust levels
#' sp1$new.group <- sample(letters[1:3], size=length(sp1), replace=TRUE)
#'
#' # tabulate pedons / group
#' tab <- table(sp1$new.group)
#'
#' # sort large -> small
#' tab <- sort(tab, decreasing = TRUE)
#'
#' # set levels based on sorted tabulation
#' # assign custom labels
#' sp1$new.group <- factor(sp1$new.group, levels=names(tab),
#' labels=paste0(names(tab), ' (', tab, ')'))
#'
#' groupedProfilePlot(sp1, groups='new.group', max.depth=150,
#' group.name.offset = -10, id.style='side')
#'
#' # offsets can be set using a vector of values, recycled as needed
#' groupedProfilePlot(sp1, groups='new.group', max.depth=150,
#' group.name.offset=c(-10, -5), id.style='side')
#'
#' # annotate with arrows instead of vertical lines
#' groupedProfilePlot(sp1, groups='new.group', max.depth=150,
#' group.name.offset=c(-10, -12), break.style='arrow', arrow.offset=-3,
#' group.line.lty = 1, group.line.lwd = 1, id.style='side')
#'
#'
#' \dontrun{
#' # more complete example using data from soilDB package
#' data(loafercreek, package='soilDB')
#' par(mar=c(1,1,1,1))
#' # lines
#' groupedProfilePlot(loafercreek, groups='hillslopeprof', group.name.cex = 0.5,
#' group.name.offset = -10)
#'
#' # arrows
#' groupedProfilePlot(loafercreek, groups='hillslopeprof', group.name.cex = 0.5,
#' group.name.offset = -10, break.style ='arrow', group.line.lty = 1,
#' group.line.lwd = 1)
#'
#' # both
#' groupedProfilePlot(loafercreek, groups='hillslopeprof', group.name.cex = 0.5,
#' group.name.offset = -10, break.style ='both', group.line.lty = 1,
#' group.line.lwd = 1)
#' }
#'
groupedProfilePlot <- function(x, groups, group.name.offset = -5, group.name.cex = 0.75, group.line.col = 'RoyalBlue', group.line.lwd = 2, group.line.lty = 2, break.style = c('line', 'arrow', 'both'), break.offset = 0.5, arrow.offset = group.name.offset + 5, arrow.length = 0.1, ...) {

  # sanity check, `groups` must be a site-level attribute
  if(! any(groups %in% siteNames(x))) {
    stop(sprintf("%s is not a site-level attribute", groups), call. = FALSE)
  }

  # sanity check
  break.style <- match.arg(break.style)
  
  # extract site data, used for ordering and labeling
  s <- site(x)

  # break.offset needs to be shifted to the right a little bit, when hz.depths = TRUE is specified
  if('hz.depths' %in% names(list(...)) && list(...)[['hz.depths']]) {
    # only make a change if break.offset isn't specified
    if(missing(break.offset)){
      # shift to the right slightly
      # message('`hz.depths=TRUE`, shifting `break.offset` to the right')
      break.offset <- 0.62
    }
  }
  
  # if groups are already a factor, keep existing levels
  # note that an ordered factor has multiple classes: "ordered" , "factor"
  if(! any(inherits(s[[groups]], 'factor'))) {
    s[[groups]] <- factor(as.character(s[[groups]]))
  }

  # derive ordering based on `groups`
  # alpha sorting when `grpups` is numeric or character
  # factor levels when `groups` is a factor
  new.order <- order(s[[groups]])
  lab <- s[[groups]][new.order]

  # test for NA
  NA.lab <- which(is.na(lab))

  # replace with missing label with '<missing>'
  # this requires conversion: factor -> character -> replace NA -> factor with new levels
  if(length(NA.lab) > 0) {
    message('NA in grouping label, filling with `<missing>`')
    o.levels <- levels(lab)
    lab <- as.character(lab)
    lab[NA.lab] <- '<missing>'
    lab <- factor(lab, levels=c(o.levels, '<missing>'))
  }

  # setup plot with plot.SoilProfileCollection
  plot(x, plot.order=new.order, ...)

  # get last plot parameters
  lsp <- get('last_spc_plot', envir=aqp.env)

  # get just those levels that are in our data, preserving order of original levels
  unique.lab <- levels(lab)[which(levels(lab) %in% unique(lab))]
  group.lengths <- rle(as.numeric(lab))$lengths

  # label positions
  lab.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + break.offset

  # group boundaries on x-axis
  boundary.positions <-  cumsum(group.lengths)[-length(group.lengths)] + break.offset

  # reasonable upper / lower boundaries on y-axis
  # these are informed by plotting parameters sent to plotSPC()
  # note: this is a vector -> reduce to scalar
  .yoffset <- mean(lsp$y.offset)
  
  upper.position <- (.yoffset) + (group.name.offset/2 * lsp$scaling.factor)
  lower.position <- (.yoffset) + (lsp$max.depth * lsp$scaling.factor)
  
  # only add grouping symbols if number of groups is > 1
  if(length(boundary.positions)) {
    # add group boundaries
    if(break.style == 'line')
      segments(y0 = upper.position, y1=lower.position, x0=boundary.positions, x1=boundary.positions, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)

    if(break.style == 'arrow')
      arrows(x0=c(0.5, boundary.positions), x1=c(boundary.positions, length(x)+0.5), y0=arrow.offset, code=3, length=arrow.length, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)

    if(break.style == 'both') {
      segments(y0 = upper.position, y1=lower.position, x0=boundary.positions, x1=boundary.positions, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
      arrows(x0=c(0.5, boundary.positions), x1=c(boundary.positions, length(x)+0.5), y0=arrow.offset, code=3, length=arrow.length, lty=group.line.lty, lwd=group.line.lwd, col=group.line.col)
    }
  }

  # annotate with group labels at group centers
  text(lab.positions, group.name.offset, unique.lab, cex = group.name.cex, adj = c(0.5, 0), font = 4)
}

