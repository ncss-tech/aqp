
# order profiles by a single, site-level attribute
groupedProfilePlot <- function(x, groups, group.name.offset=-5, group.name.cex=0.75, group.line.col='RoyalBlue', group.line.lwd=2, group.line.lty=2, break.style='line', arrow.offset=group.name.offset + 5, arrow.length=0.1, ...) {
  
  # sanity check, `groups` must be a site-level attribute
  if(! any(groups %in% siteNames(x))) {
    stop(sprintf("%s is not a site-level attribute", groups), call. = FALSE)
  }
  
  # extract site data, used for ordering and labeling
  s <- site(x)
  
  # if groups are already a factor, keep existing levels
  # note that an ordered factor has multiple classes: "ordered" , "factor"
  if(! any(inherits(s[[groups]], 'factor'))) {
    s[[groups]] <- factor(as.character(s[[groups]]))
  }
  
  # derive ordering based on `groups`
  # alpha sorting when `gopups` is numeric or character
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
  lab.positions <- (cumsum(group.lengths) - (group.lengths / 2)) + 0.5
  
  # group boundaries on x-axis
  boundary.positions <-  cumsum(group.lengths)[-length(group.lengths)] + 0.5
  
  # resonable upper / lower boundaries on y-axis
  # these are informed by plotting parameters sent to plotSPC()
  upper.position <- (lsp$y.offset) + (group.name.offset/2 * lsp$scaling.factor)
  lower.position <- (lsp$y.offset) + (lsp$max.depth * lsp$scaling.factor)
  
  if(length(boundary.positions)) { # only add grouping symbols if number of groups is > 1
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
  
  # annotate with group labels
  text(lab.positions, group.name.offset, unique.lab, cex=group.name.cex, adj=c(0.75, 0), font=4)
}

