## TODO: move more hard-coded geom elements to arguments / heuristics
## NOTE: works best if distance matrix scaled to approximately {0,1}


#' @title Plot soil profiles below a dendrogram
#' 
#' @description Plot soil profiles below a dendrogram, using methods from the ape package.
#'
#' @param x a `SoilProfileCollection` object
#' @param clust a hierarchical clustering object generated by `hclust`, `cluster::agnes`, or `cluster::diana`
#' @param rotateToProfileID logical, attempt rotation of dendrogram according to original profile IDs, requires `dendextend` package
#' @param scaling.factor vertical scaling of the profile heights (may have to tinker with this)
#' @param width scaling of profile widths
#' @param y.offset vertical offset for top of profiles
#' @param dend.y.scale extent of y-axis (may have to tinker with this)
#' @param dend.color dendrogram line color
#' @param dend.width dendrogram line width
#' @param dend.type dendrogram type, passed to `plot.phylo()`, either "phylogram" or "cladogram"
#' @param debug logical, optionally print debugging data and return a `data.frame` of linking structure
#' @param ... additional arguments to `plotSPC`
#' 
#' 
#' @note You may have to tinker with some of the arguments to get optimal arrangement and scaling of soil profiles.
#' 
#' @author D.E. Beaudette
#' 
#' @keywords hplots
#' 
#' @return This function is typically called to create graphical output, when `debug=TRUE` a `data.frame` of IDs and linking structure used to build the figure.
#' 
#' @export
#'
plotProfileDendrogram <- function(x, clust, rotateToProfileID = FALSE, scaling.factor = 0.01, width = 0.1, y.offset = 0.1, dend.y.scale = max(clust$height * 2, na.rm = TRUE), dend.color = par('fg'), dend.width = 1, dend.type = c("phylogram", "cladogram"), debug = FALSE, ...) {
  
  # limit dendrogram types
  dend.type <- match.arg(tolower(dend.type), c("phylogram", "cladogram"), several.ok = FALSE)
  
  # sanity check: must be either agnes or diana object
  if(! inherits(clust, c('agnes', 'diana', 'hclust'))) {
    stop('clust must be an object generated by hclust(), cluster::diana() or cluster::agnes()')
  }
  
  # convert to hclust for all intermediate steps
  d.hclust <- as.hclust(clust)
  
  
  # IDs from SPC and clustering object: may not be in the same order!
  d.ids <- d.hclust$labels
  x.ids <- profile_id(x)
  
  # sanity check: ID vectors should be the same length
  if( length(d.ids) != length(x.ids) ) {
    print(
      list(
        profileID = profile_id(x), 
        clustID = d.ids, 
        order = d.hclust$order
      )
    )
    stop('inconsistent SoilProfileCollection and clustering object, inconsistent number of IDs', call. = FALSE)
  }
  
  # sanity check: all IDs must be accounted for
  sd.1 <- setdiff(d.ids, x.ids)
  sd.2 <- setdiff(x.ids, d.ids)
  
  if( length(sd.1) > 0 ) {
    print(
      list(
        profileID = profile_id(x), 
        clustID = d.ids, 
        order = d.hclust$order
      )
    )
    msg <- sprintf('IDs missing from SoilProfileCollection: [%s]', paste(sd.1, collapse = ', '))
    stop(msg, call. = FALSE)
  }
  
  if( length(sd.2) > 0 ) {
    print(
      list(
        profileID = profile_id(x), 
        clustID = d.ids, 
        order = d.hclust$order
      )
    )
    msg <- sprintf('IDs missing from cluster object: [%s]', paste(sd.1, collapse = ', '))
    stop(msg, call. = FALSE)
  }
  
  # profile IDs and clustering IDs may not be in the same order
  if(any(x.ids != d.ids)){
    message('profile IDs and clustering IDs are not in the same order')
  }
  
  
  # attempt to rotate dendrogram as close to original profile IDs as possible
  if(rotateToProfileID) {
    
    # rotate dendrogram
    if (requireNamespace('dendextend', quietly = TRUE)) {
      d.hclust <- dendextend::rotate(d.hclust, order = x.ids)
      
    } else {
      message('rotation of dendrogram requires `dendextend` package', call. = FALSE)
      
      ## TODO: think about how to do this via ape methods, 
      ## and without all of the to/from conversion to hclust
      
    }
    
  }
  
  
  # plotting order
  # create a link between dendrogram-tip-sorted IDS ---> profile IDs
  link.idx <- match(d.ids[d.hclust$order], x.ids)
  
  # allocate extra space
  if(debug){
    # device options are modified locally, reset when done
    # warning: this will reset the device coordinates!
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    
    par(mar = c(5, 5, 5, 5))
  }
  
  # convert to ape class for plotting
  dend <- as.phylo(d.hclust)
  
  # setup plot and add dendrogram
  plot(
    dend, 
    cex = 0.8, 
    direction = 'up', 
    y.lim = c(dend.y.scale, 0), 
    x.lim = c(0.5, length(x)+1), 
    show.tip.label = (debug), 
    edge.color = dend.color, 
    edge.width = dend.width, 
    type = dend.type
  )
  
  # get the last plot geometry
  lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
  
  # plot the profiles, in the ordering defined by the dendrogram
  # with a couple fudge factors to make them fit
  plotSPC(
    x, 
    plot.order = link.idx, 
    add = TRUE, 
    width = width, 
    scaling.factor = scaling.factor, 
    y.offset = max(lastPP$yy) + y.offset, 
    ...
  )
  
  
  if(debug) {
    # grid()
    axis(1, las=1, at=1:length(x))
    axis(2, las=1)
    # abline(h=max(lastPP$yy) + y.offset, col='red')
    
    # IDs and linking structure
    return(
      invisible(
        data.frame(
          profileID = x.ids, 
          clustID = d.ids, 
          clustID.ordered = d.ids[d.hclust$order],
          profile.plot.order = link.idx
        )
      )
    )
    
    
  }
  
}

