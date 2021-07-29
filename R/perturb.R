#' Perturb soil horizon depths using boundary distinctness
#' 
#' @aliases permute_profile
#' 
#' @param p A single-profile SoilProfileCollection
#' @param n Number of new profiles to generate (default: 100)
#' @param id a vector of profile IDs with length equal to (\code{n}). Overrides use of \code{seq_len(n)} as default profile ID values.
#' 
#' @param boundary.attr Horizon variance attribute containing numeric "standard deviations" reflecting boundary transition distinctness
#' 
#' @param thickness.attr Horizon variance attribute containing numeric "standard deviations" reflecting horizon thickness  
#' 
#' @param max.depth Depth below which horizon depths are not perturbed (default: `NULL`)
#' @param min.thickness Minimum thickness of permuted horizons (default: `1`)
#' @param new.idname New column name to contain unique profile ID (default: `pID`)
#' 
#' @description "Perturbs" the **boundary between horizons** or the **thickness of horizons** using a standard deviation specified as a horizon-level attribute. This is selected using either `boundary.attr` or `thickness.attr` to specify the column name.
#' 
#' The boundary standard deviation corresponds roughly to the concept of "horizon boundary distinctness." In contrast, the _horizon thickness_ standard deviation corresponds roughly to the "variation in horizon thickness" so it may be determined from several similar profiles that have a particular layer "in common." 
#' 
#' @details
#' 
#' Imagine a Normal curve with mean centered on the vertical (depth axis) at a representative value (RV) horizon bottom depth or thickness. By the Empirical Rule for Normal distribution, two "standard deviations" above or below that "central" mean value represent 95% of the "typical volume" of that horizon or boundary.
#' 
#' `perturb` can leverage semi-quantitative (ordered factor) levels of boundary distinctness/topography for the upper and lower boundary of individual horizons. A handy function for this is [hzDistinctnessCodeToOffset()]. The `boundary.attr` is arguably easier to parameterize from a single profile description or "Form 232" where _horizon boundary distinctness_ classes (based on vertical distance of transition) are conventionally recorded for each layer.
#' 
#' Alternately, `perturb` can be parameterized using standard deviation in thickness of layers derived from a group. Say, the variance parameters are defined from a set of pedons correlated to a particular series or component, and the template "seed" profile is, for example, the Official Series Description or the Representative Component Pedon.
#'
#' @return a SoilProfileCollection with `n` realizations of `p`
#' 
#' @seealso [random_profile()] [hzDistinctnessCodeToOffset()]
#' 
#' @export
#' @author D.E. Beaudette, A.G. Brown
#'
#' @examples
#' 
#' ### THICKNESS
#' 
#' # load sample data and convert into SoilProfileCollection
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#' 
#' # select a profile to use as the basis for simulation
#' s <- sp3[3,]
#' 
#' # reset horizon names
#' s$name <- paste('H', seq_along(s$name), sep = '')
#' 
#' # simulate 25 new profiles
#' horizons(s)$hz.sd <- 2 # constant standard deviation
#' sim.1 <- perturb(s, n = 25, thickness.attr = "hz.sd")
#' 
#' # simulate 25 new profiles using different SD for each horizon
#' horizons(s)$hz.sd <- c(1, 2, 5, 5, 5, 10, 3)
#' sim.2 <- perturb(s, n = 25, thickness.attr = "hz.sd")
#' 
#' # plot
#' par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
#' plot(sim.1)
#' mtext(
#'   'SD = 2',
#'   side = 2,
#'   line = -1.5,
#'   font = 2,
#'   cex = 0.75
#' )
#' plot(sim.2)
#' mtext(
#'   'SD = c(1, 2, 5, 5, 5, 10, 3)',
#'   side = 2,
#'   line = -1.5,
#'   font = 2,
#'   cex = 0.75
#' )
#' 
#' # aggregate horizonation of simulated data
#' # note: set class_prob_mode=2 as profiles were not defined to a constant depth
#' sim.2$name <- factor(sim.2$name)
#' a <- slab(sim.2, ~ name, class_prob_mode=2)
#' 
#' # convert to long format for plotting simplicity
#' library(data.table)
#' a.long <- melt(as.data.table(a),
#'                id.vars = c('top', 'bottom'),
#'                 measure.vars = levels(sim.2$name))
#' 
#' # plot horizon probabilities derived from simulated data
#' # dashed lines are the original horizon boundaries
#' library(lattice)
#' 
#' xyplot(
#'   top ~ value,
#'   groups = variable,
#'   data = a.long,
#'   subset = value > 0,
#'   ylim = c(100,-5),
#'   type = c('l', 'g'),
#'   asp = 1.5,
#'   ylab = 'Depth (cm)',
#'   xlab = 'Probability',
#'   auto.key = list(
#'     columns = 4,
#'     lines = TRUE,
#'     points = FALSE
#'   ),
#'   panel = function(...) {
#'     panel.xyplot(...)
#'     panel.abline(h = s$top, lty = 2, lwd = 2)
#'   }
#' )
#' 
#' ### BOUNDARIES
#' 
#' # example with sp1 (using boundary distinctness)
#' data("sp1")
#' depths(sp1) <- id ~ top + bottom
#'
#' # specify "standard deviation" for boundary thickness
#' #   consider a normal curve centered at boundary RV depth
#' # lookup table: ~maximum thickness of boundary distinctness classes, divided by 3
#' bound.lut <- c('V'=0.5,'A'=2,'C'=5,'G'=15,'D'=45) / 3
#'
#' ## V          A          C          G          D
#' ## 0.1666667  0.6666667  1.6666667  5.0000000 15.0000000
#'
#' sp1$bound_sd <- bound.lut[sp1$bound_distinct]
#'
#' # hold any NA boundary distinctness constant
#' sp1$bound_sd[is.na(sp1$bound_sd)] <- 0
#'
#' quantile(sp1$bound_sd, na.rm = TRUE)
#' p <- sp1[3]
#' 
#' # assume boundary sd is 1/12 midpoint of horizon depth
#' # (i.e. general relationship: SD increases (less well known) with depth)
#' sp1 <- transform(sp1, midpt = (bottom - top) / 2 + top, bound_sd = midpt / 12)
#' quantile(sp1$bound_sd)
#' 
#' perturb(p, boundary.attr = "bound_sd")
#' 
perturb <- function(p,
                    n = 100,
                    id = NULL,
                    thickness.attr = NULL,
                    boundary.attr = NULL,
                    min.thickness = 1,
                    max.depth = NULL,
                    new.idname = 'pID') {
  
  custom.ids <- FALSE
  
  if (!is.null(thickness.attr) && !is.null(boundary.attr) |
      is.null(thickness.attr) && is.null(boundary.attr)) {
    stop("must provide one column name: thickness `thickness.attr` OR boundary `boundary.attr` containing horizon-level standard deviations", call. = FALSE)
  }
  
  by_thickness <- FALSE
  
  if (!is.null(thickness.attr)) {
    by_thickness <- TRUE
  }
  
  if (!missing(id)) {
    custom.ids <- TRUE
    # keep track of missing `n` argument before it is set
    missing.n <- missing(n)
    
    n <- length(unique(id))

    if (n != length(id))
      stop("custom profile ID vector `id` contains non-unique values", call. = FALSE)

    if (!missing.n)
      message("if profile ID vector `id` is specified, `n` argument is ignored")
  }

  hz <- horizons(p)
  
  if(by_thickness) {
    bounds <- hz[[thickness.attr]]
  } else {
    bounds <- hz[[boundary.attr]]
  }
  
  depthz <- horizonDepths(p)
  mindepth <- min(hz[[depthz[1]]])

  if(!is.numeric(bounds) | !length(bounds) == nrow(p)) {
    stop("variance attribute must be refer to a numeric column in `p` containing standard deviations of horizon (or boundary) thickness")
  }

  if(length(p) != 1 | !inherits(p, 'SoilProfileCollection')) {
    stop("`p` must be a single-profile SoilProfileCollection")
  }

  if(!checkHzDepthLogic(p)$valid) {
    stop("one or more horizon depth logic tests failed for object `p`")
  }

  if (by_thickness) {
    # aqp::sim() traditionally perturbs horizon thickness
    perturb_var <- hz[[depthz[2]]] - hz[[depthz[1]]]
  } else {
    # perturb bottom depths instead of thickness for boundaries
    perturb_var <- hz[[depthz[2]]]
  }
  
  # do not vary layers below `max.depth` (if not NULL) can be arbitrary depth
  if (!is.null(max.depth)) {
    if (!is.na(max.depth) & max.depth >= mindepth) {
      if (by_thickness) {
        ldx <- (cumsum(perturb_var) + mindepth) > max.depth
      } else {
        ldx <- perturb_var > max.depth
      }
      bounds[ldx] <- 0
    }
  }
  
  # for each horizon bottom depth (boundary) calculate a gaussian offset
  #  from the representative value recorded in the pedon descripton
  res <- do.call('rbind', lapply(1:nrow(p), function(i) {
    new <- rnorm(n, perturb_var[i], bounds[i])

    # this is a bit non-kosher, but rather than sorting to fix random depths
    # that may be out of order, replace them iteratively until there are none
    
    # it is possible to specify SDs so large the loop below will not converge,
    # so a hard break is triggered at 1000 iterations.

    # in practice, qc warnings for improbably large SD would be useful
    # say, if the SD is greater than 1/3 the hz thickness, you are likely to
    # generate extreme values prone to causing logic errors;
    # could be data entry error or improbable class assignment
    #
    # with irregular bounds, high SD could be intentional/desired -- if unstable
    #  - enforcing some sort of sorting?
    #  - additional random processes: waves for wavy/irregular
    #  - allowing irregular boundaries to eclipse/omit thin layers?
    #  - presence/absence of broken horizons; could this be a separate random process?
    #    would it require that volume or other % area field populated?

    idx <- 1
    counter <- 0
    # TODO: do better
    while(length(idx) > 0) {
      if(counter > 1000) {
        break
      }
      idx <- which(new <= perturb_var[pmax(1, i - 1)] |
            new >= perturb_var[pmin(i + 1, length(perturb_var))])
      new[idx] <- rnorm(length(idx), perturb_var[i], bounds[i])
      counter <- counter + 1
    }

    # finally, no depths should be negative
    return(pmax(new, 0))
  }))

  # aqp only supports integer depths
  res <- round(res)
  
  # handle minimum thickness 
  if (!by_thickness) {
    res <- apply(res, 2, function(x) diff(c(mindepth, x)))
  }
  
  res <- apply(res, 2, function(x) {
    idx <- x < min.thickness
    if (any(idx)) {
      x[idx] <- min.thickness
    }
    return(cumsum(x) + mindepth)
  })
  
  res <- round(res)

  # allocate a list for n-profile result
  pID <- 1:n
  profiles <- vector('list', n)

  profiles <- lapply(pID, function(i) {
    p.sub <- hz

    # create new idname and hzidname
    p.sub$pID <- as.character(i)
    p.sub$hzID <- as.character(1:length(p.sub$hzID) * i)

    # insert new depths
    nd <- (res[,i])     #TODO: sort is rarely needed, ensures topological
                        #      but not statistical correctness?
    p.sub[[depthz[1]]] <- c(mindepth, nd)[1:nrow(p)]
    p.sub[[depthz[2]]] <- nd

    test <- hzDepthTests(p.sub[[depthz[1]]], p.sub[[depthz[2]]])
    if(any(test)) {
      stop(paste("one or more horizon logic tests failed for realization:", i))
    }

    return(p.sub)
  })

  # fast "pbindlist" with no checks since we know the origin

  # horizon
  o.h <- as.data.frame(data.table::rbindlist(profiles))
  # need to remove duped ID from horizon table! only allowed for current ID
  o.h[[idname(p)]] <- NULL
  names(o.h)[which(names(o.h) == 'pID')] <- new.idname

  # site
  o.s <- data.frame(site(p), pID = pID, row.names = NULL)
  names(o.s)[which(names(o.s) == 'pID')] <- new.idname

  # diagnostic
  d <- diagnostic_hz(p)
  o.d <- data.frame()
  if (length(d) != 0) {
    o.d <- data.frame(pID = do.call('c', lapply(pID, rep, nrow(d))), 
                      d[rep(1:nrow(d), length(pID))])
  }
  names(o.d)[which(names(o.d) == 'pID')] <- new.idname

  # restriction
  re <- restrictions(p)
  o.r <- data.frame()
  if (nrow(re) > 0) {
    o.r <- data.frame(pID = do.call('c', lapply(pID, rep, nrow(re))),
                      re[rep(1:nrow(re), length(pID))])
  }
  names(o.r)[which(names(o.r) == 'pID')] <- new.idname

  # always drop spatial data -- still present in site
  o.sp <- new('SpatialPoints')

  metadat <- metadata(p)

  # TODO: alter metadata to reflect the processing done here?

  # drop old ID name from horizon table
  #(can't have it in site and horizon, not the ID any mores)
  o.h[[idname(p)]] <- NULL

  res <- SoilProfileCollection(idcol = new.idname, depthcols = horizonDepths(p),
                               metadata = metadat,
                               horizons = o.h, site = o.s, sp = o.sp,
                               diagnostic = o.d, restrictions = o.r)
  ## reset horizon IDs
  hzID(res) <- as.character(1:nrow(res))

  # preserve original horizon designation
  hzdesgnname(res) <- hzdesgnname(p)
  
  if (custom.ids & length(unique(id)) == length(res))
    profile_id(res) <- id

  return(res)
}

permute_profile <- function(p,
                            n = 100,
                            id = NULL,
                            boundary.attr = NULL,
                            min.thickness = 1,
                            soildepth = NULL,
                            new.idname = 'pID') {
  .Deprecated("perturb")
  
  perturb(p, n = n, id = id, 
          boundary.attr = boundary.attr, 
          min.thickness = min.thickness, 
          max.depth = soildepth, 
          new.idname = new.idname)
}

## compare permute_profile and sim, using same estimated SD

# calculate permuations of bottom depths using boundary deviations
#system.time(res <- perturb(p, n=1000, boundary_attr = "bound_sd",
#                                    min.thickness = 1))
#
# # calculate permuations of horizon thickness using horizon thickness deviation
# system.time(res2 <- perturb(p, n=1000, hz.sd = p$bound_sd, min.thick = 1))
#
# # superficially similar output
# plot(res[1:10,])
# plot(res2[1:10,])
#
# # compare slab'd result
# s.res <- slab(res, ~ prop, slab.structure = 1)
# s.res2 <- slab(res2, ~ prop, slab.structure = 1)

# inspect differences visually -- bigger differences in thicker horizons
#  applying same SD to horizon thickness as bottom depth results in more variation between realizations

# .:. thick horizons can still have abrupt boundaries, e.g. you might have a clear or gradual boundary at upper part, and abrupt at contact -- these should be handled seperately. a single SD would be useful for simulating aggregate data. the gaussian assumption is not as bad when applied to a single boundary. presumably other functions could be used to show skewed distributions.
# plot(x=s.res$p.q50, y=s.res$top, ylim=c(100,0), type="l")
# lines(x=s.res$p.q5, y=s.res$top, ylim=c(100,0), col="blue", lty=2)
# lines(x=s.res$p.q95, y=s.res$top, ylim=c(100,0), col="blue", lty=2)
#
# lines(x=s.res2$p.q50, y=s.res2$top, ylim=c(100,0), type="l", lwd=2)
# lines(x=s.res2$p.q5, y=s.res2$top, ylim=c(100,0), col="green", lty=2)
# lines(x=s.res2$p.q95, y=s.res2$top, ylim=c(100,0), col="green", lty=2)
#
# hzdesgnname(res) <- "name"
# hztexclname(res) <- "texture"
# res$clay <- res$prop
# mtr <- profileApply(res[1:1000], mollic.thickness.requirement)
# plot(hist(mtr, breaks = max(3, max(mtr) - min(mtr))))
