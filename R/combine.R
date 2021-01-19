# 2018-12-17
# D.E. Beaudette
# A.G. Brown
#
#
# This function replaces the previous rbind.SoilProfileCollection function.
#
# TODO:
# * is it possible to implement an S4 interface for a list of SPC? (cleaner code)

# 2020/09/25: yes it is possible [for a list] with the caveat that the y argument _must_ be missing in order to use the base::union generic but dispatch to the aqp method.
# The problem with this is IDEs like Rstudio that probe the generics to identify missing required arguments will erroneously warn about the aqp-usage of union

#' @title (DEPRECATED) union
#'
#' @param x a list of SoilProfileCollection objects
#' @param y Necessary for proper S4 dispatch.
#'
#' @return a SoilProfileCollection
#' @export union
#' @seealso \link{pbindlist}
#'
#' @examples
#'
#' # deprecated, see aqp::combine and aqp::pbindlist
#'
setMethod("union", signature(x = "list", y = "missing"), function(x, y)  {
  .Deprecated("combine")
  pbindlist(x)
})

if (!isGeneric("combine"))
  setGeneric("combine", function(...)
    standardGeneric("combine"))

#' Combine SoilProfileCollection objects
#' 
#' Combine SoilProfileCollection objects or lists of SoilProfileCollection objects. This method provides \code{...} expansion for the \code{pbindlist} method. 
#'
#' @param ... SoilProfileCollection objects or lists of SoilProfileCollection objects to combine
#'
#' @return A SoilProfileCollection
#' 
#' @export
#' @aliases combine
#' @examples
#' 
#' spc1 <- random_profile(1, SPC = TRUE)
#' spc2 <- random_profile(2, SPC = TRUE)
#' 
#' spc <- combine(spc1, spc2)
#' 
setMethod("combine", signature(... = "SoilProfileCollection"), function(...)  {
    objects <- list(...)
    # handle single-list input
    if (length(objects) == 1)
      if (is.list(objects[[1]]))
        objects <- objects[[1]]
    names(objects) <- NULL
    res <- pbindlist(objects)
    return(res)
})

#' @export
#' @rdname combine-SoilProfileCollection-method
setMethod("combine", signature(... = "list"), function(...)  {
  lists <- list(...)
  objects <- do.call('c', lists)
  res <- pbindlist(objects)
  return(res)
})

# TODO: when dplyr::combine() is gone, also define aqp::combine(LIST)?

#' Combine a list of SoilProfileCollection objects
#'
#' See \code{combine(...)} for a connotative short-hand method that does not require that SoilProfileCollections be in a list. Profiles will be sorted based on character sorting of profile ID.
#'
#' @param l a list of SoilProfileCollection objects
#' @param new.idname Optional: a character referring to a new column name to put unique profile IDs in; default: \code{NULL} to attempt with existing idname in first element
#' @param verbose Produce warnings and messages regarding results? default: \code{TRUE}
#'
#' @details Input data must share a common depth unit, and if spatial data are present, a common CRS and coordinate names. In the case of non-conformal @idname and/or @depthcols, the first SoilProfileCollection is used as a template. If one or more subsequent list elements has non-unique values in a site level attribute of that name, the ID name from the second list element is attempted, and so on. Non-conforming spatial data are dropped from the final result (returns default empty \code{SpatialPoints}).
#'
#' @return a SoilProfileCollection object
#' @author D.E. Beaudette and A.G. Brown
#'
#' @examples
#' # example data
#' data(sp2, package = 'aqp')
#' depths(sp2) <- id ~ top + bottom
#' site(sp2) <- ~ surface
#'
#' # copy pieces
#' x <- sp2[1:5, ]
#' y <- sp2[6:10, ]
#'
#' # reset IDs and combine
#' profile_id(y) <- sprintf("%s-copy", profile_id(y))
#'
#' # this should work
#' z <- pbindlist(list(x, y))
#'
#' # check
#' plot(z)
pbindlist <- function(l, new.idname = NULL, verbose = TRUE) {
  spc <- l

  # setup some defaults
  options(stringsAsFactors = FALSE)

  # short-circuits

  # empty list
  if (length(spc) == 0)
    return(NULL)

  # singleton
  if (length(spc) == 1)
    return(spc[[1]])

  # ALL NULL
  if (all(sapply(spc, is.null)))
    return(NULL)

  # ALL NA
  # must suppress warnings because is.na(SoilProfileCollection) throws a warning
  if (suppressWarnings(all(sapply(spc, is.na))))
    return(NULL)

  # check/filter for NULL list elements
  idx.null <- suppressWarnings(which(sapply(spc, is.null)))
  if (length(idx.null)) {
    spc <- spc[-idx.null]
    if (verbose)
      message("pbindlist: one or more input list elements is NULL")
  }

  # check/filter for NA list elements
  idx.na <- suppressWarnings(which(sapply(spc, is.na)))
  if (length(idx.na)) {
    spc <- spc[-idx.na]
    if (verbose)
      message("pbindlist: one or more input list elements is NA")
  }

  idx.notspc <- which(!sapply(spc, inherits, 'SoilProfileCollection'))
  if (length(idx.notspc)) {
    spc <- spc[-idx.notspc]
    if (verbose)
      message("pbindlist: one or more input list elements is not a SoilProfileCollection")
  }

  # short circuit:
  # stop here if the removal of NULL | NA | non-SPC has resulted in a singleton list
  if (length(spc) == 1) {
    return(spc[[1]])
  }

  # check for non-conformal depth units
  o.depth.units <- unique(lapply(spc, depth_units))
  if (length(o.depth.units) > 1)
    stop('inconsistent depth units', call. = FALSE)

  # check for non-conformal df class (revert to data.frame if non-matching)
  o.df.class <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_df_class)))
  if (length(o.df.class) > 1) {
    if (verbose)
      message("pbindlist: data.frame class type inconsistent, reset to \"data.frame\"")
    o.df.class <- "data.frame"
  }

  # check for non-conformal group (revert to "" if non-matching)
  o.group.by <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_group_by)))
  if (length(o.group.by) != 1) {
    o.group.by <- ""
  }

  # check for non-conformal hzdesgn (revert to data.frame if non-matching)
  o.hzdesgn <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_hzdesgn)))
  if (length(o.hzdesgn) != 1) {
    o.hzdesgn <- ""
  }

  # check for non-conformal hztexcl (revert to data.frame if non-matching)
  o.hztexcl <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_hztexcl)))
  if (length(o.hztexcl) != 1) {
    o.hztexcl <- ""
  }

  o.p4s <- unique(lapply(spc, function(x) suppressWarnings(proj4string(x))))

  drop.spatial <- FALSE
  if (length(o.p4s) > 1) {
    if (verbose)
      message('pbindlist: inconsistent CRS, dropping spatial data')
    drop.spatial <- TRUE
  }

  # decompose to list of lists
  spc.list <- lapply(spc, as, 'list')

  # number of SPCs
  n.spc <- length(spc.list)

  # total profiles [previously defined, but not used]
  # n.profiles <- sum(sapply(spc, length))

  # template for combined data is based on the first element
  new.pID <- spc.list[[1]]$idcol

  # allow user to specify new idname a priori
  if (!is.null(new.idname)) {
    if (is.character(new.idname) & length(new.idname) == 1) {
      new.pID <- new.idname
    } else {
      if (verbose)
        message("ignored new.idname argument; must be character vector of length 1")
    }
  }
  new.hzID <- spc.list[[1]]$hzidcol

  new.hzd <- spc.list[[1]]$depthcols
  new.metadata <- spc.list[[1]]$metadata

  # get the data.frame class and grouping variable into new metadata
  new.metadata$aqp_df_class <- o.df.class
  new.metadata$aqp_group_by <- o.group.by

  # transfer old slots if they are present
  hzdold <- spc.list[[1]]$hzdesgncol
  hztold <- spc.list[[1]]$hztexclcol

  if (length(hzdold) == 1)
   new.metadata$aqp_hzdesgn  <- hzdold

  if (length(hztold) == 1)
   new.metadata$aqp_hztexcl  <- hztold

  if (!length(new.metadata$aqp_hzdesgn))
    new.metadata$aqp_hzdesgn <- ""

  if (!length(new.metadata$aqp_hzdesgn))
    new.metadata$aqp_hztexcl <- ""

  # TODO: need a template for coordinate names if spatial data are present in all

  # Make aqp::union() robust to profile ID / site attr collisions.
  # https://github.com/ncss-tech/aqp/issues/161
  tries <- 1
  for (i in 1:n.spc) {

    # if the template ID exists in ith SPC, check it
    if (new.pID %in% names(spc.list[[i]]$site)) {

      # get the values
      potential.pid <- spc.list[[i]]$site[[new.pID]]

      # compare length to unique length
      lupid <- length(unique(potential.pid))
      if (lupid != length(potential.pid)) {

        # warn on mismatch
        if (verbose)
          warning(sprintf("template profile ID '%s' exists as a non-unique value in SPC element #%s, trying '%s'",
                        new.pID, i, spc.list[[i]]$idcol), call. = FALSE)

        # update the template to ith IDcol
        new.pID <- spc.list[[i]]$idcol

        # increment counter (present, but nonunique)
        tries <- tries + 1

        # reset loop (check everything again)
        i <- 1
      }
    }
    if (tries >= n.spc)
      stop("result has conflicts in profile ID name, set alternate name with `new.idname` argument", call. = FALSE)
  }

  # make sure first element template has "correct" pID name and reasonable values
  splt <- spc.list[[1]]
  if (splt$idcol != new.pID) {
    snames <- colnames(splt$site)
    snames <- c(splt$idcol, snames[!snames %in% splt$idcol])
    hnames <- colnames(splt$horizons)
    hnames <- c(splt$idcol, hnames[!hnames %in% splt$idcol])
    newsite <- .data.frame.j(splt$site, snames, "data.table")
    newhorizons <- .data.frame.j(splt$horizons, hnames, "data.table")
    colnames(newsite)[1] <- new.pID
    colnames(newhorizons)[1] <- new.pID
    newhorizons[[splt$idcol]] <- NULL
    spc.list[[1]]$site <- newsite
    spc.list[[1]]$horizons <- newhorizons
  }

  # starting from the second SPC in the list
  # reset profile ID names in all other objects
  # also reset depth names
  for (i in 2:n.spc) {
    # save originals
    old.pID <- spc.list[[i]]$idcol
    old.hzd <- spc.list[[i]]$depthcols

    # profile ID in horizons
    idx <- match(old.pID, names(spc.list[[i]]$horizons))
    names(spc.list[[i]]$horizons)[idx] <- new.pID

    # profile ID in site
    idx <- match(old.pID, names(spc.list[[i]]$site))
    names(spc.list[[i]]$site)[idx] <- new.pID

    # profile ID in diagnostic, may be missing
    if (old.pID %in% names(spc.list[[i]]$diagnostic)) {
      idx <- match(old.pID, names(spc.list[[i]]$diagnostic))
      names(spc.list[[i]]$diagnostic)[idx] <- new.pID
    }

    # profile ID in restriction, may be missing
    if (old.pID %in% names(spc.list[[i]]$restriction)) {
      idx <- match(old.pID, names(spc.list[[i]]$restrictions))
      names(spc.list[[i]]$restrictions)[idx] <- new.pID
    }

    # hz depth columns
    idx <- match(old.hzd, names(spc.list[[i]]$horizons))
    names(spc.list[[i]]$horizons)[idx] <- new.hzd

    # reset id names
    spc.list[[i]]$idcol <- new.pID
    spc.list[[i]]$depthcols <- new.hzd
  }

  # extract pieces
  o.h <- lapply(spc.list, '[[', 'horizons')
  o.s <- lapply(spc.list, '[[', 'site')
  o.d <- lapply(spc.list, '[[', 'diagnostic')
  o.r <- lapply(spc.list, '[[', 'restrictions')
  o.sp <- lapply(spc.list, '[[', 'sp')

  # generate new SPC components
  # https://github.com/ncss-tech/aqp/issues/71
  if (requireNamespace("data.table")) {
    # preferentially use data.table if available
    o.h <- data.table::rbindlist(o.h, fill = TRUE)
    # fix nasty duping of names by fill by taking first column of each name
    # enforcing data.frame/tibble-like column-name uniqueness
    ohn <- colnames(o.h)
    o.h <- .data.frame.j(o.h, unique(ohn[match(ohn, ohn)]), "data.table")
    o.s <- data.table::rbindlist(o.s, fill = TRUE)
    osn <- colnames(o.s)
    o.s <- .data.frame.j(o.s, unique(osn[match(osn, osn)]), "data.table")
    o.d <- data.table::rbindlist(o.d, fill = TRUE)
    odn <- colnames(o.d)
    o.d <- .data.frame.j(o.d, unique(odn[match(odn, odn)]), "data.table")
    o.r <- data.table::rbindlist(o.r, fill = TRUE)
    orn <- colnames(o.r)
    o.r <- .data.frame.j(o.r, unique(orn[match(orn, orn)]), "data.table")
  } else if (requireNamespace("plyr")) {
    o.h <- do.call('rbind.fill', o.h) # horizon data
    o.s <- do.call('rbind.fill', o.s) # site data
    o.d <- do.call('rbind.fill', o.d) # diagnostic data, leave as-is
    o.r <- do.call('rbind.fill', o.r) # restriction data, leave as-is
  } else {
    stop("package `data.table` or `plyr` is required to combine nonconformal data.frames.", call.=FALSE)
  }

  if (!drop.spatial) {
    # check for non-conformal coordinates
    dim.coords <- sapply(o.sp, function(i) {
      ncol(sp::coordinates(i))
    })

    # note: an SPC with default @sp is a 1 column matrix
    if (length(unique(dim.coords)) > 1) {
      if (verbose)
        message('pbindlist: non-conformal point geometry, dropping spatial data')
      drop.spatial <- TRUE
    }

    ## TODO: this may not matter to rbind.SpatialPoints
    # # check for variations in coordinate names
    # coords.names <- lapply(o.sp, function(i) {
    #   dimnames(coordinates(i))[[2]]
    # })
    #
    # if(length(unique(coords.names)) > 1) {
    #   stop('non-conformal coordinate names', call. = FALSE)
    # }

    # spatial points require some more effort when spatial data are missing
    if (!drop.spatial) {
      o.1.sp <- spc.list[[1]]$sp

      # missing spatial data
      if (ncol(coordinates(o.1.sp)) == 1) {
        o.sp <- o.1.sp # copy the first filler
      } else {
        # not missing spatial data
        # 2015-12-18: added call to specific function: "sp::rbind.SpatialPoints"
        o.sp <- suppressMessages(do.call("rbind.SpatialPoints", o.sp))
      }
    }
  }

  if (drop.spatial) {
    # default NULL SpatialPoints
    o.sp <- new('SpatialPoints')
  }

  ## sanity check: profile IDs should be unique
  if (length(o.s[[new.pID]]) != length(unique(o.s[[new.pID]]))) {
    stop('non-unique profile IDs detected')
  }

  # take combined horizon data, convert to target df subclass
  res <- .as.data.frame.aqp(o.h, o.df.class)

  ## reset horizon IDs (if the default column hzID is presnt)
  if ('hzID' %in% colnames(res))
    res$hzID <- NULL

  # rebuild SPC using safe constructors
  depths(res) <- formula(sprintf("%s ~ %s + %s", new.pID, new.hzd[1], new.hzd[2]))

  site(res) <- .as.data.frame.aqp(o.s, o.df.class)

  diagnostic_hz(res) <- .as.data.frame.aqp(o.d, o.df.class)
  restrictions(res) <- .as.data.frame.aqp(o.r, o.df.class)

  # append any novel metadata
  #  note that o.df.class is calculated from all elements, and defaults to data.frame
  #  this line is to cover metadata like citations etc. that are not part of all SPCs
  new.names <- !(names(new.metadata) %in% names(res@metadata))

  # explicit slot replacement
  res@metadata <- c(res@metadata, new.metadata[new.names])

  # attempt using a common ID
  suppressWarnings(hzidname(res) <- new.hzID)

  # check validity; try to do a bandaid to fix common problems if possible
  if (!spc_in_sync(res)$valid) {
    warning("pbindlist: SoilProfileCollection integrity checks failed. This should not happen! Contact the aqp package developers with your use case on the GitHub Issue Page (https://github.com/ncss-tech/aqp/). Attempting to rebuild object...", call. = FALSE)
    res <- try(rebuildSPC(res))
  }

  # if rebuild fails then this will be a try-error
  return(res)
}

