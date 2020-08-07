# 2018-12-17
# D.E. Beaudette
# A.G. Brown
#
#
# This function replaces the previous rbind.SoilProfileCollection function.
#
# TODO:
# * is it possible to implement an S4 interface for a list of SPC? (cleaner code)


# # ease the transition to union()
#' [DEPRECATED] rbind.SoilProfileCollection
#'
#' @param ... One or more SoilProfileCollection objects to \code{union()}
#'
#' @return A SoilProfileCollection
#' @export rbind.SoilProfileCollection
#'
#' @examples
#'
#' data(sp5)
#'
#' # rbind is deprecated, use aqp::union()
#' # rbind(sp5[1:2,], sp5[(length(sp5) - 1):length(sp5),])
#'
rbind.SoilProfileCollection <- function(...) {
  .Deprecated('please use union()')

  # parse dots
  objects <- list(...)
  names(objects) <- NULL

  # make compatible
  res <- union(spc = objects)
  return(res)
}

union <- function(spc=list(), method='all', na.rm=TRUE, drop.spatial=FALSE) {
  # setup some defaults
  options(stringsAsFactors=FALSE)

  # short-circuits

  # empty list
  if(length(spc) == 0)
    return(NULL)

  # singleton
  if(length(spc) == 1)
    return(spc[[1]])

  # ALL NULL
  if(all(sapply(spc, is.null)))
    return(NULL)

  # ALL NA
  # must suppress warnings because is.na(SoilProfileCollection) throws a warning
  if(suppressWarnings(all(sapply(spc, is.na))))
    return(NULL)

  # check/filter for NULL list elements
  idx.null <- suppressWarnings(which(sapply(spc, is.null)))
  if(length(idx.null)) {
    spc <- spc[-idx.null]
    message("union: one or more input list elements is NULL")
  }

  # check/filter for NA list elements
  idx.na <- suppressWarnings(which(sapply(spc, is.na)))
  if(length(idx.na) & na.rm) {
    spc <- spc[-idx.na]
    message("union: one or more input list elements is NA")
  } else if(length(idx.na)) {
    stop("union: one or more input list elements is NA and na.rm=FALSE")
  }

  idx.notspc <- which(!sapply(spc, inherits, 'SoilProfileCollection'))
  if(length(idx.notspc)) {
    spc <- spc[-idx.notspc]
    message("union: one or more input list elements is not a SoilProfileCollection")
  }

  # check for non-conformal depth units
  o.depth.units <- unique(lapply(spc, depth_units))
  if(length(o.depth.units) > 1)
    stop('inconsistent depth units', call.=FALSE)

  # check for non-conformal df class (revert to data.frame if non-matching)
  o.df.class <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_df_class)))
  if(length(o.df.class) > 1) {
    message("data.frame class type inconsistent, reset to \"data.frame\"")
    o.df.class <- "data.frame"
  }

  # check for non-conformal group (revert to "" if non-matching)
  o.group.by <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_group_by)))
  if(length(o.group.by) != 1) {
    o.group.by <- ""
  }

  # check for non-conformal hzdesgn (revert to data.frame if non-matching)
  o.hzdesgn <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_hzdesgn)))
  if(length(o.hzdesgn) != 1) {
    o.hzdesgn <- ""
  }

  # check for non-conformal hztexcl (revert to data.frame if non-matching)
  o.hztexcl <- unique(unlist(lapply(spc, function(s) metadata(s)$aqp_hztexcl)))
  if(length(o.hztexcl) != 1) {
    o.hztexcl <- ""
  }

  # test for non-conformal CRS if keeping spatial data
  if(!drop.spatial) {
    # check for non-conformal CRS in @sp
    o.p4s <- unique(lapply(spc, function(x) suppressWarnings(proj4string(x))))
    if(length(o.p4s) > 1)
      stop('inconsistent CRS', call.=FALSE)
  }

  # decompose to list of lists
  spc.list <- lapply(spc, as, 'list')

  # number of SPCs
  n.spc <- length(spc.list)
  # total profiles
  n.profiles <- sum(sapply(spc, length))

  # template for combined data is based on the first element
  new.pID <- spc.list[[1]]$idcol
  new.hzID <- spc.list[[1]]$hzidcol

  new.hzd <- spc.list[[1]]$depthcols
  new.metadata <- spc.list[[1]]$metadata

  # get the data.frame class and grouping variable into new metadata
  new.metadata$aqp_df_class <- o.df.class
  new.metadata$aqp_group_by <- o.group.by

  # transfer old slots if they are present
  hzdold <- spc.list[[1]]$hzdesgncol
  hztold <- spc.list[[1]]$hztexclcol
  if(length(hzdold) == 1)
   new.metadata$aqp_hzdesgn  <- hzdold
  if(length(hztold) == 1)
   new.metadata$aqp_hztexcl  <- hztold

  if(!length(new.metadata$aqp_hzdesgn))
    new.metadata$aqp_hzdesgn <- ""

  if(!length(new.metadata$aqp_hzdesgn))
    new.metadata$aqp_hztexcl <- ""

  # TODO: need a template for coordinate names if spatial data are present in all

  # reset profile ID names in all other objects
  # also reset depth names
  for(i in 2:n.spc) {
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
    if(old.pID %in% names(spc.list[[i]]$diagnostic)) {
      idx <- match(old.pID, names(spc.list[[i]]$diagnostic))
      names(spc.list[[i]]$diagnostic)[idx] <- new.pID
    }

    # profile ID in restriction, may be missing
    if(old.pID %in% names(spc.list[[i]]$restriction)) {
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
  if(requireNamespace("data.table")) {
    # preferentially use data.table if available
    o.h <- data.table::rbindlist(o.h, fill = TRUE)
    o.s <- data.table::rbindlist(o.s, fill = TRUE)
    o.d <- data.table::rbindlist(o.d, fill = TRUE)
    o.r <- data.table::rbindlist(o.r, fill = TRUE)
  } else if(requireNamespace("plyr")) {
    o.h <- do.call('rbind.fill', o.h) # horizon data
    o.s <- do.call('rbind.fill', o.s) # site data
    o.d <- do.call('rbind.fill', o.d) # diagnostic data, leave as-is
    o.r <- do.call('rbind.fill', o.r) # restriction data, leave as-is
  } else {
    stop("package `data.table` or `plyr` is required to union nonconformal data.frames.", call.=FALSE)
  }
  if(! drop.spatial) {
    # check for non-conformal coordinates
    dim.coords <- sapply(o.sp, function(i) {
      ncol(sp::coordinates(i))
    })

    # note: an SPC with default @sp is a 1 column matrix
    if(length(unique(dim.coords)) > 1) {
      stop('non-conformal point geometry', call. = FALSE)
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
    o.1.sp <- spc.list[[1]]$sp

    # missing spatial data
    if(ncol(coordinates(o.1.sp)) == 1) {
      o.sp <- o.1.sp # copy the first filler
    } else {
      # not missing spatial data
      # 2015-12-18: added call to specific function: "sp::rbind.SpatialPoints"
      o.sp <- suppressMessages(do.call("rbind.SpatialPoints", o.sp))
    }

  } else {
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
  
  # resorts for case when "joining" only ID (which is only column in site after above line) and depths<- resorted the horizon data
  #  site and horizons<- allow short circuiting to ensure that site(x)$<- and horizons(x)$<- work
  site(res) <- .as.data.frame.aqp(o.s, o.df.class)[ match(.coalesce.idx(horizons(res)[[idname(res)]]), o.s[[new.pID]]),]
  
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
  return(res)
}

