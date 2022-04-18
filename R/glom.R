setGeneric("glom", function(p, z1, z2 = NULL,
                            ids = FALSE, df = FALSE,
                            truncate = FALSE, invert = FALSE, fill = FALSE,
                            modality = "all")
  standardGeneric("glom"))

#' Subset soil horizon data using a depth or depth interval
#'
#' @param p a SoilProfileCollection
#' @param z1 numeric vector of top depth to intersect horizon (required). Can be an expression involving `siteNames(p)` or quoted column name. Should evaluate to numeric length `1` or length equal to `length(p)`
#' @param z2 numeric vector bottom depth of intersection interval (optional). Can also be an expression involving `siteNames(p)` or quoted column name. Should evaluate to numeric length `1`, length equal to `length(p)` or `NULL`. Default: `NULL` is "point" intersection 
#' @param ids return only horizon IDs? default: `FALSE`
#' @param df return a data.frame, by intersection with `horizons(p)`? default: `FALSE`
#' @param truncate truncate horizon top and bottom depths to `z1` and \code{z2}? default: `FALSE`
#' @param invert get horizons _outside_ the interval `[z1,z2]`? default: `FALSE`
#' @param fill keep sites and preserve order for profiles that do not have horizons in interval by filling with a single horizon with `NA` top and bottom depth. default: `FALSE`
#' @param modality default: `"all"` return all horizons; or `modality = "thickest"`) to return the _thickest_ horizon in interval. If multiple horizons have equal thickness, the first (shallowest) is returned.
#'
#' @description Make a "clod" of horizons from a SoilProfileCollection given a point or a depth interval to intersect. The interval `[z1,z2]` may be profile-specific (equal in length to `p`), or may be recycled over all profiles (if boundaries are length 1). For "point" intersection, `z2` may be left as the default value `NULL`. 
#'
#' @details "To glom" is "to steal" or to "become stuck or attached to". The word is related to the compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil.
#' 
#' The full depth range of horizons included within the interval are returned (a "ragged" SoilProfileCollection) unless the `truncate` argument is set as `TRUE`. Horizon intersection is based on unique ID \code{hzidname(spc)} and depth range of interest. Profiles that lack data in the range of interest will be dropped from the resulting SoilProfileCollection. 
#'
#' If inverting results with `invert`, it is possible that thick horizons (whose boundaries span wider than the specified interval) will be split into _two_ horizons, where previously they were one. This may make the results from `ids = TRUE` different from what you expect, as they will be based on a profile with an "extra" horizon and re-calculated unique horizon ID (`hzidname(spc)`) `"hzID"`.
#'
#' @seealso \code{\link{glomApply}} \code{\link{trunc}}
#'
#' @author Andrew G. Brown
#'
#' @return a SoilProfileCollection, data.frame, or a vector of horizon IDs. \code{NULL} if no result.
#'
#' @export glom
#' @aliases glom
#'
#' @examples
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- glom(sp1, 25, 150)
#'
#' # 28 horizons
#' nrow(p) 
#' 
#' # inspect graphically
#' par(mar = c(1,1,3,1))
#' plot(p, color = "prop", max.depth = 200)
#' abline(h = c(25, 100), lty = 2)
#' 
#' ## glom(..., truncate = TRUE)
#' 
#' p2 <- glom(sp1, 25, 150, truncate = TRUE)
#'
#' # 28 horizons
#' nrow(p2) 
#' 
#' # inspect graphically
#' par(mar = c(1,1,3,1))
#' plot(p2, color = "prop", max.depth = 200)
#' abline(h = c(25, 100), lty = 2)
#' 
#' ## glom(..., truncate = TRUE, invert = TRUE)
#' 
#' p3 <- glom(sp1, 25, 150, truncate = TRUE, invert = TRUE)
#'
#' # 45 horizons
#' nrow(p3) 
#' 
#' # inspect graphically
#' par(mar = c(1,1,3,1))
#' plot(p3, color = "prop", max.depth = 200)
#' abline(h = c(25, 100), lty = 2)
#' 
#' ## profile-specific interval, using expressions evaluated within sp1@site
#' 
#' # calculate some new site-level variables containing target interval
#' sp1$glom_top <- (1:9) * 10
#' sp1$glom_bottom <- 10 + sp1$glom_top
#' 
#' # glom evaluates non-standard expressions using siteNames(sp1) column names
#' p4 <- glom(sp1, glom_top / 2, glom_bottom * 1.2, truncate = TRUE)
#' 
#' # inspect graphically
#' par(mar = c(1,1,3,1))
#' plot(p4, color = "prop", max.depth = 200)
#' 
setMethod(f = 'glom', signature(p = 'SoilProfileCollection'),
          function(p,
                   z1,
                   z2 = NULL,
                   ids = FALSE,
                   df = FALSE,
                   truncate = FALSE,
                   invert = FALSE,
                   fill = FALSE,
                   modality = "all") {
            
  
  # handle unquoted symbols from @site column names (or expressions using them)
  zz <- eval(substitute(list(z1, z2)),
             envir = site(p), 
             enclos = parent.frame())
  
  z1 <- zz[[1]]
  z2 <- zz[[2]]
  
  # handle _quoted_ character column names from @site for z1/z2
  if (is.character(z1) && length(z1) == 1) {
    if (z1 %in% siteNames(p)) {
      sitez1 <- p[[z1]]
      if (!is.numeric(sitez1))
        stop("If `z1` is a column name, must refer to a numeric column in `siteNames(p)`")
      z1 <- sitez1
    }
  }
  if (is.character(z2) && length(z2) == 1) {
    if (z2 %in% siteNames(p)) {
      sitez2 <- p[[z2]]
      if (!is.numeric(sitez2))
        stop("If `z2` is a column name, must refer to a numeric column in `siteNames(p)`")
      z2 <- sitez2
    }
  }
  
  depthn <- horizonDepths(p) 
  fillids <- profile_id(p)
  
  # recycle z1 to size of SPC if length is 1
  if (length(z1) == 1) {
    z1 <- rep(z1, length(p))
  
  # error if z1 does not match number of profiles
  } else if (length(z1) != length(p)) {
    stop("`z1` should have length 1 or length equal to number of profiles in `p`", call. = FALSE)
  }
  
  if (is.null(z2)) {
    z2 <- z1
  } else if (length(z2) == 1) {
    z2 <- rep(z2, length(p))
  }
  
  if (length(z2) != length(z1)) {
    stop("`z2`, if specified, should have same length as `z1`", call. = FALSE)
  }
  
  # handle bad z logic
  bad.idx <- which(z2 < z1)
  if (length(bad.idx) > 0) {
    warning("`z2` smaller than `z1`; setting top and bottom to `NA`")
    z1[bad.idx] <- NA
    z2[bad.idx] <- NA
  }
  
  if (!invert) {
    idx <- .multiglomDT(p, z1 = z1, z2 = z2, modality = modality)
  } else {
    idx <- c(.multiglomDT(p, min(p[[depthn[1]]], na.rm = TRUE), z1, modality = "all"),
             .multiglomDT(p, z2, max(p[[depthn[2]]], na.rm = TRUE), modality = "all"))
  }

  h <- horizons(p)[which(hzID(p) %in% idx), ]
  id <- idname(p)
  
  # truncate ragged edges
  if (!invert & truncate & !is.null(z2)) {
    
    pdepths <- data.table::data.table(id = as.character(h[[id]]),
                                      tdep = h[[depthn[1]]],
                                      bdep = h[[depthn[2]]])
    
    glomdepths <- data.table::data.table(id = profile_id(p),
                                         z1 = z1,
                                         z2 = z2)
    
    # join and remove horizons that are not part of interval
    newhz <- pdepths[glomdepths, on = "id"]
    newhz <- newhz[complete.cases(newhz),]
    
    # truncate upper bounds to z1
    z1tidx <- newhz[,.I[newhz$tdep < z1]]
    z1bidx <- newhz[,.I[newhz$bdep < z1]]
    newhz[z1tidx, 'tdep'] <- newhz[z1tidx, 'z1']
    newhz[z1bidx, 'bdep'] <- newhz[z1bidx, 'z1']
    
    # truncate lower bounds to z2
    z2tidx <- newhz[,.I[newhz$tdep > z2]]
    z2bidx <- newhz[,.I[newhz$bdep > z2]]
    newhz[z2tidx, 'tdep'] <- newhz[z2tidx, 'z2']
    newhz[z2bidx, 'bdep'] <- newhz[z2bidx, 'z2']

    # replace original values with truncated 
    h[[depthn[1]]] <- newhz$tdep
    h[[depthn[2]]] <- newhz$bdep

  # or "invert" and truncate
  } else if (invert & truncate & !is.null(z2)) {
    p0 <- p

    .top <- h[[depthn[1]]]
    .bottom <- h[[depthn[2]]]

    # create an upper and lower SPC
    p1 <- glom(p, 0, z1, truncate = TRUE)
    p2 <- glom(p, z2, max(p[[depthn[2]]]), truncate = TRUE)

    # combine horizon data
    if(inherits(p1, 'SoilProfileCollection') && 
       inherits(p2, 'SoilProfileCollection')) {
      
      if (nrow(p1) > 0) {
        p <- p1
        if(inherits(p2, 'SoilProfileCollection') && nrow(p2) > 0) {
          h <- rbind(horizons(p1), horizons(p2))
        } else {
          h <- horizons(p1)
        }
      } 
    } else {
      h <- horizons(p2)
    }

    if (modality == "thickest") {
      first.thickest.idx <- .thickestHzID(p, h)
      h <- h[first.thickest.idx,]
    }
    
    # enforce ID+top depth order after combining upper and lower SPCs
    h <- h[order(h[[idname(p)]], h[[depthn[1]]]), ]          
    
    # replace horizon IDs because we split horizons possibly
    h$hzID <- as.character(seq_len(nrow(h)))
    
    # force autocalculated ID
    hzidname(p) <- "hzID"
    
    if (fill) {
      p <- p0
    }
  }
  
  if (fill) {
    missingid <- fillids[!fillids %in% h[[idname(p)]]]
    if (length(missingid) > 0) {
      hmissing <- h[0,][1:length(missingid),]
      hmissing[[idname(p)]] <- missingid
      h <- rbind(h, hmissing)
    }
    
    # enforce ID+top depth order after combining upper and lower SPCs
    h <- h[order(h[[idname(p)]], h[[depthn[1]]]), ]          
    
    # replace horizon IDs because we split horizons possibly
    h$hzID <- as.character(seq_len(nrow(h)))
    
    # force autocalculated ID
    hzidname(p) <- "hzID"
  } else {
    # update slots for new h
    p <- .updateSite(p, h)
  }

  if (nrow(h) > 0) {
    # replace @horizons with h
    replaceHorizons(p) <- h
  } else {
    p <- p[0,]
  }
  
  # short circuit to get hzIDs of result
  if (ids) {
    if (invert)
      warning("invert = TRUE may split horizons, which affects resulting ids when ids=TRUE!",
              call. = FALSE)
    return(idx)
  }

  if (!df) {
    # verify updating of non-horizons slots worked
    if (!spc_in_sync(p)$valid) {
      warning("invalid SPC created by glom result!", call. = FALSE)
    }
    # return SPC
    return(p)
  } else {
    # return data.frame
    return(h)
  }
})

# helper functions for creating new horizon and paired SPC data
.thickestHzID <- function(p, newhz = horizons(p)) {
  
  # appease R CMD check
  .thk <- NULL
  
  htb <- horizonDepths(p)
  # make a data.table with id and thickness (.thk)
  res <- data.table::data.table(id = newhz[[idname(p)]], .thk = newhz[[htb[2]]] - newhz[[htb[1]]])
  # get the row index where the thickness is maximized (by profile ID)
  res[, .I[.thk >= suppressWarnings(max(.thk))], by = "id"]$V1
}

.updateSite <- function(p, newhz = horizons(p)) {
  # profile_id relies on p@horizons. we are testing new horizons data.frame
  keep <- unique(newhz[[idname(p)]])
  
  # keep only relevant site and other slots using SPC[i, ]
  p <- p[which(site(p)[[idname(p)]] %in% keep), ]
  return(p)
}

# new workhorse function replacing .glom
# much less fussy, and vectorized
.multiglom <- function (p, z1, z2 = NULL, modality = "all", as.list = FALSE) {

  # access SPC slots to get important info about p
  hz <- horizons(p)
  dz <- horizonDepths(p)
  id <- idname(p)
  pid <- profile_id(p)
  hzid <- hzidname(p)

  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]

  # determine top depths greater/equal to z1
  # include horizons whose bottom portion are below z1
  gt1 <- tdep >= z1 | (bdep > z1)

  # get the index of the first horizon
  idx.top <- which(gt1)

  if (length(idx.top) == 0) {
    warning(paste0('Invalid upper bound `z1` (',z1,'), returning `NULL` (',
                   id,': ', pid,')'), call.=FALSE)
    return(NULL)
  }

  # if a bottom depth of the clod interval is not specified
  if (is.null(z2)) {
    z2 <- z1
  }

  # determine bottom depths less than z2
  lt2 <- bdep < z2

  # include bottom depths equal to z2
  lt2[which(bdep == z2)] <- TRUE

  # include horizons whose top portion are above z2
  lt2 <- lt2 | (tdep < z2)

  # get index of last horizon
  idx.bot <- which(lt2)

  # intersection of idx.top and idx.bot
  idx <- intersect(idx.top, idx.bot)

  if (modality == "thickest") {
    # get index of thickest horizon by profile
    idx <- .thickestHzID(p, hz[idx, ])
  }

  # get the ID values out of horizon table
  idval <- hz[idx, ][[hzidname(p)]]

  # just the horizon IDs
  if (!as.list) {
    return(idval)
  }

  # # list result.
  return(list(
    hzid = hzid,
    hz.idx = idx,
    value = idval
  ))
}

.multiglomDT <- function (p, z1, z2 = NULL, modality = "all", as.list = FALSE) {
  
  # access SPC slots to get important info about p
  hz <- data.table::as.data.table(horizons(p))
  dz <- horizonDepths(p)
  id <- idname(p)
  pid <- profile_id(p)
  hzid <- hzidname(p)
  
  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]
  
  pdepths <- data.table::data.table(id = as.character(hz[[id]]),
                                    tdep = tdep,
                                    bdep = bdep)
  
  glomdepths <- data.table::data.table(id = profile_id(p),
                                       z1 = z1,
                                       z2 = z2)
  
  if (is.null(z2))
    z1 <- z2
  
  idx <- pdepths[glomdepths, on = "id"][,.I[((tdep >= z1) | 
                                              (bdep > z1)) & 
                                              ((bdep < z2) |
                                              (bdep == z2) | 
                                              (tdep < z2))]]
  if (modality == "thickest") {
    # get index of thickest horizon by profile
    idx <- .thickestHzID(p, hz[idx, ])
  }
  
  # get the ID values out of horizon table
  idval <- hz[idx, ][[hzidname(p)]]
  
  # just the horizon IDs
  if (!as.list) {
    return(idval)
  }
  
  # # list result.
  return(list(
    hzid = hzid,
    hz.idx = idx,
    value = idval
  ))
}
