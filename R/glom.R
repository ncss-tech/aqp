setGeneric("glom", function(p, z1, z2 = NULL,
                            ids = FALSE, df = FALSE,
                            truncate = FALSE, invert = FALSE,
                            modality = "all")
  standardGeneric("glom"))

#' Subset soil horizon data using a depth or depth interval
#'
#' @param p A SoilProfileCollection
#' @param z1 Top depth (required) - depth to intersect horizon; if 'z2' specified, top depth of intersection interval.
#' @param z2 optional: bottom depth of intersection interval
#' @param ids Return only horizon IDs? default: `FALSE`
#' @param df Return a data.frame, by intersection with \code{horizons(p)}? default: `FALSE`
#' @param truncate Truncate horizon top and bottom depths to \code{z1} and \code{z2}? default: `FALSE`
#' @param invert Get the horizons ranges outside the interval z1/z2? default: `FALSE`
#' @param modality Return all horizons (default: \code{"all"}) or first, _thickest_ (\code{modality = "thickest"}) horizon in interval.
#'
#' @description \code{glom()} returns a "clod" of horizons from a SoilProfileCollection from a depth interval.
#'
#' All horizons included within the specified interval are returned in their entirety (not just the portion within the interval), unless the \code{truncate} argument is specified. Horizon intersection is based on unique ID \code{hzidname(spc)} and attribute of interest.
#'
#' If intersection at the specified boundaries \code{['z1', 'z2']} results in no horizon data, 'NULL' is returned with a warning containing the offending pedon ID.
#'
#' If inverting results with \code{invert}, it is possible that thick horizons (that span more than the entire glom interval) will be split into two horizons. This may make the results from \code{ids = TRUE} different from what you expect, as they will be based on a profile with an "extra" horizon.
#'
#' @details The verb/function that creates a clod is "glom". "To glom" is "to steal" or to "become stuck or attached to". The word is related to the compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil.
#'
#' @seealso \code{\link{glomApply}} \code{\link{trunc}}
#'
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection, data.frame, or a vector of horizon IDs. \code{NULL} if no result.
#'
#' @export glom
#' @aliases glom
#'
#' @examples
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1[1]
#'
#' foo <- glom(p, 25, 100)
#'
#' # there are 4 horizons in the clod glommed from depths 25 to 100 on profile 1 in sp1
#' nrow(foo) 
setMethod(f = 'glom', signature(p = 'SoilProfileCollection'),
          function(p, z1, z2 = NULL,
                 ids = FALSE, df = FALSE,
                 truncate = FALSE, invert = FALSE,
                 modality = "all") {

  depthn <- horizonDepths(p)

  if (!invert) {
    idx <- .multiglom(p, z1, z2, modality)
  } else {
    idx <- c(.multiglom(p, min(p[[depthn[1]]], na.rm = TRUE), z1, modality = "all"),
             .multiglom(p, z2, max(p[[depthn[2]]], na.rm = TRUE), modality = "all"))
  }

  h <- horizons(p)[which(hzID(p) %in% idx), ]

  # truncate ragged edges
  if (!invert & truncate & !is.null(z2)) {
    .top <- h[[depthn[1]]]
    .bottom <- h[[depthn[2]]]

    .top[.top < z1] <- z1
    .bottom[.bottom < z1] <- z1

    .top[.top > z2] <- z2
    .bottom[.bottom > z2] <- z2

    h[[depthn[1]]] <- .top
    h[[depthn[2]]] <- .bottom

  # or "invert" and truncate
  } else if (invert & truncate & !is.null(z2)) {

    .top <- h[[depthn[1]]]
    .bottom <- h[[depthn[2]]]

    # create an upper and lower SPC
    p1 <- glom(p, 0, z1, truncate = TRUE)
    p2 <- glom(p, z2, max(p[[depthn[2]]]), truncate = TRUE)

    # combine horizon data
    if(inherits(p1, 'SoilProfileCollection') && 
       inherits(p2, 'SoilProfileCollection')) {
      
      if(nrow(p1) > 0) {
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
    h$hzID <- as.character(1:nrow(h))
    
    # force autocalculated ID
    hzidname(p) <- "hzID"
  }
  
  # update slots for new h
  p <- .updateSite(p, h)

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
  id <- NULL
  
  htb <- horizonDepths(p)
  # make a data.table with id and thickness (.thk)
  res <- data.table::data.table(id = newhz[[idname(p)]], .thk = newhz[[htb[2]]] - newhz[[htb[1]]])
  # get the row index where the thickness is maximized (by profile ID)
  res[, .I[.thk >= suppressWarnings(max(.thk))], by = id]$V1
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
