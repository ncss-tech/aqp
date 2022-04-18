# finds all horizons that are possibly part of a cambic horizon
# excluding those that are part of an argillic horizon
# (defined either by depth interval or getArgillicBounds)

#' Find all intervals that are potentially part of a Cambic horizon
#' @description Find all intervals that are potentially part of a Cambic horizon excluding those that are part of an argillic horizon (defined either by depth interval or \code{getArgillicBounds()}).
#'
#' There may be multiple cambic horizons (indexes) in a profile. Each cambic index has a top and bottom depth associated: cambic_top and cambic_bottom. This result is designed to be used for single profiles, or with \code{profileApply(..., frameify = TRUE)}
#'
#' @param p A single-profile SoilProfileCollection
#' @param hzdesgn Column name containing horizon designations.
#' @param texcl.attr Arguments to \code{getArgillicBounds()}
#' @param clay.attr Arguments to \code{getArgillicBounds()}
#' @param ... Arguments to \code{getArgillicBounds()}
#' @param argi_bounds Optional: numeric vector of length 2 with top and bottom of argillic; (Default: NULL)
#' @param d_value Column name containing dry value. Default: d_value
#' @param m_value Column name containing moist value. Default: m_value
#' @param m_chroma Column name containing moist chroma Default: m_chroma
#' @param sandy.texture.pattern this is a pattern for matching sandy textural classes: `-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$`
#' 
#' @return A \code{data.frame} containing profile, cambic indexes, along with top and bottom depths.
#'
#' @author Andrew G. Brown
#'
#' @export
#'
#' @examples
#' # construct a fake profile
#' spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxerepts",
#'                   hzname   = c("A","AB","Bw","BC","R"),
#'                   hzdept   = c(0,  20, 32, 42,  49),
#'                   hzdepb   = c(20, 32, 42, 49, 200),
#'                   clay     = c(19, 22, 22, 21,  NA),
#'                   texcl    = c("l","l","l", "l","br"),
#'                   d_value  = c(5,   5,  5,  6,  NA),
#'                   m_value  = c(2.5, 3,  3,  4,  NA),
#'                   m_chroma = c(2,   3,  4,  4,  NA))
#'
#' # promote to SoilProfileCollection
#' depths(spc) <- id ~ hzdept + hzdepb
#' hzdesgnname(spc) <- 'hzname'
#' hztexclname(spc) <- 'texcl'
#'
#' # print results in table
#' getCambicBounds(spc)
#'
getCambicBounds <- function(p,
                            hzdesgn = guessHzDesgnName(p, required = TRUE),
                            texcl.attr = guessHzTexClName(p, required = TRUE),
                            clay.attr = guessHzAttrName(p, attr = 'clay', c("total", "_r")),
                            argi_bounds = NULL,
                            d_value = "d_value",
                            m_value = "m_value",
                            m_chroma = "m_chroma", 
                            sandy.texture.pattern = "-S$|^S$|COS$|L[^V]FS$|[^L]VFS$|LS$|LFS$",
                            ...) {

  # construct data.frame result for no-cambic-found (NA)
  empty_frame <- data.frame(id = character(0),
                            cambic_id = numeric(0),
                            cambic_top = numeric(0),
                            cambic_bottom = numeric(0))

  empty_frame_names <- names(empty_frame)
  empty_frame_names[1] <- idname(p)
  names(empty_frame) <- empty_frame_names

  hzd <- horizonDepths(p)

  if (is.null(argi_bounds)) {
    argi_bounds <- getArgillicBounds(p, hzdesgn = hzdesgn, 
                                     clay.attr = clay.attr, texcl.attr = texcl.attr, 
                                     ..., 
                                     simplify = FALSE)
  } else if (is.vector(argi_bounds)) {
    if (length(p) > 1) {
      stop("`argi_bounds` should be a data.frame if `p` has more than one profile", call. = FALSE)
    }
    argi_bounds <- data.frame(id = profile_id(p), ubound = argi_bounds[1], lbound = argi_bounds[2])
    colnames(argi_bounds)[1] <- idname(p)
  } 

  cambictop <- minDepthOf(p, pattern = "B", hzdesgn = hzdesgn, no.contact.assigned = NA, simplify = FALSE)
  cambicbottom <- maxDepthOf(p, pattern = "B", hzdesgn = hzdesgn, top = FALSE, no.contact.assigned = NA, simplify = FALSE)

  cambic <- glom(p, cambictop[[hzd[1]]], cambicbottom[[hzd[2]]], truncate = TRUE, fill = TRUE)

  if (any(!is.na(argi_bounds$ubound), !is.na(argi_bounds$lbound))) {
    # if an argillic is present, remove with glom truncate+invert
    cab <- argi_bounds[complete.cases(argi_bounds),]
    non.argillic <- glom(cambic, 
                         argi_bounds$ubound, argi_bounds$lbound, 
                         truncate = TRUE, invert = TRUE, fill = TRUE)
  } else {
    non.argillic <- cambic
  }

  dark.colors <- hasDarkColors(non.argillic)
  non.argillic$w <- rep(1, nrow(non.argillic))

  textures <- non.argillic[[texcl.attr]]
  sandy.textures <- grepl(sandy.texture.pattern, textures, ignore.case = TRUE)

  nhz <- horizons(non.argillic)

  # remove horizons that are sandy or have dark colors
  if (any(sandy.textures | dark.colors, na.rm = TRUE)) {
    nhz <- nhz[-which(sandy.textures | dark.colors),]
  }

  final <- data.frame(id = NA_character_, cambic_top = NA_real_, cambic_bottom = NA_real_)[0,]

  # iterate through combinations of horizons, check topology and thickness
  # finds multiple occurrences of cambic horizons, excluding argillics
  nhzs <- split(nhz, nhz[[idname(p)]])
  for (k in seq_along(nhzs)){
    for (j in 1:nrow(nhzs[[k]])) {
      for (i in j:nrow(nhzs[[k]])) {
  
        ftop <- nhzs[[k]][j:i, hzd[1]]
        fbot <- nhzs[[k]][j:i, hzd[2]]

        if (any(suppressWarnings(hzDepthTests(ftop, fbot)))) {
          i <- i - 1
          break;
        }
      }
  
      if (is.numeric(fbot) & is.numeric(ftop)) {
        pcamb.thickness <- fbot - ftop
        if (length(pcamb.thickness) > 0 & sum(pcamb.thickness, na.rm = TRUE) >= 15) {
    
          final <- rbind(final, data.frame(id = as.character(nhzs[[k]][[idname(p)]][1]),
                                           cambic_top = min(ftop, na.rm = TRUE),
                                           cambic_bottom = max(fbot, na.rm = TRUE)))
          if (i == nrow(nhzs[[k]])) {
              k = k + 1
              break
          }
        }
      }
      if (k > length(nhzs)) { 
        break
      }
    }
  }
  names(final)[1] <- idname(p)

  .N <- NULL 
  iddf <- data.table::data.table(id = as.character(final[[idname(p)]]))[, list(cambic_index = 1:.N), by = "id"]
  nadf <- data.frame(id = profile_id(p)[!profile_id(p) %in% final[[idname(p)]]])
  if (!is.null(nadf$id)){
    nadf[[idname(p)]] <- as.character(nadf$id)
    nadf$cambic_index <- rep(NA_real_, nrow(nadf))
    nadf$cambic_top <- rep(NA_real_, nrow(nadf))
    nadf$cambic_bottom <- rep(NA_real_, nrow(nadf))
  }
  iddf <- iddf[,-1]
  res <- cbind(final, iddf)
  res <- data.table::rbindlist(list(res, nadf), fill = TRUE)
  .as.data.frame.aqp(res, aqp_df_class(p))
}
