#' Calculate the minimum thickness requirement for Mollic epipedon
#'
#' @description Utilize horizon depths, designations and textures in a profile to estimate the thickness requirement for the Mollic or Umbric epipedon, per criterion 6 in the U.S. Keys to Soil Taxonomy (12th Edition).
#'
#' @param p A single-profile SoilProfileCollection.
#' @param hzdesgn Column in horizon table containing designations. Default: `guessHzDesgnName(p)`
#' @param texcl.attr Column in horizon table containing texture classes. Default: \code{guessHzTexClName(p)}
#' @param clay.attr Column in horizon table containing clay contents. Default: \code{guessHzAttrName(p, 'clay', c('total','_r'))}
#' @param truncate Should sliding scale (Criterion 6C) results be truncated to 18 to 25cm interval? (Experimental; Default: TRUE)
#'
#' @return A unit length numeric vector containing Mollic or Umbric epipedon minimum thickness requirement.
#'
#' @author Andrew G. Brown
#' @export mollic.thickness.requirement
#'
#' @examples
#' # construct a fake profile
#' spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxeralfs",
#'                   hzname   = c("A","AB","Bt","BCt","R"),
#'                   hzdept   = c(0,  20, 32, 42,  49),
#'                   hzdepb   = c(20, 32, 42, 49, 200),
#'                   prop     = c(18, 22, 28, 24,  NA),
#'                   texcl    = c("l","l","cl", "l","br"),
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
#' data.frame(id = spc[[idname(spc)]],
#'            thickness_req = mollic.thickness.requirement(spc, clay.attr='prop'),
#'            thickness_req_nobound = mollic.thickness.requirement(spc,
#'                                         clay.attr='prop', truncate=FALSE))
#'
mollic.thickness.requirement <- function(p, 
                                         hzdesgn = guessHzDesgnName(p),
                                         texcl.attr = guessHzTexClName(p),
                                         clay.attr = guessHzAttrName(p, 'clay', c('total','_r')),
                                         truncate = TRUE) {

  hzd <- horizonDepths(p)
  buf <- rep(NA_real_, length(p))
  
  if (nrow(p) == 0) {
    return(numeric(0))
  }

  # determine boundaries
  # For purposes of identification of minimum thickness of mollic for field descriptions
  #   technically it is not applying the true taxonomic rules b/c it is based on hz desgn
  mss <- getMineralSoilSurfaceDepth(p, hzdesgn = hzdesgn, simplify = FALSE)

  soil_depth <- minDepthOf(p, pattern = "Cr|R|Cd|m",
                           hzdesgn = hzdesgn,
                           no.contact.depth = 200,
                           no.contact.assigned = NA,
                           simplify = FALSE)
  .LAST <- NULL
  idx1 <- which(soil_depth[[hzd[1]]] == 0)
  buf[idx1] <- 10

  #  if criteria aren't met in at least some part of upper 25 of mineral soil material, they won't be met deeper
  #  it is possible there are criteria greater than 25cm that affect total mollic thickness, but here we
  #  are only calculating the thickness _requirement_

  # get horizon data within mineral soil surface "critical zone"
  cztop <- mss[[hzd[2]]]
  czbot <- cztop + pmin(soil_depth[[hzd[1]]] - cztop, 25, na.rm = TRUE)
  deepest.bot.depth <- p[, , .LAST][[hzd[2]]]
  
  # soils with no mineral soil material have NA minimum thickness
  buf[which(cztop >= deepest.bot.depth)] <- NA
  
  epi <- trunc(p, cztop, czbot)

  # get horizon data from mineral soil surface to bedrock, physical root restriction,
  #  or pedogenic cementation >90% OR bottom of profile OR 250cm (~beyond SCS)
  soil.bottom <- pmin(soil_depth[[hzd[1]]], deepest.bot.depth, 250, na.rm = TRUE)
  sol <- trunc(p, mss[[hzd[2]]], soil.bottom)

  # 6C1 - TODO: create functions for testing basic field criteria for these diagnostics
  #       and lab data extension to support carbonates/calcic
  # cemented, carbonates or fragipan depth
  pan_depth <- minDepthOf(p,
                          pattern = "m|k|x",
                          hzdesgn = hzdesgn,
                          no.contact.assigned = NA,
                          simplify = FALSE)[[hzd[1]]]
  
  # calculate "shallowest of secondary carbonates/calcic, petrocalcic, duripan, fragipan"
  crit6c1 <- pan_depth 
  
  # 6C2 - identify argillic boundaries via aqp::getArgillicBounds
  # TODO: create functions for testing basic field criteria for these diagnostics (color structure)
  #       and lab data extension for spodic materials; can use logic from "sandy cambic" demo -- but more is needed
  argi_bounds <- getArgillicBounds(p, hzdesgn = hzdesgn, 
                                   clay.attr = clay.attr, 
                                   texcl.attr = texcl.attr, 
                                   simplify = FALSE)
  argillic_bottom <- argi_bounds$lbound

  # AGAIN, for purposes of identification of minimum thickness of mollic this is fast and probably fine
  #   but technically it is not applying the true taxonomic rules b/c it is based on hz desgn

  natric_oxic_spodic_cambic_bottom <- maxDepthOf(p,
                                                 pattern = "n|o|h|s[^s]",
                                                 top = FALSE,
                                                 no.contact.assigned = NA,
                                                 simplify = FALSE)
  
  #suppressWarnings(max(getCambicBounds(p, argi_bounds = argi_bounds)$cambic_bottom, na.rm=TRUE))
  
  .I <- NULL; cambic_bottom <- NULL
  
  .maxcmb <- function(x) {
    res <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(res) || length(x) == 0) return(NA_real_)
    res
  }
  cmb <- data.table::data.table(getCambicBounds(p, 
                                                hzdesgn = hzdesgn,
                                                clay.attr = clay.attr,
                                                texcl.attr = texcl.attr,
                                                argi_bounds = argi_bounds))[,
                                .maxcmb(cambic_bottom), by = c(idname(p))]$V1

  # calculate "deepest of lower boundary of argillic/natric, cambic, oxic, spodic"
  crit6c2 <- pmax(
    argillic_bottom,
    natric_oxic_spodic_cambic_bottom[[hzd[2]]],
    cmb,
    na.rm = TRUE
  )

  # SHORT CIRCUITS
  depi <- data.table::data.table(id = horizons(epi)[[idname(p)]], texcl = epi[[texcl.attr]])
  
  sandy.textures <- depi[,list(sandy.textures = all(grepl("S$", epi[[texcl.attr]], ignore.case = TRUE) &
                               !grepl("LVFS$|LFS$", epi[[texcl.attr]], ignore.case = TRUE))), by = "id"]$sandy.textures
  # 6A - assumes you must check for sandy textures throughout 0-25 to trigger minimum thickness of 25cm
  #      there is an implicit assumption that after mixing any non-sandy texture into sandy texture
  #      you would have non-sandy. this logic could be altered to be more "restrictive" by changing
  #      all() to any() -- forcing the 25cm requirement.
  #
  #      in that case, _any_ sandy texture would imply sandy textures, importantly: precluding
  #      the 10cm requirement.
  buf[sandy.textures] <- 25

  maxdepth <- sol[ , , .LAST][[hzd[2]]]    
  # 6B - if all horizons above a contact are non-sandy and meet all mollic characteristics then
  #      the minimum thickness could be only 10cm, we have filtered out sandy textures
  #
  #      technically, the 10cm requirement requires knowledge of whether other mollic requirements are met,
  #      so gives the epipedon a somewhat circular definition. This case applies in deep soils with mollic materials
  #      all the way to contact -- but only practically matters where a contact is less than 25cm depth,
  #      so that is when we return it.
  buf[maxdepth < 25] <- 10
  
  # no diagnostics present
  crit6c <- !is.na(crit6c1) & !is.na(crit6c2) 
  
  # TODO: "fluventic" soils with no other diagnostics have minimum thickness of 25cm  
  #     # for now, hope that the assigned "taxsubgrp" reflects floodplain condition where it matters
  #     ## for example:
  #     # tolerance <- 0.1 # set a threshold for "different" carbon contents of 0.1%
  #     #                  # carbon-depth "fluctuations" less or equal to tolerance will be ignored
  #     #                  # for different data sources this number may need to be higher
  # 
  #     # irregular.decrease <- diff(epi$estimated_organic_carbon) < 0
  #     # if(length(irregular.decrease[irregular.decrease >= tolerance]) {
  #     #  return(25)
  #     # }
  #     # TODO: most pedons don't have OC measured, develop color-carbon-depth-spatial surrogate model?
  buf[crit6c & grepl("fluv|cumulic", site(p)$taxsubgrp)] <- 25

  # calculate the most restrictive requirement from 6a, 6b, 6c
  #   which contain several restatements of the fundamental criteria for a sliding scale thickness
  sixcdepths <- pmax(crit6c1 / 3, crit6c2 / 3, na.rm = TRUE)

  # the thickness requirement is based on the most restrictive condition
  sixcdepths.t <- sixcdepths
  
  # sliding scale depths based on diagnostics are truncated to [18, 25]
  if (truncate) {
    sixcdepths.t[sixcdepths > 25] <- 25
    sixcdepths.t[sixcdepths < 18] <- 18
  }
  buf[is.na(buf)] <- sixcdepths.t[is.na(buf)]
  
  # this is criterion 6d (if "none of above" apply)
  buf[is.na(buf)] <- 18
  
  buf
}

#' Find horizons with colors darker than a Munsell hue, value, chroma threshold
#'
#' @description \code{hasDarkColors} returns a boolean value by horizon representing whether darkness thresholds are met. The code is fully vectorized and deals with missing data and optional thresholds.
#'
#' Default arguments are set up for "5-3-3 colors" -- the basic criteria for Mollic/Umbric epipedon/mineral soil darkness. Any of the thresholds or column names can be altered. Any thresholds that are set equal to \code{NA} will be ignored.
#'
#' @param p A SoilProfileCollection.
#' @param d_hue Optional: character vector of dry hues to match (default: NA)
#' @param m_hue Optional: character vector of moist hues to match (default: NA)
#' @param d_value Maximum value of dry value (default: 5)
#' @param d_chroma Optional: Maximum value of dry chroma (default: NA)
#' @param m_value Maximum value of moist value (default: 3)
#' @param m_chroma Maximum value of moist chroma (default: 3)
#' @param dhuenm Column name containing dry hue.
#' @param dvalnm Column name containing dry value.
#' @param dchrnm Column name containing dry chroma.
#' @param mhuenm Column name containing moist hue.
#' @param mvalnm Column name containing moist value.
#' @param mchrnm Column name containing moist chroma.
#'
#' @return Boolean value (for each horizon in \code{p}) reflecting whether "darkness" criteria are met.
#'
#' @author Andrew G. Brown
#'
#' @export hasDarkColors
#'
#' @examples
#' # construct a fake profile
#' spc <- data.frame(id=1, taxsubgrp = "Lithic Haploxeralfs",
#'                   hzdesgn  = c("A","AB","Bt","BCt","R"),
#'                   hzdept   = c(0, 20, 32, 42,  49),
#'                   hzdepb   = c(20, 32, 42, 49, 200),
#'                   d_value  = c(5,   5,  5,  6,  NA),
#'                   m_value  = c(2.5, 3,  3,  4,  NA),
#'                   m_chroma = c(2,   3,  4,  4,  NA))
#'
#' # promote to SoilProfileCollection
#' depths(spc) <- id ~ hzdept + hzdepb
#'
#' # print results in table
#' data.frame(id = spc[[idname(spc)]],
#'            hz_desgn = spc$hzdesgn,
#'            has_dark_colors = hasDarkColors(spc))
#'
hasDarkColors <-  function(p, d_hue=NA, m_hue=NA, d_value=5, d_chroma=NA, m_value=3, m_chroma=3,
                           dhuenm='d_hue', dvalnm = "d_value", dchrnm="d_chroma",
                           mhuenm='m_hue', mvalnm = "m_value", mchrnm = "m_chroma") {
  hz <- horizons(p)
  r <- matrix(nrow = nrow(hz), ncol = 6)

  r1 <- hz[[dhuenm]] %in% d_hue
  r2 <- hz[[mhuenm]] %in% m_hue
  r3 <- hz[[dvalnm]] <= d_value
  r4 <- hz[[dchrnm]] <= d_chroma
  r5 <- hz[[mvalnm]] <= m_value
  r6 <- hz[[mchrnm]] <= m_chroma

  if(length(r1) > 0) r[,1] <- r1
  if(length(r2) > 0) r[,2] <- r2
  if(length(r3) > 0) r[,3] <- r3
  if(length(r4) > 0) r[,4] <- r4
  if(length(r5) > 0) r[,5] <- r5
  if(length(r6) > 0) r[,6] <- r6

  required <- !is.na(c(d_hue, m_hue, d_value, d_chroma, m_value, m_chroma))
  risna <- apply(r, 2, function(x) all(is.na(x)))

  # if all data are missing, return NA for each horizon
  if(all(required == risna)) {
    return(rep(NA, nrow(p)))
  }

  # return only required results
  r <- r[,required]

  # result may contain NA for individual horizons without color data
  if(is.null(dim(r))) {
    # single horizon result
    return(all(r))
  } else {
    # multi horizon result
    return(apply(r, 1, all))
  }
}
