# miscellaneous soil color indices

#' @title Horizon Color Indices
#' @description Calculate basic horizon-level color indices for a SoilProfileCollection. Basic indices do not require aggregation over the whole profile or comparison to a "reference" (e.g. parent material) color. Includes Hurst (1977) Redness Index, Barron-Torrent Redness Index (1986) and Buntley-Westin Index (1965). 
#' @param p A SoilProfileCollection
#' @param hue Column name containing moist hue; default: "m_hue"
#' @param value Column name containing moist value; default: "m_value"
#' @param chroma Column name containing moist chroma; default: "m_chroma"
#' @return A data.frame containing unique pedon and horizon IDs and horizon-level color indices.
#' @author Andrew G. Brown.
#' @examples horizons(spc) <- horizonColorIndices(spc)
#' @rdname horizonColorIndices
#' @export horizonColorIndices
horizonColorIndices <- function(p, hue="m_hue", value="m_value", chroma="m_chroma") {
  hz <- horizons(p)
  
  hz$hurst_redness <- hurst.redness(hz[[hue]], hz[[value]], hz[[chroma]])
  
  hz$barron_torrent_redness <- barron.torrent.redness.LAB(hz[[hue]], hz[[value]], hz[[chroma]])
  
  hz$buntley_westin <- buntley.westin.index(hz[[hue]], hz[[chroma]])
  
  return(hz[,c(idname(p), hzidname(p), names(hz)[!names(hz) %in% names(horizons(p))])])
}

#' @title Hurst (1977) Redness Index
#' @description Calculate Redness Index after Hurst (1977) "Visual estimation of iron in saprolite" DOI: 10.1130/0016-7606(1977)88<174:VEOIIS>2.0.CO;2. Accepts vectorized inputs for hue, value and chroma, produces vector output.
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param value A numeric vector containing Munsell values
#' @param chroma A numeric vector containing Munsell chromas
#' @return A numeric vector of horizon redness index (lower values = redder).
#' @author Andrew G. Brown.
#' @rdname hurst.redness
#' @export hurst.redness
hurst.redness <-  function(hue, value, chroma) {
  # after Hurst (1977) "Visual estimation of iron in saprolite"
  # 10.1130/0016-7606(1977)88<174:VEOIIS>2.0.CO;2
  hue.lookup.table <- seq(5, 22.5, 2.5)
  names(hue.lookup.table) <- c('5R','7.5R','10R','2.5YR',
                               '5YR','7.5YR','10YR','2.5Y')
  hstar <- hue.lookup.table[hue]
  return(hstar * value / chroma)
}

#' @title Barron & Torrent (1986) Redness Index in LAB color space
#' @description Calculate Redness Index after Barron & Torrent (1986) "Use of the Kubelka—Munk Theory to Study the Influence of Iron Oxides on Soil Colour" using Munsell colors converted to LAB. DOI: 10.1111/j.1365-2389.1986.tb00382.x. Accepts vectorized inputs for hue, value and chroma, produces vector output.
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param value A numeric vector containing Munsell values
#' @param chroma A numeric vector containing Munsell chromas
#' @return A numeric vector of horizon redness index (higher values = redder).
#' @author Andrew G. Brown.
#' @rdname barron.torrent.redness.LAB
#' @export barron.torrent.redness.LAB
barron.torrent.redness.LAB <- function(hue, value, chroma) {
  # after Barron & Torrent (1986) "Use of the Kubelka—Munk Theory to Study the Influence of Iron Oxides on Soil Colour"
  # 10.1111/j.1365-2389.1986.tb00382.x
  lab <- munsell2rgb(hue, value, chroma, returnLAB = TRUE)
  return(lab$A * sqrt(lab$A^2 + lab$B^2) * 10 ^ 10 / (lab$B * (lab$L ^ 6)))
}

#' @title Harden (1982) Rubification
#' @description Calculate Rubification component of Profile Development Index after Harden (1982) "A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California". Accepts vectorized inputs for hue, value and chroma to produce vector output. In Harden (1982) rubification is calculated relative to a reference parent material. Several other non-color components are normalized relative to a maximum value and summed to obtain the overall Profile Development Index.
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param chroma A numeric vector containingMunsell chromas
#' @param hue_ref A character vector containing Munsell hue(s) (e.g. "10YR") for reference material
#' @param chroma_ref A numeric vector containing Munsell chroma(s) for reference material
#' @return A numeric vector reflecting horizon redness increase relative to a reference (e.g. parent) material.
#' @author Andrew G. Brown.
#' @rdname harden.rubification
#' @export harden.rubification
harden.rubification <- function(hue, chroma, hue_ref, chroma_ref) {
  # after Harden (1982)  "A quantitative index of soil development from field descriptions: 
  #                       Examples from a chronosequence in central California"
  hue.lookup.table <- 8:0
  names(hue.lookup.table) <- c('5R','7.5R','10R','2.5YR','5YR','7.5YR','10YR','2.5Y')
  dH <- as.numeric(hue.lookup.table[hue] - hue.lookup.table[hue_ref])
  dC <- chroma - chroma_ref
  return(10 * (dH + dC))
}

#' @title Harden (1982) Melanization
#' @description Calculate Melanization component of Profile Development Index after Harden (1982) "A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California". Accepts vectorized inputs for hue and chroma to produce vector output. In Harden (1982), melanization is calculated relative to a reference parent material for all horizons within 100cm of the soil surface. In addition, several other non-color components are normalized relative to a maximum value and summed to obtain the overall Profile Development Index.
#' @param value numeric vector containing Munsell values
#' @param value_ref A numeric vector containingMunsell value(s) for reference material
#' @return A numeric vector reflecting horizon darkening relative to a reference (e.g. parent) material.
#' @author Andrew G. Brown.
#' @rdname harden.melanization
#' @export harden.melanization
harden.melanization <- function(value, value_ref) {
  # for horizons within 100cm of soil surface
  # after Harden (1982)  "A quantitative index of soil development from field descriptions: 
  #                       Examples from a chronosequence in central California"
  # 10.1016/0016-7061(82)90037-4
  return(10 * (value_ref - value))
}

#' @title Buntley-Westin (1965) Index
#' @description Calculate "Color Development Equivalent" by the method of Buntley & Westin (1965) "A Comparative Study of Developmental Color in a Chestnut-Chernozem-Brunizem Soil Climosequence" DOI: 10.2136/sssaj1965.03615995002900050029x. Originally developed for Mollisols, the Buntley-Westin index has been used as a tool to separate soils based on depth to particular colors.
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param chroma A numeric vector containing Munsell chromas
#' @return A numeric vector reflecting horizon redness (higher values = redder).
#' @author Andrew G. Brown.
#' @rdname buntley.westin.index
#' @export buntley.westin.index
buntley.westin.index <- function(hue, chroma) {
  # after Buntley-Westin (1965) "A Comparative Study of Developmental Color in a Chestnut-Chernozem-Brunizem Soil Climosequence"
  # 10.2136/sssaj1965.03615995002900050029x
  hue.lookup.table <- 7:0
  #note: original publication included interpolation of 1.25Y hue @ 2.5
  names(hue.lookup.table) <- c('10R','2.5YR','5YR','7.5YR','10YR','2.5Y','5Y','N')
  return(hue.lookup.table[hue] * chroma)
}

#' @title Thompson-Bell (1996) Index
#' @description Calculate the "Profile Darkness Index" by the method of Thompson & Bell (1996) "Color index for identifying hydric conditions for seasonally saturated mollisols in Minnesota" DOI: 10.2136/sssaj1996.03615995006000060051x. The Thompson-Bell Index has been shown to reflect catenary relationships in some Mollisols of Minnesota (generally: wetter landscape positions = thicker, darker surfaces).
#' @param p A single-profile SoilProfileCollection (e.g. via profileApply())
#' @param name Column name containing horizon designations used to find A horizons (default: first column name containing 'name')
#' @param pattern Regular expression to match A horizons (default: "^A" which means horizon designation _starts with_ A)
#' @param value Column name containing horizon color values (default: "m_value")
#' @param chroma Column name containing horizon color chromas (default: "m_chroma")
#' @return A numeric vector reflecting horizon redness (higher values = redder).
#' @author Andrew G. Brown.
#' @rdname thompson.bell.darkness
#' @export thompson.bell.darkness
thompson.bell.darkness <- function(p, name = NULL, pattern="^A", value="m_value", chroma="m_chroma") {
  # after Thompson & Bell (1996) "Color index for identifying hydric conditions for seasonally saturated mollisols in Minnesota"
  # 10.2136/sssaj1996.03615995006000060051x
  hz <- horizons(p)
  depthz <- horizonDepths(p)
  nm <- names(hz)
  
  # TODO: this should be an internal function
  if (missing(name)) {
    possible.name <- nm[grep("name", nm, ignore.case = TRUE)]
    if (length(possible.name) > 0) {
      possible.name <- possible.name[1]
      name <- possible.name
      message(paste("guessing horizon designations are stored in `", 
                    name, "`", sep = ""))
    }
    else {
      message("unable to guess column containing horizon designations")
      name <- NA
    }
  }
  
  a.hz <- hz[grepl(hz[[name]], pattern = pattern),]
  a.hz$pdi_partial <- (a.hz[[depthz[2]]] - a.hz[[depthz[1]]]) / (a.hz[[value]] * a.hz[[chroma]] + 1)
  return(sum(a.hz$pdi_partial))
}

