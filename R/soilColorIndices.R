# miscellaneous soil color indices

#' @title Horizon Color Indices
#' @description Calculate basic horizon-level color indices for a SoilProfileCollection. Basic indices do not require aggregation over the whole profile or comparison to a "reference" (e.g. parent material) color. Includes Hurst (1977) Redness Index, Barron-Torrent Redness Index (1986) and Buntley-Westin Index (1965). This is a wrapper method around several horizon-level indices. See the individual functions for more details.
#' @param p A SoilProfileCollection
#' @param hue Column name containing moist hue; default: "m_hue"
#' @param value Column name containing moist value; default: "m_value"
#' @param chroma Column name containing moist chroma; default: "m_chroma"
#' @return A data.frame containing unique pedon and horizon IDs and horizon-level color indices.
#' @author Andrew G. Brown
#' @seealso \code{\link{hurst.redness}} \code{\link{barron.torrent.redness.LAB}} \code{\link{buntley.westin.index}}
#' @examples
#' data(sp1)
#'
#' # promote sp1 data to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#'
#' # move site data
#' site(sp1) <- ~ group
#'
#' # compute indices
#' # merged into `sp1` with left-join on hzidname(sp1)
#' horizons(sp1) <- horizonColorIndices(sp1, hue="hue", value="value", chroma="chroma")
#'
#' # visualize
#' par(mar=c(0, 1, 3, 1))
#' plot(sp1, color='hurst_redness')
#' plot(sp1, color='barron_torrent_redness')
#' plot(sp1, color='buntley_westin')
#' @rdname horizonColorIndices
#' @export horizonColorIndices
horizonColorIndices <- function(p, hue="m_hue", value="m_value", chroma="m_chroma") {
  hz <- horizons(p)

  hz$hurst_redness <- hurst.redness(hz[[hue]], hz[[value]], hz[[chroma]])

  hz$barron_torrent_redness <- barron.torrent.redness.LAB(hz[[hue]], hz[[value]], hz[[chroma]])

  hz$buntley_westin <- buntley.westin.index(hz[[hue]], hz[[chroma]])

  return(.data.frame.j(hz, c(idname(p), hzidname(p), names(hz)[!names(hz) %in% names(horizons(p))]), aqp_df_class(p)))
}

#' @title Hurst (1977) Redness Index
#' @description Calculate Redness Index after Hurst (1977) "Visual estimation of iron in saprolite" DOI: 10.1130/0016-7606(1977)88<174:VEOIIS>2.0.CO;2. Accepts vectorized inputs for hue, value and chroma, produces vector output.
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param value A numeric vector containing Munsell values
#' @param chroma A numeric vector containing Munsell chromas
#' @return A numeric vector of horizon redness index (lower values = redder).
#' @author Andrew G. Brown
#' @references Hurst, V.J. (1977) Visual estimation of iron in saprolite. GSA Bulletin. 88(2): 174–176. doi: https://doi.org/10.1130/0016-7606(1977)88<174:VEOIIS>2.0.CO;2
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
#' @author Andrew G. Brown
#' @references Barron, V. and Torrent, J. (1986), Use of the Kubelka—Munk theory to study the influence of iron oxides on soil colour. Journal of Soil Science, 37: 499-510. doi:10.1111/j.1365-2389.1986.tb00382.x
#' @rdname barron.torrent.redness.LAB
#' @export barron.torrent.redness.LAB
barron.torrent.redness.LAB <- function(hue, value, chroma) {
  # after Barron & Torrent (1986) "Use of the Kubelka—Munk Theory to Study the Influence of Iron Oxides on Soil Colour"
  # 10.1111/j.1365-2389.1986.tb00382.x
  lab <- munsell2rgb(hue, value, chroma, returnLAB = TRUE)
  return(lab$A * sqrt(lab$A^2 + lab$B^2) * 10 ^ 10 / (lab$B * (lab$L ^ 6)))
}

#' @title Harden (1982) Rubification
#' @description Calculate "rubification" component of "Profile Development Index" after Harden (1982) "A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California". Accepts vectorized inputs for hue and chroma to produce vector output.
#'
#' In Harden (1982) "rubification" is calculated relative to a reference parent material. Several other non-color components are normalized relative to a maximum value and summed to obtain the overall Profile Development Index.
#'
#' @param hue A character vector containing Munsell hues (e.g. "7.5YR")
#' @param chroma A numeric vector containing Munsell chromas
#' @param hue_ref A character vector containing Munsell hue(s) (e.g. "10YR") for reference material
#' @param chroma_ref A numeric vector containing Munsell chroma(s) for reference material
#' @return A numeric vector reflecting horizon redness increase relative to a reference (e.g. parent) material.
#' @author Andrew G. Brown
#' @references Harden, J.W. (1982) A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California. Geoderma. 28(1) 1-28. doi:  10.1016/0016-7061(82)90037-4
#' @rdname harden.rubification
#' @export harden.rubification
#' @examples
#' library(aqp)
#' data("jacobs2000", package="aqp")
#'
#' # LEFT JOIN hue, value, chroma matrix color columns
#' horizons(jacobs2000) <- cbind(horizons(jacobs2000)[,c(idname(jacobs2000), hzidname(jacobs2000))],
#'                               parseMunsell(jacobs2000$matrix_color_munsell, convertColors = FALSE))
#'
#' #' # calculate a mixed 150-200cm color ~"parent material"
#' jacobs2000$c_horizon_color <- profileApply(jacobs2000, function(p) {
#'
#'   # and derive the parent material from the 150-200cm interval
#'   p150_200 <- glom(p, 150, 200, truncate = TRUE)
#'   p150_200$thickness <- p150_200$bottom - p150_200$top
#'   
#'   # subset colors and thickness
#'   clrs <- na.omit(horizons(p150_200)[,c('matrix_color_munsell','thickness')])
#'   
#'   # simulate a subtractive mixture using thickness as weight
#'   mixMunsell(
#'   clrs$matrix_color_munsell, 
#'   w = clrs$thickness, 
#'   mixingMethod = 'exact')$munsell
#' })
#'
#' # segment profile into 1cm slices (for proper depth weighting)
#' jacobs2000$rubif <- profileApply(jacobs2000, function(p) {
#'
#'   # sum the melanization index over the 0-100cm interval
#'   p0_100 <- segment(p, 0:100)
#'
#'   ccol <- parseMunsell(p$c_horizon_color, convertColors = FALSE)
#'
#'   sum(harden.rubification(
#'     hue = p0_100$hue,
#'     chroma = as.numeric(p0_100$chroma),
#'     hue_ref = ccol$hue,
#'     chroma_ref = as.numeric(ccol$chroma)
#'   ), na.rm = TRUE)
#'
#' })
#'
#' jacobs2000$rubiforder <- order(jacobs2000$rubif)
#'
#' # Plot in order of increasing Rubification index
#'
#' plotSPC(jacobs2000,
#' color = "matrix_color",
#' label = "rubif",
#' plot.order = jacobs2000$rubiforder,
#' max.depth = 250
#' )
#' 
#' segments(
#'   x0 = 0.5, 
#'   x1 = length(jacobs2000) + 0.5, 
#'   y0 = c(0,100,150,200), 
#'   y1 = c(0,100,150,200), 
#'   lty = 2
#' )
#' 
#' # Add [estimated] parent material color swatches
#' trash <- sapply(seq_along(jacobs2000$c_horizon_color), function(i) {
#'   rect(i - 0.15, 250, i + 0.15, 225,
#'        col = parseMunsell(jacobs2000$c_horizon_color[jacobs2000$rubiforder[i]]))
#' })
harden.rubification <- function(hue, chroma, hue_ref, chroma_ref) {
  # after Harden (1982)  "A quantitative index of soil development from field descriptions:
  #                       Examples from a chronosequence in central California"
  hue.lookup.table <- 0:8
  names(hue.lookup.table) <- c('5R','7.5R','10R','2.5YR','5YR','7.5YR','10YR','2.5Y')
  dH <- -as.numeric(hue.lookup.table[hue] - hue.lookup.table[hue_ref])
  dC <- chroma - chroma_ref
  return(10 * (dH + dC))
}

#' @title Harden (1982) Melanization
#'
#' @description Calculate "melanization" component of "Profile Development Index" after Harden (1982) "A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California". Accepts vectorized inputs for value and reference value to produce vector output. A convenient use case would be to apply this on a profile-specific basis, where the `value_ref` has a single value, and `value` is a vector of length equal to the number of horizons within the upper 100 cm.
#'
#' @details In Harden (1982), "melanization" is calculated relative to a reference parent material for all horizons within 100cm of the soil surface. In addition, several other non-color components are normalized relative to a maximum value and summed to obtain the overall Profile Development Index.
#'
#' @param value numeric vector containing Munsell values
#' @param value_ref A numeric vector containing Munsell value(s) for reference material
#' @return A numeric vector reflecting horizon darkening relative to a reference (e.g. parent) material.
#' @author Andrew G. Brown
#' @references Harden, J.W. (1982) A quantitative index of soil development from field descriptions: Examples from a chronosequence in central California. Geoderma. 28(1) 1-28. doi:  10.1016/0016-7061(82)90037-4
#' @rdname harden.melanization
#' @export harden.melanization
#' @examples
#' library(aqp)
#' data("jacobs2000", package="aqp")
#'
#' # LEFT JOIN hue, value, chroma matrix color columns
#' horizons(jacobs2000) <- cbind(horizons(jacobs2000)[,c(idname(jacobs2000), hzidname(jacobs2000))],
#'                               parseMunsell(jacobs2000$matrix_color_munsell, convertColors = FALSE))
#'
#' # calculate a mixed 150-200cm color ~"parent material"
#'
#' jacobs2000$c_horizon_color <- profileApply(jacobs2000, function(p) {
#'
#'   # and derive the parent material from the 150-200cm interval
#'   p150_200 <- glom(p, 150, 200, truncate = TRUE)
#'   p150_200$thickness <- p150_200$bottom - p150_200$top
#'
#'   # mix colors
#'   clrs <- na.omit(horizons(p150_200)[,c('matrix_color_munsell','thickness')])
#'   mixMunsell(clrs$matrix_color_munsell, w = clrs$thickness)$munsell
#'
#' })
#'
#' # segment profile into 1cm slices (for proper depth weighting)
#' jacobs2000$melan <- profileApply(jacobs2000, function(p) {
#'
#'   # sum the melanization index over the 0-100cm interval
#'   p0_100 <- segment(p, 0:100)
#'
#'   ccol <- parseMunsell(p$c_horizon_color, convertColors = FALSE)
#'
#'   sum(harden.melanization(
#'     value = as.numeric(p0_100$value),
#'     value_ref = as.numeric(ccol$value)), na.rm = TRUE)
#'
#' })
#'
#' jacobs2000$melanorder <- order(jacobs2000$melan)
#'
#' # Plot in order of increasing Melanization index
#'
#' plotSPC(jacobs2000, 
#'         color = "matrix_color",
#'         label = "melan",
#'         plot.order = jacobs2000$melanorder,
#'         max.depth = 250
#'         )
#'
#' segments(
#'   x0 = 0.5, 
#'   x1 = length(jacobs2000) + 0.5, 
#'   y0 = c(0,100,150,200), 
#'   y1 = c(0,100,150,200), 
#'   lty = 2
#' )
#'
#' # Add [estimated] parent material color swatches
#' lapply(seq_along(jacobs2000$c_horizon_color), function(i) {
#'   rect(i - 0.15, 250, i + 0.15, 225,
#'        col = parseMunsell(jacobs2000$c_horizon_color[jacobs2000$melanorder[i]]))
#' })
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
#' @return A numeric vector reflecting horizon color development.
#' @author Andrew G. Brown
#' @references Buntley, G.J. and Westin, F.C. (1965), A Comparative Study of Developmental Color in a Chestnut-Chernozem-Brunizem Soil Climosequence. Soil Science Society of America Journal, 29: 579-582. doi:10.2136/sssaj1965.03615995002900050029x
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
#' @return A numeric vector reflecting horizon darkness (lower values = darker).
#' @author Andrew G. Brown
#' @references Thompson, J.A. and Bell, J.C. (1996), Color Index for Identifying Hydric Conditions for Seasonally Saturated Mollisols in Minnesota. Soil Science Society of America Journal, 60: 1979-1988. doi:10.2136/sssaj1996.03615995006000060051x
#' @rdname thompson.bell.darkness
#' @export thompson.bell.darkness
thompson.bell.darkness <- function(p,
                                   name = guessHzDesgnName(p, required = TRUE),
                                   pattern = "^A",
                                   value = "m_value",
                                   chroma = "m_chroma") {
    
  # after Thompson & Bell (1996) "Color index for identifying hydric conditions for seasonally saturated mollisols in Minnesota"
  # 10.2136/sssaj1996.03615995006000060051x
  
  hz <- horizons(p)
  depthz <- horizonDepths(p)
  if (!all(name %in% horizonNames(p))) {
    name <- guessHzDesgnName(p, required = TRUE)
  }

  a.hz <- hz[grepl(hz[[name]], pattern = pattern),]
  a.hz$pdi_partial <- (a.hz[[depthz[2]]] - a.hz[[depthz[1]]]) / (a.hz[[value]] * a.hz[[chroma]] + 1)
  return(sum(a.hz$pdi_partial))
}

