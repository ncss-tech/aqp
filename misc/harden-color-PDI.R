library(aqp)
data("jacobs2000", package="aqp")

# LEFT JOIN hue, value, chroma matrix color columns
horizons(jacobs2000) <- cbind(horizons(jacobs2000)[,c(idname(jacobs2000), hzidname(jacobs2000))],
                              parseMunsell(jacobs2000$matrix_color_munsell, convertColors = FALSE))

#' # calculate a mixed 150-200cm color ~"parent material"

jacobs2000$surface_color <- profileApply(jacobs2000, function(p) {

  print(profile_id(p))

  # and derive the parent material from the 150-200cm interval
  p2 <- glom(p, 0, 25, truncate = TRUE)
  p2$thickness <- p2$bottom - p2$top

  # mix colors
  clrs <- na.omit(horizons(p2)[,c('matrix_color_munsell','thickness')])
  mixMunsell(clrs$matrix_color_munsell, w = clrs$thickness)$munsell

})

jacobs2000$b_horizon_color <- profileApply(jacobs2000, function(p) {

  # and derive the parent material from the 150-200cm interval
  p2 <- glom(p, 25, 100, truncate = TRUE)
  p2$thickness <- p2$bottom - p2$top
  # mix colors
  clrs <- na.omit(horizons(p2)[,c('matrix_color_munsell','thickness')])
  mixMunsell(clrs$matrix_color_munsell, w = clrs$thickness)$munsell
})

jacobs2000$c_horizon_color <- profileApply(jacobs2000, function(p) {

  # and derive the parent material from the 150-200cm interval
  p2 <- glom(p, 150, 200, truncate = TRUE)
  p2$thickness <- p2$bottom - p2$top
  # mix colors
  clrs <- na.omit(horizons(p2)[,c('matrix_color_munsell','thickness')])
  mixMunsell(clrs$matrix_color_munsell, w = clrs$thickness)$munsell
})

# segment profile into 1cm slices (for proper depth weighting)
jacobs2000$rubif <- profileApply(jacobs2000, function(p) {

  # sum the melanization index over the 0-100cm interval
  p2 <- segment(p, 25:100)

  ccol <- parseMunsell(p$c_horizon_color, convertColors = FALSE)

  sum(harden.rubification(
    hue = p2$hue,
    chroma = as.numeric(p2$chroma),
    hue_ref = ccol$hue,
    chroma_ref = as.numeric(ccol$chroma)
  ), na.rm = TRUE)

})

jacobs2000$rubiforder <- order(jacobs2000$rubif)

# segment profile into 1cm slices (for proper depth weighting)
jacobs2000$melan <- profileApply(jacobs2000, function(p) {

  # sum the melanization index over the 0-100cm interval
  p2 <- segment(p, 0:25)

  ccol <- parseMunsell(p$c_horizon_color, convertColors = FALSE)

  sum(harden.melanization(
    value = as.numeric(p2$value),
    value_ref = as.numeric(ccol$value)), na.rm = TRUE)

})

jacobs2000$melanorder <- order(jacobs2000$melan)

# Plot in order of increasing Rubification index
par(mar=c(0,0,1,0))
plotSPC(jacobs2000, axis.line.offset = -1,
     color = "matrix_color",
     label = "rubif",
     plot.order = jacobs2000$rubiforder)
title("Modified Harden Rubification Index")
# Add [estimated] parent material color swatches
lapply(seq_along(jacobs2000$c_horizon_color), function(i) {
  rect(i - 0.15, 230, i + 0.15, 220,
       col = parseMunsell(jacobs2000$surface_color[jacobs2000$rubiforder[i]]))
  rect(i - 0.15, 245, i + 0.15, 235,
       col = parseMunsell(jacobs2000$b_horizon_color[jacobs2000$rubiforder[i]]))
  rect(i - 0.15, 260, i + 0.15, 250,
       col = parseMunsell(jacobs2000$c_horizon_color[jacobs2000$rubiforder[i]]))
})

# Plot in order of increasing Melanization index

par(mar=c(0,0,1,0))
plotSPC(jacobs2000, axis.line.offset = -1,
        color = "matrix_color",
        label = "melan",
        plot.order = jacobs2000$melanorder)
title("Modified Harden Melanization Index")

# Add [estimated] parent material color swatches
lapply(seq_along(jacobs2000$c_horizon_color), function(i) {
  rect(i - 0.15, 230, i + 0.15, 220,
       col = parseMunsell(jacobs2000$surface_color[jacobs2000$melanorder[i]]))
  rect(i - 0.15, 245, i + 0.15, 235,
       col = parseMunsell(jacobs2000$b_horizon_color[jacobs2000$melanorder[i]]))
  rect(i - 0.15, 260, i + 0.15, 250,
       col = parseMunsell(jacobs2000$c_horizon_color[jacobs2000$melanorder[i]]))
})

