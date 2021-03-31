library(aqp)

data("rowley2019")

# not so interesting, these all share the same texture
rowley2019$texture <- ssc_to_texcl(sand = rowley2019$Sand, clay = rowley2019$Clay)

## TODO: condition the use of \n on horizon thickness:
##       thin horizons use namne : texture : pH

# composite horizon ID
rowley2019$hz.label <- sprintf(
  "%s\n%s: %s", 
  rowley2019$name, 
  rowley2019$texture, 
  round(rowley2019$pH, 1)
)


par(mar = c(0, 0, 0, 2))

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85
)

groupedProfilePlot(
  rowley2019, 
  groups = 'group', 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85
)


par(mar = c(0, 0, 3, 2))

plotSPC(
  rowley2019, 
  width = 0.25, 
  name.style = 'center-center', 
  name = 'hz.label', 
  cex.names = 0.85,
  color = 'Ca_exch', 
  col.label = 'Exchangeable Ca (cmol[+] / kg)'
)


