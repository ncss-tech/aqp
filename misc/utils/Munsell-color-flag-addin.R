
# maybe useful when writing Rmd or Qmd documentation that references Munsell color

m2h <- function(m) {
  
  .col <- aqp::parseMunsell(m)
  .txtcol <- aqp::invertLabelColor(.col)
  
  sprintf(
    '<span style = "font-family: sans-serif; font-kerning: auto; font-size: 75%%; padding: 1pt 15pt 1pt 15pt; border: 1pt solid black; background-color: %s; color: %s">%s</span>', 
    .col, .txtcol, m
    )
}

cat(m2h('10GY 4/6'))

cat(m2h('7.5YR 3/4'))

