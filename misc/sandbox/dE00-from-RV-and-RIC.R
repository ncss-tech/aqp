
## Is it possible to generate a reasonable dE00 threshold from soil color RV/RIC as typically specified in an OSD?

# DRUMMER: A horizon RV / RIC colors

rv <- '10YR 2/1'

ric <- expand.grid(
  hue = c('10YR', '2.5Y', '5Y'),
  value = c(2, 3),
  chroma = c(1, 2)
)

ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)

soilPalette(parseMunsell(ric), lab = ric)
previewColors(parseMunsell(ric), labels = ric)

groupContrast <- function(rv, ric, wt = rep(1, times = length(ric)), method = c('wt.mean', 'max')) {
  
  method <- match.arg(method)
  
  cc <- colorContrast(rep(rv, times = length(ric)), ric)
  
  res <- switch(method, 
         wt.mean = {
           # flag non-NA dE00
           idx <- which(!is.na(cc$dE00))
           
           # subset NA if present
           v <- cc$dE00[idx]
           w <- wt[idx]
           
           # weighted mean dE00
           sum(v * w) / sum(w)  
         },
         
         max = max(cc$dE00, na.rm = TRUE)
  )
  
  return(res)
  
}

groupContrast(rv, ric, method = 'wt.mean')

p <- list(
  list(m = rv, thresh = 8.8, hues = c('10YR', '7.5YR'))
)

s <- simulateColor(method = 'dE00', n = 100, parameters = p)
sort(table(s), decreasing = TRUE)


contrastChart(m = rv, hues = c('10YR', '2.5Y', '5Y'), thres = 8.8)
