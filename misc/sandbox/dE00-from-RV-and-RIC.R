





p <- list(
  list(m = '10YR 4/4', thresh = 10, hues = c('10YR', '7.5YR'))
)

s <- simulateColor(method = 'dE00', n = 500, parameters = p)

sort(table(s), decreasing = TRUE)

colorChart(s[[1]], chip.cex = 3, annotate = TRUE)

contrastChart('10YR 4/4', hues = c('10YR', '7.5YR'), thresh = 15)



# library(aqp)
library(soilDB)


x <- fetchKSSL(series='clarksville', returnMorphologicData = TRUE, simplifyColors = TRUE)

# extract pedons into SoilProfileCollection
s <- x$SPC

# genhz
s$genhz <- generalize.hz(s$hzn_desgn, c('A', 'E', 'Bt', '2Bt', '3Bt'), pat=c('A', 'E', '^Bt', '2B', '3B'), non.matching.code = NA)
s$genhz <- factor(s$genhz, levels = guessGenHzLevels(s, "genhz")$levels)

m <- paste0(s$m_hue, ' ', s$m_value, '/', s$m_chroma)
g <- s$genhz


colorChart(m, g = g, chip.cex = 3)

colorChart(m, g = g, chip.cex = 2.5, size = FALSE)

colorChart(m, g = g, chip.cex = 2.5, size = FALSE, annotate = TRUE)






# SIERRA: Bt1 horizon
rv <- '2.5YR 3/6'

ric <- expand.grid(
  hue = c('2.5YR', '5YR'),
  value = 3:5,
  chroma = 4:8
)

ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)


p <- list(
  list(m = rv, thresh = 15, hues = c('2.5YR', '5YR'))
)

s <- simulateColor(method = 'dE00', n = 100, parameters = p)

colorChart(c(rv, ric), chip.cex = 4, annotate = TRUE)
colorChart(c(rv, ric), chip.cex = 4, size = FALSE, annotate = TRUE)

colorChart(s[[1]], chip.cex = 4)



# DRUMMER: A horizon RV / RIC colors

rv <- '10YR 2/1'

ric <- expand.grid(
  hue = c('10YR', '2.5Y', '5Y'),
  value = 2:3,
  chroma = 1:2
)

ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)
colorChart(c(rv, ric), g = factor('A'), chip.cex = 4, annotate = TRUE)





## Is it possible to generate a reasonable dE00 threshold from soil color RV/RIC as typically specified in an OSD?

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


# DRUMMER: A horizon RV / RIC colors

rv <- '10YR 2/1'

ric <- expand.grid(
  hue = c('10YR', '2.5Y', '5Y'),
  value = c(2, 3),
  chroma = c(1, 2)
)

ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)



# SIERRA: Bt1 horizon
rv <- '2.5YR 3/6'

ric <- expand.grid(
  hue = c('2.5YR', '5YR'),
  value = c(3, 5),
  chroma = c(4, 8)
)

ric <- sprintf("%s %s/%s", ric$hue, ric$value, ric$chroma)



soilPalette(parseMunsell(ric), lab = ric)
previewColors(parseMunsell(ric), labels = ric)


(ric.dE00 <- groupContrast(rv, ric, method = 'wt.mean'))

p <- list(
  list(m = rv, thresh = ric.dE00, hues = c('2.5YR', '5YR'))
)

s <- simulateColor(method = 'dE00', n = 100, parameters = p)
sort(table(s), decreasing = TRUE)


contrastChart(m = rv, hues = c('2.5YR', '5YR'), thres = ric.dE00)


colorChart(s[[1]])

