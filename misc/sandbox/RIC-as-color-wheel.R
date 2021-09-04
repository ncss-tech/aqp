
## not as effective as it seemed at first

## use output from simulateColor() or aggregateColor() vs. manual tabulation



library(aqp)
library(soilDB)
library(ggplot2)
library(forcats)

x <- fetchKSSL('bearden', returnMorphologicData = TRUE, simplifyColors = TRUE)
s <- x$SPC

## only pedons with complete colors
s <- subset(s, ! is.na(moist_soil_color))

# s$genhz <- generalize.hz(s$hzn_desgn, c('Ap', 'ABk', 'Bk1', 'Bk2', 'C'), pat = c('Ap', 'ABk', 'Bk1', 'Bk2', 'C'), non.matching.code = NA)
# s$genhz <- factor(s$genhz, levels = c('Ap', 'ABk', 'Bk1', 'Bk2', 'C'))
# 
# table(s$hzn_desgn, useNA = 'always')
# table(s$genhz, s$hzn_desgn, useNA = 'always')

m <- paste0(s$m_hue, ' ', s$m_value, '/', s$m_chroma)
g <- 1

colorChart(m, g = g, chip.cex = 3)

colorChart(m, g = g, chip.cex = 2.5, size = FALSE)

colorChart(m, g = g, chip.cex = 2.5, size = FALSE, annotate = TRUE, annotate.type = 'percentage')

colorChart(m, chip.cex = 2.5, size = FALSE, annotate = TRUE, annotate.type = 'percentage')

d <- data.frame(m, g)
mm <- split(d, d$g)

tab <- lapply(mm, function(i) {
  tb <- as.data.frame(prop.table(table(m = i)))
  tb$col <- parseMunsell(tb$m)
  tb$g <- i$g[1]
  tb <- na.omit(tb)
  return(tb)
})


tab <- do.call('rbind', tab)

tab <- cbind(tab, parseMunsell(tab$m, convertColors = FALSE))

tab$hue <- factor(tab$hue, levels = huePosition(returnHues = TRUE, includeNeutral = TRUE), ordered = TRUE)

tab.unique <- unique(tab[, c('m', 'hue', 'value', 'chroma')])
tab.unique <- tab.unique[order(tab.unique$hue, tab.unique$value, tab.unique$chroma), ]


tab$m <- factor(tab$m, levels = tab.unique$m, ordered = TRUE)

# https://bjnnowak.netlify.app/2021/08/31/r-polar-barplots/

p1 <- ggplot() +
  geom_bar(
    data = tab,
    aes(x = m, y = sqrt(Freq)),
    stat = "identity", fill = tab$col) + 
  theme_minimal() 

p1 <- p1 +
  coord_polar(start = 0)

p1 <- p1 + ylab('') + xlab('Munsell Color')

## doesn't work all that well
# p1 + facet_wrap(vars(g))
 

p1


p1 + theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )



