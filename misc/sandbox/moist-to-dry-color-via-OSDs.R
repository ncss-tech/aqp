library(aqp)
library(latticeExtra)
library(igraph)


x.10 <- read.csv(gzfile('E:/working_copies-SVN/soil-color-book/trunk/data/osd_colors-10cm.csv.gz'), stringsAsFactors = FALSE)
x.50 <- read.csv('E:/working_copies-SVN/soil-color-book/trunk/data/osd_colors-50cm.csv.gz', stringsAsFactors = FALSE)

x.10 <- na.omit(x.10)
x.50 <- na.omit(x.50)

x.10$d_color <- rgb(x.10$d_r, x.10$d_g, x.10$d_b, maxColorValue = 255)
x.50$d_color <- rgb(x.50$d_r, x.50$d_g, x.50$d_b, maxColorValue = 255)

x.10$m_color <- rgb(x.10$m_r, x.10$m_g, x.10$m_b, maxColorValue = 255)
x.50$m_color <- rgb(x.50$m_r, x.50$m_g, x.50$m_b, maxColorValue = 255)

hues <- c('2.5YR','5YR','7.5YR','10YR')

x.10$m_hue <- factor(x.10$m_hue, levels=hues)
x.50$m_hue <- factor(x.50$m_hue, levels=hues)

x.10 <- subset(x.10, subset=m_hue %in% hues & d_hue %in% hues)
x.50 <- subset(x.50, subset=m_hue %in% hues & d_hue %in% hues)


all.colors <- expand.grid(hue=hues, value=sort(unique(c(x.10$m_value, x.10$d_value))), chroma=1:8, stringsAsFactors=FALSE)
all.colors$hue <- factor(all.colors$hue, levels=hues)
all.colors$color <- with(all.colors, munsell2rgb(hue, value, chroma))

q25 <- function(i) {quantile(i, probs = 0.25, na.rm = TRUE)}
q75 <- function(i) {quantile(i, probs = 0.75, na.rm = TRUE)}

p.1 <- xyplot(value ~  chroma | hue,
       main="Common Soil Colors", scales=list(alternating=1, x=list(at=1:8), y=list(at=2:8)),
       xlim=0.5:8.5, ylim=c(1.5:8.5),
       strip=strip.custom(bg=grey(0.85)), layout=c(4,1),
       data=all.colors,
       as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
       panel=function(x, y, subscripts, ...)
       {
         d <- all.colors[subscripts, ]
         panel.xyplot(x, y, pch=15, cex=3, col=d$color)
       }
)


p.2 <- xyplot(m_value ~  m_chroma | m_hue,
       main="Common Soil Colors", scales=list(alternating=1, x=list(at=1:8), y=list(at=2:8)),
       xlim=0.5:8.5, ylim=c(1.5:8.5),
       strip=strip.custom(bg=grey(0.85)), layout=c(4,1),
       data=x.10, subset=m_hue %in% hues,
       as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
       panel=function(x, y, subscripts, ...)
       {
         d <- x.10[subscripts, ]
         panel.arrows(x0=d$m_chroma, x1=jitter(d$d_chroma), y0=d$m_value, y1=jitter(d$d_value), alpha=0.01, length=0.05, col='black')
         
         panel.arrows(x0=median(d$m_chroma), x1=median(jitter(d$d_chroma)), y0=median(d$m_value), y1=median(jitter(d$d_value)), lwd=3, length=0.1, col='green')
         
         panel.arrows(x0=median(d$m_chroma), x1=q25(jitter(d$d_chroma)), y0=median(d$m_value), y1=q25(jitter(d$d_value)), lwd=2, length=0.1, lty=2, col='green')
         
         panel.arrows(x0=median(d$m_chroma), x1=q75(jitter(d$d_chroma)), y0=median(d$m_value), y1=q75(jitter(d$d_value)), lwd=2, length=0.1, lty=2, col='green')
       }
)


p.3 <- xyplot(m_value ~  m_chroma | m_hue,
              main="Common Soil Colors", scales=list(alternating=1, x=list(at=1:8), y=list(at=2:8)),
              xlim=0.5:8.5, ylim=c(1.5:8.5),
              strip=strip.custom(bg=grey(0.85)), layout=c(4,1),
              data=x.50, subset=m_hue %in% hues,
              as.table=TRUE, subscripts=TRUE, xlab='Chroma', ylab='Value',
              panel=function(x, y, subscripts, ...)
              {
                d <- x.10[subscripts, ]
                panel.arrows(x0=d$m_chroma, x1=jitter(d$d_chroma), y0=d$m_value, y1=jitter(d$d_value), alpha=0.01, length=0.05, col='black')
                
                panel.arrows(x0=median(d$m_chroma), x1=median(jitter(d$d_chroma)), y0=median(d$m_value), y1=median(jitter(d$d_value)), lwd=3, length=0.1, col='green')
                
                panel.arrows(x0=median(d$m_chroma), x1=q25(jitter(d$d_chroma)), y0=median(d$m_value), y1=q25(jitter(d$d_value)), lwd=2, length=0.1, lty=2, col='green')
                
                panel.arrows(x0=median(d$m_chroma), x1=q75(jitter(d$d_chroma)), y0=median(d$m_value), y1=q75(jitter(d$d_value)), lwd=2, length=0.1, lty=2, col='green')
              }
)




p.4 <- p.1 + p.2
p.4 <- update(p.4, main='Moist vs Dry Soil Color (10cm, OSD records)')

png(file='moist-vs-dry-OSD-10cm.png', width=1200, height=600, type='cairo', antialias = 'subpixel', res = 110)
print(p.4)
dev.off()

p.5 <- p.1 + p.3
p.5 <- update(p.5, main='Moist vs Dry Soil Color (50cm, OSD records)')

png(file='moist-vs-dry-OSD-50cm.png', width=1200, height=600, type='cairo', antialias = 'subpixel', res = 110)
print(p.5)
dev.off()

# 
# by(x.10, x.10$m_hue, function(i) {with(i, quantile(i$d_value - i$m_value))})
# 
# by(x.50, x.50$m_hue, function(i) {with(i, quantile(i$d_value - i$m_value))})


## next time use MDS coordinates to show changes

## use RF model to show general trends, or just over-fit classifier






