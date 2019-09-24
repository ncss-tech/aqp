# https://dl.sciencesocieties.org/publications/sssaj/articles/66/1/315

options(stringsAsFactors = FALSE)

library(aqp)

h <- read.table('hz.txt', header = TRUE, sep='\t', quote = '')
str(h)

s <- read.table('site.txt', header = TRUE, sep='|', quote = '')
str(s)


# extract / split hz depths
hzd <- strsplit(h$hz_depths, '-', fixed = TRUE)

hzd <- lapply(hzd, function(i) {
  data.frame(top=as.numeric(i[1]), bottom=as.numeric(i[2]))
})

hzd <- do.call('rbind', hzd)

h <- data.frame(h, hzd)

# remove source composite
h$hz_depths <- NULL

# fill missing bottom depths with reasonable approx
idx <- which(is.na(h$bottom))
h$bottom[idx] <- h$top[idx] + 10

# copy / format matrix color
h$matrix_color_munsell <- h$matrix_color
h$matrix_color <- parseMunsell(h$matrix_color_munsell)
h$matrix_color_munsell[is.na(h$matrix_color)] <- NA

# extract / format RMF
# a real Munsell parser would be useful here
# see strcapture()
# splitting by whitespace will work for now

parseRMF <- function(txt) {
  
  z.split <- strsplit(txt, split = ' ', fixed = TRUE)
  
  # color is elements 2+3
  z <- lapply(z.split, function(i) {
    col <- paste0(i[2], ' ', i[3])
    return(col)
  })
  
  z.color <- do.call('c', z)
  
  # size/grade is element 1
  z.code <- sapply(z.split, function(i) i[1])
  
  # todo split further
  
  # composite
  d <- data.frame(code=z.code, munsell=z.color, color=parseMunsell(z.color))
  
  return(d)
}

dep <- parseRMF(h$depletion)
names(dep) <- c('depletion_code', 'depletion_munsell', 'depletion_color')

conc <- parseRMF(h$concentration)
names(conc) <- c('concentration_code', 'concentration_munsell', 'concentration_color')

h

# merge back into hz data
h <- data.frame(h, dep, conc)

h$concentration_munsell[is.na(h$concentration_color)] <- NA
h$depletion_munsell[is.na(h$depletion_color)] <- NA

# parse depletion/concentration codes
h$depletion_pct <- rep(0, times=nrow(h))
h$concentration_pct <- rep(0, times=nrow(h))

h$depletion_pct[grep('f', h$depletion_code)] <- 2
h$depletion_pct[grep('c', h$depletion_code)] <- 5
h$depletion_pct[grep('m', h$depletion_code)] <- 15

h$concentration_pct[grep('f', h$concentration_code)] <- 2
h$concentration_pct[grep('c', h$concentration_code)] <- 5
h$concentration_pct[grep('m', h$concentration_code)] <- 15


# remove source columns after parsing
h$depletion <- NULL
h$concentration <- NULL
h$hz_depths <- NULL



## init SPC
depths(h) <- id ~ top + bottom

par(mar=c(0,1,3,3))
plot(h, name='name', color='matrix_color', width=0.3)
addVolumeFraction(h, 'concentration_pct', col = h$concentration_color, pch = 16, cex.max = 0.5)

plot(h, name='name', color='matrix_color', width=0.3)
addVolumeFraction(h, 'depletion_pct', col = h$depletion_color, pch = 16, cex.max = 0.5)

plotSPC(h, color='time_saturated', cex.names=0.8, col.label = 'Time Saturated')


cc <- colorContrast(h$matrix_color_munsell, h$concentration_munsell)
cc <- na.omit(cc)

cc <- cc[order(cc$dE00), ]
cc <- unique(cc)

par(bg='black', fg='white')
colorContrastPlot(cc$m1[1:10], cc$m2[1:10], labels = c('matrix', 'concentration'))
colorContrastPlot(cc$m1[11:21], cc$m2[11:21], labels = c('matrix', 'concentration'))




cc <- colorContrast(h$depletion_munsell, h$concentration_munsell)
cc <- na.omit(cc)

cc <- cc[order(cc$dE00), ]
cc <- unique(cc)

par(bg='black', fg='white')
colorContrastPlot(cc$m1, cc$m2, labels = c('depletion', 'concentration'))





