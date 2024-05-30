library(aqp)

h <- read.csv('misc/example-data/bergstrom2019/horizon-data.csv')
s <- read.csv('misc/example-data/bergstrom2019/site-data.csv')

colnames(h)[1] <- "pedon"
h$hzID <- NULL
s$id <- NULL


depths(h) <- pedon ~ top + bottom
site(h) <- s
hzdesgnname(h) <- 'name'

h$group <- factor(substr(profile_id(h), 0, 2))

par(mar = c(0, 0, 3, 2))
plotSPC(h, color = 'pH', id.style = 'top', width = 0.33, name.style = 'center-center')

groupedProfilePlot(h, groups = 'group', color = 'pH', id.style = 'top', width = 0.33, name.style = 'center-center', group.name.offset = -15)

groupedProfilePlot(h, groups = 'group', color = 'tau_K', id.style = 'top', width = 0.33, name.style = 'center-center', group.name.offset = -15)

groupedProfilePlot(h, groups = 'group', color = 'Sr87_Sr86_ratio', id.style = 'top', width = 0.33, name.style = 'center-center', group.name.offset = -15)

