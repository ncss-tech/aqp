# functions
aqp.functions <- getNamespaceExports("aqp")
aqp.functions <- aqp.functions[-grep("_[TC]_", aqp.functions)]
aqp.grp <- vector('list')

# plotting functions
plot.idx <- grep("[Pp]lot|add|find|fix|segments", aqp.functions)
aqp.grp[['plot']] <- sort(aqp.functions[plot.idx])
aqp.functions <- aqp.functions[-plot.idx]

# soil color
color.idx <- grep("[Cc]olor|red|dark|melan|rubif|[Mm]unsell|rgb|contrast|hue|buntley|Palette", aqp.functions)
aqp.grp[['color']] <- sort(aqp.functions[color.idx])
aqp.functions <- aqp.functions[-color.idx]

# soil texture
texture.idx <- grep("tex", aqp.functions)
aqp.grp[['texture']] <- sort(aqp.functions[texture.idx])
aqp.functions <- aqp.functions[-texture.idx]

# soilprofilecollection operations
spccalc.idx <- grep("sim|get|estim|clay|mollic", aqp.functions)
aqp.grp[['spccalc']] <- sort(aqp.functions[spccalc.idx])
aqp.functions <- aqp.functions[-spccalc.idx]

spc.idx <- grep("^l?un|split|SPC|spc|[Hh]orizon|[Ss]ite|subset|filter|mutate|[Pp]rofile|Apply|glom|<-|[Hh]z|id|length|nrow|min|max|[Dd]epthOf|coord|proj|denorm|meta|restrict|aqp|check|guess", aqp.functions)
aqp.grp[['spc']] <- sort(aqp.functions[spc.idx])
aqp.functions <- aqp.functions[-spc.idx]

# aggregate profiles
spc.agg.idx <- grep("aggregate|eval|slice|[Ss]lab|group|summarize|pc", aqp.functions)
aqp.grp[['spcagg']] <- sort(aqp.functions[spc.agg.idx])
aqp.functions <- aqp.functions[-spc.agg.idx]

# xrd
xrd.idx <- grep("noise|twotheta", aqp.functions)
aqp.grp[['xrd']] <- sort(aqp.functions[xrd.idx])
aqp.functions <- aqp.functions[-xrd.idx]

# classification
tau.idx <- grep("[Tt]au|confusion|shannon|brier", aqp.functions)
aqp.grp[['classification']] <- sort(aqp.functions[tau.idx])
aqp.functions <- aqp.functions[-tau.idx]


lapply(aqp.grp, length)

View(aqp.grp)

# devtools::install_github("datastorm-open/DependenciesGraphs")
#
# library(DependenciesGraphs)
# library(aqp)
# deps <- funDependencies("package:aqp", "getArgillicBounds")
# plot(deps)
# # 
# edeps <- envirDependencies("package:aqp")
# plot(edeps)

library(mvbutils)
library(aqp)
library(reshape)
ixWhere <- match(c("package:aqp","package:reshape"), search())
foodweb(where = ixWhere, prune = ls("package:reshape"), descendents = FALSE)
