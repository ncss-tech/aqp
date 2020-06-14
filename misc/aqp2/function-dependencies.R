#devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)
library(aqp) 

deps <- funDependencies("package:aqp", "getArgillicBounds")
plot(deps)

edeps <- envirDependencies("package:aqp")
plot(edeps)
