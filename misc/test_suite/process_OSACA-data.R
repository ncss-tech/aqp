library(aqp)

x <- read.csv('OSACA-example_soil_data.csv')

# assume horizons are measured from 0
x.new <- ddply(x, .(soil), .fun=function(i) {data.frame(i, top=c(0, i$depth[-length(i$depth)]), bottom=i$depth)})

# fix depths
x.new$top <- as.integer(x.new$top)
x.new$bottom <- as.integer(x.new$bottom)

# figure out column names / meanings\
# R,G,B appear to be color data
x.new$soil_color <- rgb(x.new$R25, g=x.new$G25, b=x.new$B25)
plot(depth ~ soil, data=x.new, col=x.new$soil_color, pch=15, ylim=c(400, 0))

# SPC
depths(x.new) <- soil ~ top + bottom

# give it a try: seems reasonable
d <- profile_compare(x.new, vars=c('sand','Mg','EC'), replace_na=TRUE, max_d=100, k=0, add_soil_flag=TRUE)

# save to .Rdata
sp5 <- x.new
sp5@horizons$depth <- NULL
sp5@horizons$hor <- NULL
sp5@horizons$soil <- as.character(sp5@horizons$soil)

save(sp5, file='sp5.rda')
