\name{sp3}
\Rdversion{1.1}
\alias{sp3}
\docType{data}
\title{
Soil Profile Data Example 3
}
\description{
Soil samples from 10 soil profiles, taken from the Sierra Foothill Region of California.
}
\usage{data(sp3)}
\format{
  A data frame with 46 observations on the following 15 variables.
  \describe{
    \item{\code{id}}{soil id}
    \item{\code{top}}{horizon upper boundary (cm)} 
    \item{\code{bottom}}{horizon lower boundary (cm)}
    \item{\code{clay}}{clay content}
    \item{\code{cec}}{CEC by amonium acetate at pH 7}
    \item{\code{ph}}{pH in 1:1 water-soil mixture}
    \item{\code{tc}}{total carbon percent}
    \item{\code{hue}}{Munsell hue (dry)}
    \item{\code{value}}{Munsell value (dry)}
    \item{\code{chroma}}{Munsell chroma (dry)}
    \item{\code{mid}}{horizon midpoint (cm)}
    \item{\code{ln_tc}}{natural log of total carbon percent}
    \item{\code{L}}{color: l-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{A}}{color: a-coordinate, CIE-LAB colorspace (dry)}
    \item{\code{B}}{color: b-coordinate, CIE-LAB colorspace (dry)}
    \item{name}{horizon name}
    \item{soil_color}{horizon color}
  }
}
\details{
These data were collected to support research funded by the Kearney Foundation of Soil Science.
}

\references{http://casoilresource.lawr.ucdavis.edu/}


\examples{
## this example investigates the concept of a "mean profile"

# required packages
library(ape)
data(sp3)

# make a fake grouping variables
sp3$group <- factor('A')
a <- soil.slot.multiple(sp3, g='group', vars=c('clay','cec','ph'), seg_size=10)

# aggregation metric affects the result
a.wide <- cast(a, top + bottom ~ variable, value=c('p.mean'))

# add a new id for the "mean profile"
# and convert top/bottoms to integers
a.wide$id <- 'mp'
a.wide$top <- as.integer(a.wide$top)
a.wide$bottom <- as.integer(a.wide$bottom)

# combine original data with "mean profile"
sp3.grouped <- rbind(sp3[, c('top','bottom','id','clay','cec','ph')], a.wide[, c('top','bottom','id','clay','cec','ph')])

# perform comparison, and convert to phylo class object
d <- profile_compare(sp3.grouped, vars=c('clay','cec','ph'), max_d=100, k=0.01)
h <- agnes(d, method='ward')
p <- ladderize(as.phylo(as.hclust(h)))


# look at distance plot
plot_distance_graph(d, 11)

# similarity relative to mean profile
round(1 - (as.matrix(d)[11, ] / 23.69539), 2)


# convert soil data into ProfileList object for plotting
sp3.grouped$soil_color <- 'white'
sp3.grouped$name <- paste(round(sp3.grouped$clay), '/' , round(sp3.grouped$cec), '/', round(sp3.grouped$ph,1))
sp3.list <- initProfileList(sp3.grouped)


par(mar=c(1,1,1,1))
p.plot <- plot(p, cex=0.8, label.offset=3, direction='up', y.lim=c(100,-2), x.lim=c(1,sp3.list$num_profiles+1))

# get the last plot geometry
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

# the original labels, and new (indexed) order of pedons in dendrogram
d.labels <- attr(d, 'Labels')

new_order <- sapply(1:lastPP$Ntip,
function(i) which(as.integer(lastPP$xx[1:lastPP$Ntip]) == i))

# plot the profiles, in the ordering defined by the dendrogram
# with a couple fudge factors to make them fit
profile_plot(sp3.list, color="soil_color", plot.order=new_order,
scaling.factor=0.3, width=0.1, cex.names=0.65,
y.offset=max(lastPP$yy)+8, add=TRUE)
}

\keyword{datasets}
