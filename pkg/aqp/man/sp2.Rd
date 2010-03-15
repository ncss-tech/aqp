\name{sp2}
\alias{sp2}
\docType{data}
\title{Honcut Creek Soil Profile Data}
\description{A collection of 18 soil profiles, consisting of select soil morphologic attributes, associated with a stratigraphic study conducted near Honcut Creek, California.}
\usage{data(sp2)}
\format{
  A data frame with 154 observations on the following 21 variables.
  \describe{
    \item{\code{id}}{profile id}
    \item{\code{surface}}{dated surface}
    \item{\code{top}}{horizon top in cm}
    \item{\code{bottom}}{horizon bottom in cm}
    \item{\code{bound_distinct}}{horizon lower boundary distinctness class}
    \item{\code{bound_topography}}{horizon lower boundary topography class}
    \item{\code{name}}{horizon name}
    \item{\code{texture}}{USDA soil texture class}
    \item{\code{prop}}{field-estimated clay content}
    \item{\code{structure_grade}}{soil structure grade}
    \item{\code{structure_size}}{soil structure size}
    \item{\code{structure_type}}{soil structure type}
    \item{\code{stickiness}}{stickiness}
    \item{\code{plasticity}}{plasticity}
    \item{\code{field_ph}}{field-measured pH}
    \item{\code{hue}}{Munsell hue}
    \item{\code{value}}{Munsell value}
    \item{\code{chroma}}{Munsell chroma}
    \item{\code{r}}{RGB red component}
    \item{\code{g}}{RGB green component}
    \item{\code{b}}{RGB blue component}
    \item{\code{soil_color}}{R-friendly encoding of soil color}
  }
}
\details{}
\source{
Busacca, Alan J.; Singer, Michael J.; Verosub, Kenneth L. 1989. Late Cenozoic stratigraphy of the Feather and Yuba rivers area, California, with a section on soil development in mixed alluvium at Honcut Creek. USGS Bulletin 1590-G.
}
\references{}
\examples{
data(sp2)

# convert into SoilProfileList and plot
# plotting order, based on the dated surface each soil was described on
p.surface <- as.numeric(aggregate(sp2$surface, by=list(sp2$id), unique)$x)
p.order <- order(p.surface)

sp2.list <- initProfileList(sp2)
profile_plot(sp2.list, plot.order=p.order)

# truncate plot to 200 cm depth
profile_plot(sp2.list, plot.order=p.order, max.depth=200)


# look at numerical distances between profiles
d <- profile_compare(sp2, vars=c('prop','field_ph','hue','value'), max_d=100, k=0.01)

# better plotting with ape package:
require(ape)
require(cluster)
h <- diana(d)
p <- as.phylo(as.hclust(h))
plot(ladderize(p), cex=0.75, label.offset=1, no.margin=TRUE)

# add in the dated surface type via color
tiplabels(col=p.surface, pch=15)

# based on distance matrix values, YMMV
legend(x=-0.36, y=6.1, legend=levels(sp2$surface), col=1:6, pch=15, bty='n', bg='white')


}
\keyword{datasets}
