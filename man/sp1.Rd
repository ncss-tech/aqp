\name{sp1}
\Rdversion{1.1}
\alias{sp1}
\docType{data}
\title{
Soil Profile Data Example 1
}
\description{
Soil profile data from Pinnacles National Monument, CA.
}
\usage{data(sp1)}
\format{
  A data frame with 60 observations on the following 21 variables.
  \describe{
    \item{\code{group}}{a numeric vector}
    \item{\code{id}}{a character vector}
    \item{\code{top}}{a numeric vector}
    \item{\code{bottom}}{a numeric vector}
    \item{\code{bound_distinct}}{a character vector}
    \item{\code{bound_topography}}{a character vector}
    \item{\code{name}}{a character vector}
    \item{\code{texture}}{a character vector}
    \item{\code{prop}}{a numeric vector}
    \item{\code{structure_grade}}{a character vector}
    \item{\code{structure_size}}{a character vector}
    \item{\code{structure_type}}{a character vector}
    \item{\code{stickiness}}{a character vector}
    \item{\code{plasticity}}{a character vector}
    \item{\code{field_ph}}{a numeric vector}
    \item{\code{hue}}{a character vector}
    \item{\code{value}}{a numeric vector}
    \item{\code{chroma}}{a numeric vector}
  }
}

\references{http://casoilresource.lawr.ucdavis.edu/}

\examples{
data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# re-sample each profile into 1 cm (thick) depth slices
# for the variables 'prop', 'name', 'soil_color'
# result is a SoilProfileCollection object
s <- slice(sp1, 0:25 ~ prop + name + soil_color)

# plot, note slices
plot(s)


# aggregate all profiles along 1 cm depth slices, 
# using data from column 'prop'
s1 <- slab(sp1, fm= ~ prop)

# plot mean +/- 1SD
library(lattice)
xyplot(top ~ p.mean + I(p.mean + p.sd) + I(p.mean - p.sd), 
data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2), 
panel=panel.superpose, ylim=c(110,-5))

# check median & IQR
xyplot(top ~ p.q50 + p.q25 + p.q75, 
data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2), 
panel=panel.superpose, ylim=c(110,-5))

# other segment sizes
# 5cm
s5 <- slab(sp1, fm= ~ prop, seg_size=5)

# 10cm segments:
s10 <- slab(sp1, fm= ~ prop, seg_size=10)

# 20cm
s20 <- slab(sp1, fm= ~ prop, seg_size=20)

# variation in segment-weighted mean property: very little
sapply(
list(s1,s5,s10,s20), 
function(i) {
	with(i, sum((bottom - top) * p.mean) / sum(bottom - top)) 
	}
)

# combined viz
g2 <- make.groups("1cm interval"=s1, "5cm interval"=s5, 
"10cm interval"=s10, "20cm interval"=s20)

# note special syntax for plotting step function
xyplot(cbind(top,bottom) ~ p.mean, groups=which, data=g2, id=g2$which,
panel=panel.depth_function, ylim=c(250,-10), 
scales=list(y=list(tick.number=10)), xlab='Property', 
ylab='Depth (cm)', main='Soil Profile Averaging by Slotting',
auto.key=list(columns=4, points=FALSE, lines=TRUE)
)


}
\keyword{datasets}
