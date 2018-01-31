library(devtools)

# old version
load_all('../pkg/aqp/')

# new version
load_all('../sandbox/aqp2/')


# Munsell to RGB triplets: 
# function is vectorized as long as arguments are the same length
color <- munsell2rgb(the_hue=c('10YR', '2.5YR'), the_value=c(3, 5), the_chroma=c(5, 6), return_triplets=TRUE)

# RGB triplets to closest Munsell color (in RGB space)
# function is vectorized
rgb2munsell(color)


data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

plot(sp1)

# replace 0% frags with NA
sp1$prop[which(sp1$prop == 0)] <- NA

# plot in random order, note that annotations follow
par(mar=c(0, 0, 3, 0))
plot(sp1, color='prop', plot.order=sample(1:length(sp1)))
addVolumeFraction(sp1, 'prop', pch=1)

# re-sample each profile into 1 cm (thick) depth slices
# for the variables 'prop', 'name', 'soil_color'
# result is a SoilProfileCollection object
s <- slice(sp1, 0:25 ~ prop + name + soil_color)

# plot, note slices
plot(s)
