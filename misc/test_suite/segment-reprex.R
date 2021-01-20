library(aqp, warn.conflicts = FALSE)

data(jacobs2000)

# use glom to create some fake missing data (REMOVE 50-100cm zone)
input.spc <- glom(jacobs2000[1,], 50, 100, 
                  truncate = TRUE, invert = TRUE)

# Error: the 50:100 zone is missing; and only one profile
output.spc <- segment(input.spc, 50:100)

# Error: even with data above and below 50/150 -- single profile
output.spc <- segment(input.spc, 25:150)

# Error: just 1 cm of missing data in single profile case breaks it
output.spc <- segment(input.spc, 0:51)

# Works
segment(input.spc, 0:50)

# Add another profile
input.spc <- combine(input.spc, jacobs2000[2,])

# Works
output.spc <- segment(input.spc, 50:100)

# Cannot combine because the transformed SPC has the same ID 
#  (this is a feature not a bug, resampled horizons != original data)
show.spc <- combine(input.spc, output.spc)

# segment should assign new profile IDs automatically -- like slice, permute_profile, etc
profile_id(output.spc) <- paste0(profile_id(output.spc), "segmented", 1:2)

par(mfrow=c(1,2),mar=c(0,0,2,1))
plot(input.spc, max.depth=200, color="matrix_color")
plot(output.spc, max.depth=200, color="matrix_color", divide.hz=FALSE, name=NA)

# combine to display results
show.spc <- combine(input.spc, output.spc)

# segmentation of zones with data is correct
plot(show.spc, color="matrix_color", divide.hz=FALSE)

