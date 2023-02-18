# new roxygen for old Rds

library(Rd2roxygen)

# may not be exhaustive
filez <- list.files("misc/man_todo","Rd", full.names = TRUE)

lapply(filez, function(x) {
  fn <- gsub("Rd", "R", basename(x))
  dp <- dirname(x)
  cat(create_roxygen(parse_file(x)), sep = "\n", file =  file.path(dp, fn), append = TRUE)
  return(file.exists(file.path(dp, fn)))
})

# manual processing...

# make data doc into one file; good enough for a one time thing -- add to data-documentation.R
a <- c("sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sierraTransect", "ca630",
       "rowley2019", "jacobs2000", "ROSETTA.centroids", "munsell", "soil_minerals")
datafilez <- file.path("R", paste0(a,".R"))
cat((paste0(do.call('c', lapply(datafilez, readLines)),'\n')), file = "misc/newdatadoc.R")
lapply(datafilez, file.remove)
