library(aqp)
library(maptools)
library(rgdal)
library(rgeos)


###############################################
## new version based on NSSL access database ##
###############################################

## warning... there are still some errors in these data:
# 07CA109004 [fixed]

x <- read.csv('MLRA_18_22-lab_data.csv', as.is=TRUE)

# how many hz per pedon?
n.hz <- ddply(x, .(pedon_id), nrow)
# remove samples with only a single horizon
x <- subset(x, subset=pedon_id %in% n.hz$pedon_id[n.hz$V1 > 1])

# there are still errors in these data!
depths(x) <- pedon_id ~ hztop + hzbot

# clean out hz-depth problems:
# hz with no top depth
# missing coordinates
x <- subset(x, subset=!is.na(hztop) & !is.na(lon) & !is.na(lat))

# replace missing bottom depths with top depth
x$hzbot[is.na(x$hzbot)] <- x$hztop[is.na(x$hzbot)]

# can now init SPC with horizons, site, coordinates
depths(x) <- pedon_id ~ hztop + hzbot
site(x) <- ~ site_id + observation_date + controlsec_top + controlsec_bottom + lon + lat + sampled_as + correlated_as + correlated_taxon_kind + correlated_class_name + mlra + ssa
coordinates(x) <- ~ lon + lat
proj4string(x) <- '+proj=lonlat +datum=WGS84'
# convert points to UTM
x@sp <- spTransform(x@sp, CRS('+proj=utm +zone=10 +datum=NAD83'))

## spatial subset with CA630 polygon
ca630_b <- readOGR(dsn='.', layer='ca630_b')

# graphical check: OK
plot(x@sp, cex=0.5)
plot(ca630_b, add=TRUE)

# projections are close enough (NAD83 vs. WGS84): perform intersection
proj4string(ca630_b) <- proj4string(x)
ca630 <- spatial_subset(x, ca630_b)

# looks good
points(ca630@sp, col='red', pch=3)

### re-make ca630.Rda

# test: works
a <- slab(ca630, fm= mlra ~ bs7, strict=TRUE)

# save new data
save(ca630, file='ca630-new.Rda')

# 
# 
# ############################################
# ## old version based on NSSL website data ##
# ############################################
# 
# ## NSSL
# # site data
# s <- read.csv('CA630-site.csv', as.is=TRUE)
# # lab data
# lab <- read.csv('CA630-CEC_and_Bases.csv', as.is=TRUE)
# # pedon data
# p <- read.csv('CA630-pedon.csv', as.is=TRUE)
# 
# ##############################################################################
# ############################## site data #####################################
# ##############################################################################
# 
# # identify missing coordinates
# s.vars <- c('longitude_degrees', 'longitude_minutes', 'longitude_seconds', 'latitude_degrees','latitude_minutes','latitude_seconds')
# s.no.missing.idx <- which(complete.cases(s[, s.vars]))
# 
# # add cols to original DF
# s$lon <- NA
# s$lat <- NA
# s$proj4 <- NA
# 
# # fix coordinates -- DMS to DD, then add to DF
# s$lon <- with(s, -(longitude_degrees + (longitude_minutes/60) + (longitude_seconds/60/60)))
# s$lat <- with(s, latitude_degrees + (latitude_minutes/60) + (latitude_seconds/60/60))
# 
# # convert to single datum
# s$datum <- with(s, ifelse(horizontal_datum_name == '', 'WGS84', horizontal_datum_name))
# 
# s$proj4[s.no.missing.idx] <- paste('+proj=longlat +datum=', s$datum[s.no.missing.idx], sep='')
# 
# # iterate over valid coordinates, and convert to WGS84
# s.final.coords <- ddply(s[s.no.missing.idx, ], .(user_site_id), 
# .progress='text', .fun=function(i) {
#   coordinates(i) <- ~ lon + lat
# 	proj4string(i) <- CRS(i$proj4)
# 	i.t <- spTransform(i, CRS('+proj=longlat +datum=WGS84'))
# 	as.matrix(coordinates(i.t))
# 	})
# 
# 
# # merge with original data
# s.final <- join(s, s.final.coords, by='user_site_id')
# 
# # keep only some columns: V1=fixed lon, V2= fixed lat
# s.final.vars <- c('user_site_id','mlra','county','ssa','V1','V2')
# # subset columns, and save to temp CSV for now
# ca630.site <- s.final[, s.final.vars]
# names(ca630.site) <- c('user_site_id', 'mlra', 'county', 'ssa', 'lon', 'lat')
# 
# ##############################################################################
# ############################## pedon data ####################################
# ##############################################################################
# 
# # keep only some columns
# # combine with site data, throw-out sites missing coordinates
# ca630.pedon <- p[, 1:9]
# ca630.site <- join(ca630.site[!is.na(ca630.site$lon), ], ca630.pedon, by='user_site_id')
# 
# # remove some junk
# ca630.site$sampled_class_name <- NULL
# ca630.site$correlated_taxon_name <- NULL
# ca630.site$correlated_taxon_name.1 <- NULL
# 
# ##############################################################################
# ############################## lab data #####################################
# ##############################################################################
# 
# # remove horizons that are missing top/bottom
# ca630.lab <- lab[!is.na(lab$hzn_top) | !is.na(lab$hzn_bot), ]
# 
# # remove samples with only a single horizon
# n.hz <- ddply(ca630.lab, .(pedon_key), nrow)
# ca630.lab <- subset(ca630.lab, subset=pedon_key %in% n.hz$pedon_key[n.hz$V1 > 1])
# 
# # id samples with bad horizons
# lab.test <- ddply(ca630.lab, .(pedon_key), test_hz_logic, topcol='hzn_top', bottomcol='hzn_bot')
# bad.ids <- as.character(lab.test$pedon_key[which(lab.test$V1 == FALSE)])
# 
# # keep the good ones
# ca630.lab <- subset(ca630.lab, pedon_key %in% lab.test$pedon_key[lab.test$V1 == TRUE])
# 
# 
# 
# ##############################################################################
# ############################## combine and save ##############################
# ##############################################################################
# ca630 <- list(site=ca630.site, lab=ca630.lab)
# save(ca630, file='ca630.rda')
# 
