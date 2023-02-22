##
## document data here
## don't forget: @usage data(XXX)


#' @title Example Output from soilDB::fetchOSD()
#' @description An example `SoilProfileCollection` object created by `soilDB::fetchOSD()`, derived from the Cecil, Appling, and Bonneau Official Series Descriptions.
#' @keywords datasets
#' @usage data(osd)
#' @format A `SoilProfileCollection`
"osd"



#' @title Example Data from Wilson et al. 2022
#' @description An example `SoilProfileCollection`, derived from Wilson et al., 2022. Select data extracted from Appendix tables.
#' @keywords datasets
#' @usage data(wilson2022)
#' @format A `SoilProfileCollection` with the following elements. Total elemental analysis by lithium borate fusion.
#' 
#' Horizon level attributes:
#'   * name: horizon designation
#'   * Al2O3: total Al (wt %)
#'   * Fe2O3: total Fe (wt %)
#'   * K2O: total K (wt %)
#'   * MgO: total Mg (wt %)
#'   * Na2O: total Na (wt %)
#'   * P2O5: total P (wt %)
#'   * SiO2: total Si (wt %)
#'   * CaO: total Ca(wt %)
#'   * Alo: Oxalate Extractable Al (g/kg)
#'   * Feo: Oxalate Extractable Fe (g/kg)
#'   * Fed: Dithionite extractable Fe (g/kg)
#'   * Fed_minus_Feo: Crystalline Fe (hydr)oxides (g/kg)
#'   * CIA: Chemical Index of Alteration, see original paper (ratio, unitless)
#'   * Fed_div_Fet: (ratio, unitless)
#'   * Fet: Total Fe from lithium borate fusion (g/kg)
#'   * resin_Pi: Hedley phosphorus fractions (mg/kg)
#'   * NaHCO3_Pi: Hedley phosphorus fractions (mg/kg) 
#'   * labile_Pi: Sum of resin Pi and NaHCO3 Pi (mg/kg)
#'   * NaCO3_Po: Hedley phosphorus fractions (mg/kg)
#'   * NaOH_Pi: Hedley phosphorus fractions (mg/kg)
#'   * NaOH_Po: Hedley phosphorus fractions (mg/kg)
#'   * Ca_Pi: Hedley phosphorus fractions (mg/kg)
#'   * organic_P: Sum of NaHCO3 and NaOH Po fractions (mg/kg)
#'   * total_P: Total P from lithium borate fusion (mg/kg)
#'   * occluded_P: Difference between total P and sum of Hedley P fractions (mg/kg)
#'   * top: horizon top depth (cm)
#'   * bottom: horizon bottom depth (cm)
#'   * pedonID: pedon ID (serial number)
#'   
#' Site level attributes:
#'   * pm: parent material group
#'   * biome: biome
#'
#' @references 
#' Stewart G. Wilson, Randy A. Dahlgren, Andrew J. Margenot, Craig Rasmussen, Anthony T. O'Geen. 2022. Expanding the Paradigm: The influence of climate and lithology on soil phosphorus, Geoderma: 421. \doi{doi.org/10.1016/j.geoderma.2022.115809}
#' 
#' @examples 
#' 
#' data(wilson2022)
#' 
#' groupedProfilePlot(wilson2022, groups = 'pm', 
#' group.name.offset = -15, label = 'biome', 
#' name.style = 'center-center', color = 'CIA', 
#' cex.names = 0.66, cex.id = 0.66, width = 0.3, 
#' plot.depth.axis = FALSE, hz.depths = TRUE)
#' 
#' groupedProfilePlot(wilson2022, groups = 'biome', 
#' group.name.offset = -15, label = 'pm', 
#' name.style = 'center-center', color = 'Fet', 
#' cex.names = 0.66, cex.id = 0.66, width = 0.3, 
#' plot.depth.axis = FALSE, hz.depths = TRUE)
#' 
"wilson2022"






#' Soil Profile Data Example 1
#'
#' Soil profile data from Pinnacles National Monument, CA.
#'
#'
#' @name sp1
#' @docType data
#' @format A data frame with 60 observations on the following 21 variables.
#' \describe{ \item{group}{a numeric vector} \item{id}{a
#' character vector} \item{top}{a numeric vector}
#' \item{bottom}{a numeric vector} \item{bound_distinct}{a
#' character vector} \item{bound_topography}{a character vector}
#' \item{name}{a character vector} \item{texture}{a character
#' vector} \item{prop}{a numeric vector}
#' \item{structure_grade}{a character vector}
#' \item{structure_size}{a character vector}
#' \item{structure_type}{a character vector}
#' \item{stickiness}{a character vector} \item{plasticity}{a
#' character vector} \item{field_ph}{a numeric vector}
#' \item{hue}{a character vector} \item{value}{a numeric
#' vector} \item{chroma}{a numeric vector} }
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @keywords datasets
#' @examples
#'
#' data(sp1)
#' # convert colors from Munsell to hex-encoded RGB
#' sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))
#'
#' # promote to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' # re-sample each profile into 1 cm (thick) depth slices
#' # for the variables 'prop', 'name', 'soil_color'
#' # result is a SoilProfileCollection object
#' s <- dice(sp1, 0:25 ~ prop + name + soil_color)
#'
#' # plot, note slices
#' plot(s)
#'
#'
#' # aggregate all profiles along 1 cm depth slices,
#' # using data from column 'prop'
#' s1 <- slab(sp1, fm= ~ prop)
#'
#' # check median & IQR
#' library(lattice)
#' xyplot(top ~ p.q50 + p.q25 + p.q75,
#' data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2),
#' panel=panel.superpose, ylim=c(110,-5), asp=2)
#'
#'
NULL

#' Honcut Creek Soil Profile Data
#'
#' A collection of 18 soil profiles, consisting of select soil morphologic
#' attributes, associated with a stratigraphic study conducted near Honcut
#' Creek, California.
#'
#'
#' @name sp2
#' @docType data
#' @format A data frame with 154 observations on the following 21 variables.
#' \describe{ \item{id}{profile id} \item{surface}{dated
#' surface} \item{top}{horizon top in cm} \item{bottom}{horizon
#' bottom in cm} \item{bound_distinct}{horizon lower boundary
#' distinctness class} \item{bound_topography}{horizon lower boundary
#' topography class} \item{name}{horizon name}
#' \item{texture}{USDA soil texture class}
#' \item{prop}{field-estimated clay content}
#' \item{structure_grade}{soil structure grade}
#' \item{structure_size}{soil structure size}
#' \item{structure_type}{soil structure type}
#' \item{stickiness}{stickiness} \item{plasticity}{plasticity}
#' \item{field_ph}{field-measured pH} \item{hue}{Munsell hue}
#' \item{value}{Munsell value} \item{chroma}{Munsell chroma}
#' \item{r}{RGB red component} \item{g}{RGB green component}
#' \item{b}{RGB blue component} \item{soil_color}{R-friendly
#' encoding of soil color} }
#' @author Dylan E. Beaudette
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @source Busacca, Alan J.; Singer, Michael J.; Verosub, Kenneth L. 1989. Late
#' Cenozoic stratigraphy of the Feather and Yuba rivers area, California, with
#' a section on soil development in mixed alluvium at Honcut Creek. USGS
#' Bulletin 1590-G.
#' @keywords datasets
#' @examples
#'
#' data(sp2)
#'
#' # convert into SoilProfileCollection object
#' depths(sp2) <- id ~ top + bottom
#'
#' # transfer site-level data
#' site(sp2) <- ~ surface
#'
#' # generate a new plotting order, based on the dated surface each soil was described on
#' p.order <- order(sp2$surface)
#'
#' # plot
#' par(mar=c(1,0,3,0))
#' plot(sp2, plot.order=p.order)
#'
#' # setup multi-figure output
#' par(mfrow=c(2,1), mar=c(0,0,1,0))
#'
#' # truncate plot to 200 cm depth
#' plot(sp2, plot.order=p.order, max.depth=200)
#' abline(h=200, lty=2, lwd=2)
#'
#' # compute numerical distances between profiles
#' # based on select horizon-level properties, to a depth of 200 cm
#' d <- NCSP(sp2, vars=c('prop','field_ph','hue'), maxDepth = 100, k = 0)
#'
#' # plot dendrogram with ape package:
#' if(require(ape) & require(cluster)) {
#' h <- diana(d)
#' p <- as.phylo(as.hclust(h))
#' plot(p, cex=0.75, label.offset=0.01, font=1, direct='down', srt=90, adj=0.5, y.lim=c(-0.125, 0.5))
#'
#' # add in the dated surface type via color
#' tiplabels(col=as.numeric(sp2$surface), pch=15)
#'
#' # based on distance matrix values, YMMV
#' legend('topleft', legend=levels(sp2$surface), col=1:6, pch=15, bty='n', bg='white', cex=0.75)
#' }
#'
#'
NULL
#' Soil Profile Data Example 3
#'
#' Soil samples from 10 soil profiles, taken from the Sierra Foothill Region of
#' California.
#'
#' These data were collected to support research funded by the Kearney
#' Foundation of Soil Science.
#'
#' @name sp3
#' @docType data
#' @format A data frame with 46 observations on the following 15 variables.
#' \describe{ \item{id}{soil id} \item{top}{horizon upper
#' boundary (cm)} \item{bottom}{horizon lower boundary (cm)}
#' \item{clay}{clay content} \item{cec}{CEC by amonium acetate
#' at pH 7} \item{ph}{pH in 1:1 water-soil mixture}
#' \item{tc}{total carbon percent} \item{hue}{Munsell hue
#' (dry)} \item{value}{Munsell value (dry)}
#' \item{chroma}{Munsell chroma (dry)} \item{mid}{horizon
#' midpoint (cm)} \item{ln_tc}{natural log of total carbon percent}
#' \item{L}{color: l-coordinate, CIE-LAB colorspace (dry)}
#' \item{A}{color: a-coordinate, CIE-LAB colorspace (dry)}
#' \item{B}{color: b-coordinate, CIE-LAB colorspace (dry)}
#' \item{name}{horizon name} \item{soil_color}{horizon color} }
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @keywords datasets
#' @examples
#'
#' ## this example investigates the concept of a "median profile"
#'
#' # required packages
#' if (require(ape) & require(cluster)) {
#'   data(sp3)
#'
#'   # generate a RGB version of soil colors
#'   # and convert to HSV for aggregation
#'   sp3$h <- NA
#'   sp3$s <- NA
#'   sp3$v <- NA
#'   sp3.rgb <- with(sp3, munsell2rgb(hue, value, chroma, return_triplets = TRUE))
#'
#'   sp3[, c('h', 's', 'v')] <- t(with(sp3.rgb, rgb2hsv(r, g, b, maxColorValue = 1)))
#'
#'   # promote to SoilProfileCollection
#'   depths(sp3) <- id ~ top + bottom
#'
#'   # aggregate across entire collection
#'   a <- slab(sp3, fm = ~ clay + cec + ph + h + s + v, slab.structure = 10)
#'
#'   # check
#'   str(a)
#'
#'   # convert back to wide format
#'   library(data.table)
#'
#'   a.wide.q25 <- dcast(a, top + bottom ~ variable, value.var = c('p.q25'))
#'   a.wide.q50 <- dcast(a, top + bottom ~ variable, value.var = c('p.q50'))
#'   a.wide.q75 <- dcast(a, top + bottom ~ variable, value.var = c('p.q75'))
#'
#'   # add a new id for the 25th, 50th, and 75th percentile pedons
#'   a.wide.q25$id <- 'Q25'
#'   a.wide.q50$id <- 'Q50'
#'   a.wide.q75$id <- 'Q75'
#'
#'   # combine original data with "mean profile"
#'   vars <- c('top', 'bottom', 'id', 'clay', 'cec', 'ph', 'h', 's', 'v')
#'   # make data.frame version of sp3
#'   sp3.df <- as(sp3, 'data.frame')
#'   sp3.grouped <- rbind(sp3.df[, vars], a.wide.q25[, vars], a.wide.q50[, vars], a.wide.q75[, vars])
#'
#'   # re-constitute the soil color from HSV triplets
#'   # convert HSV back to standard R colors
#'   sp3.grouped$soil_color <- with(sp3.grouped, hsv(h, s, v))
#'
#'   # give each horizon a name
#'   sp3.grouped$name <- paste(
#'     round(sp3.grouped$clay),
#'     '/' ,
#'     round(sp3.grouped$cec),
#'     '/',
#'     round(sp3.grouped$ph, 1)
#'   )
#'
#' 
#' # first promote to SoilProfileCollection
#' depths(sp3.grouped) <- id ~ top + bottom
#' 
#' plot(sp3.grouped)
#' 
#' ## perform comparison, and convert to phylo class object
#' ## D is rescaled to [0,]
#' d <- NCSP(
#'   sp3.grouped,
#'   vars = c('clay', 'cec', 'ph'),
#'   maxDepth = 100,
#'   k = 0.01
#' )
#' 
#' h <- agnes(d, method = 'ward')
#' p <- ladderize(as.phylo(as.hclust(h)))
#' 
#' # look at distance plot-- just the median profile
#' plot_distance_graph(d, 12)
#' 
#' # similarity relative to median profile (profile #12)
#' round(1 - (as.matrix(d)[12, ] / max(as.matrix(d)[12, ])), 2)
#' 
#' ## make dendrogram + soil profiles
#' 
#' # setup plot: note that D has a scale of [0,1]
#' par(mar = c(1, 1, 1, 1))
#' p.plot <- plot(p,
#'                cex = 0.8,
#'                label.offset = 3,
#'                direction = 'up',
#'                y.lim = c(200, 0),
#'                x.lim = c(1.25, length(sp3.grouped) + 1),
#'                show.tip.label = FALSE)
#' 
#' # get the last plot geometry
#' lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
#' 
#' # the original labels, and new (indexed) order of pedons in dendrogram
#' d.labels <- attr(d, 'Labels')
#' 
#' new_order <- sapply(1:lastPP$Ntip,
#'                     function(i)
#'                       which(as.integer(lastPP$xx[1:lastPP$Ntip]) == i))
#' 
#' # plot the profiles, in the ordering defined by the dendrogram
#' # with a couple fudge factors to make them fit
#' plotSPC(
#'   sp3.grouped,
#'   color = "soil_color",
#'   plot.order = new_order,
#'   y.offset = max(lastPP$yy) + 10,
#'   width = 0.1,
#'   cex.names = 0.5,
#'   add = TRUE
#' )
#' }
NULL

#' Soil Chemical Data from Serpentinitic Soils of California
#'
#' Soil Chemical Data from Serpentinitic Soils of California
#'
#' Selected soil physical and chemical data from (McGahan et al., 2009).
#'
#' @name sp4
#' @docType data
#' @format A data frame with 30 observations on the following 13 variables.
#' \describe{ \item{id}{site name} \item{name}{horizon
#' designation} \item{top}{horizon top boundary in cm}
#' \item{bottom}{horizon bottom boundary in cm}
#' \item{K}{exchangeable K in c mol/kg} \item{Mg}{exchangeable
#' Mg in cmol/kg} \item{Ca}{exchangeable Ca in cmol/kg}
#' \item{CEC_7}{cation exchange capacity (NH4OAc at pH 7)}
#' \item{ex_Ca_to_Mg}{extractable Ca:Mg ratio} \item{sand}{sand
#' content by weight percentage} \item{silt}{silt content by weight
#' percentage} \item{clay}{clay content by weight percentage}
#' \item{CF}{>2mm fraction by volume percentage} }
#' @references McGahan, D.G., Southard, R.J, Claassen, V.P. 2009.
#' Plant-Available Calcium Varies Widely in Soils on Serpentinite Landscapes.
#' Soil Sci. Soc. Am. J. 73: 2087-2095.
#' @source https://www.soils.org/publications/sssaj/articles/73/6/2087
#' @keywords datasets
#' @examples
#'
#' # load sample data set, a simple data.frame object with horizon-level data from 10 profiles
#' library(aqp)
#' data(sp4)
#' str(sp4)
#' sp4$idbak <- sp4$id
#' #sp4 <- sp4[order(match(sp4$id, aqp:::.coalesce.idx(sort(sp4$id))), sp4$top),]
#'
#'
#' # upgrade to SoilProfileCollection
#' # 'id' is the name of the column containing the profile ID
#' # 'top' is the name of the column containing horizon upper boundaries
#' # 'bottom' is the name of the column containing horizon lower boundaries
#' depths(sp4) <- id ~ top + bottom
#'
#' # check it out
#' class(sp4) # class name
#' str(sp4) # internal structure
#'
#' # check integrity of site:horizon linkage
#' spc_in_sync(sp4)
#'
#' # check horizon depth logic
#' checkHzDepthLogic(sp4)
#'
#' # inspect object properties
#' idname(sp4) # self-explanitory
#' horizonDepths(sp4) # self-explanitory
#'
#' # you can change these:
#' depth_units(sp4) # defaults to 'cm'
#' metadata(sp4) # not much to start with
#'
#' # alter the depth unit metadata
#' depth_units(sp4) <- 'inches' # units are really 'cm'
#'
#' # more generic interface for adjusting metadata
#'
#' # add attributes to metadata list
#' metadata(sp4)$describer <- 'DGM'
#' metadata(sp4)$date <- as.Date('2009-01-01')
#' metadata(sp4)$citation <- 'McGahan, D.G., Southard, R.J, Claassen, V.P.
#' 2009. Plant-Available Calcium Varies Widely in Soils
#' on Serpentinite Landscapes. Soil Sci. Soc. Am. J. 73: 2087-2095.'
#'
#' depth_units(sp4) <- 'cm' # fix depth units, back to 'cm'
#'
#' # further inspection with common function overloads
#' length(sp4) # number of profiles in the collection
#' nrow(sp4) # number of horizons in the collection
#' names(sp4) # column names
#' min(sp4) # shallowest profile depth in collection
#' max(sp4) # deepest profile depth in collection
#'
#' # extraction of soil profile components
#' profile_id(sp4) # vector of profile IDs
#' horizons(sp4) # horizon data
#'
#' # extraction of specific horizon attributes
#' sp4$clay # vector of clay content
#'
#' # subsetting SoilProfileCollection objects
#' sp4[1, ] # first profile in the collection
#' sp4[, 1] # first horizon from each profile
#'
#' # basic plot method, highly customizable: see manual page ?plotSPC
#' plot(sp4)
#' # inspect plotting area, very simple to overlay graphical elements
#' abline(v=1:length(sp4), lty=3, col='blue')
#' # profiles are centered at integers, from 1 to length(obj)
#' axis(1, line=-1.5, at=1:10, cex.axis=0.75, font=4, col='blue', lwd=2)
#' # y-axis is based on profile depths
#' axis(2, line=-1, at=pretty(1:max(sp4)), cex.axis=0.75, font=4, las=1, col='blue', lwd=2)
#'
#'
#' # symbolize soil properties via color
#' par(mar=c(0,0,4,0))
#' plot(sp4, color='clay')
#' plot(sp4, color='CF')
#'
#' # apply a function to each profile, returning a single value per profile,
#' # in the same order as profile_id(sp4)
#' soil.depths <- profileApply(sp4, max) # recall that max() gives the depth of a soil profile
#'
#' # check that the order is correct
#' all.equal(names(soil.depths), profile_id(sp4))
#'
#' # a vector of values that is the same length as the number of profiles
#' # can be stored into site-level data
#' sp4$depth <- soil.depths
#' # check: looks good
#' max(sp4[1, ]) == sp4$depth[1]
#'
#' # extract site-level data
#' site(sp4) # as a data.frame
#' sp4$depth # specific columns as a vector
#'
#' # use site-level data to alter plotting order
#' new.order <- order(sp4$depth) # the result is an index of rank
#' par(mar=c(0,0,0,0))
#' plot(sp4, plot.order=new.order)
#'
#' # deconstruct SoilProfileCollection into a data.frame, with horizon+site data
#' as(sp4, 'data.frame')
#'
NULL

#' Sample Soil Database #5
#'
#' 296 Soil Profiles from the La Rochelle region of France (F. Carre and
#' Girard, 2002)
#'
#' These data are c/o F. Carre (Florence.CARRE@ineris.fr).
#'
#' @name sp5
#' @docType data
#' @format \preformatted{ Formal class 'SoilProfileCollection' [package "aqp"]
#' with 6 slots ..@ idcol : chr "soil" ..@ depthcols: chr [1:2] "top" "bottom"
#' ..@ metadata :'data.frame': 1 obs. of 1 variable: .. ..$ depth_units: chr
#' "cm" ..@ horizons :'data.frame': 1539 obs. of 17 variables: .. ..$ soil :
#' soil ID .. ..$ sand : sand .. ..$ silt : silt .. ..$ clay : clay .. ..$ R25
#' : RGB r-coordinate .. ..$ G25 : RGB g-coordinate .. ..$ B25 : RGB
#' b-coordinate .. ..$ pH : pH .. ..$ EC : EC .. ..$ CaCO3 : CaC03 content ..
#' ..$ C : C content .. ..$ Ca : Ca .. ..$ Mg : Mg .. ..$ Na : Na .. ..$ top :
#' horizon top boundary (cm) .. ..$ bottom : horizon bottom boundary (cm) ..
#' ..$ soil_color: soil color in r-friendly format ..@ site :'data.frame': 296
#' obs. of 1 variable: .. ..$ soil: chr [1:296] "soil1" "soil10" "soil100"
#' "soil101" ...  ..@ sp :Formal class 'SpatialPoints' [package "sp"] with 3
#' slots .. .. ..@ coords : num [1, 1] 0 .. .. ..@ bbox : logi [1, 1] NA .. ..
#' ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots .. .. .. ..
#' ..@ projargs: chr NA }
#' @references F. Carre, M.C. Girard. 2002. Quantitative mapping of soil types
#' based on regression kriging of taxonomic distances with landform and land
#' cover attributes. Geoderma. 110: 241--263.
#' @source 296 Soil Profiles from the La Rochelle region of France (F. Carre
#' and Girard, 2002). These data can be found on the OSACA project page
#' (\url{http://eusoils.jrc.ec.europa.eu/projects/OSACA/}).
#' @keywords datasets
#' @examples
#'
#' library(scales)
#' data(sp5)
#' par(mar=c(1,1,1,1))
#' # plot a random sampling of profiles
#' s <- sample(1:length(sp5), size=25)
#' plot(sp5[s, ], divide.hz=FALSE)
#'
#' # plot the first 100 profiles, as 4 rows of 25, hard-coding the max depth
#' layout(matrix(c(1,2,3,4), ncol=1), height=c(0.25,0.25,0.25,0.25))
#' plot(sp5[1:25, ], max.depth=300)
#' plot(sp5[26:50, ], max.depth=300)
#' plot(sp5[51:75, ], max.depth=300)
#' plot(sp5[76:100, ], max.depth=300)
#'
#'
#' # 4x1 matrix of plotting areas
#' layout(matrix(c(1,2,3,4), ncol=1), height=c(0.25,0.25,0.25,0.25))
#'
#' # plot profiles, with points added to the mid-points of randomly selected horizons
#' sub <- sp5[1:25, ]
#' plot(sub, max.depth=300) ; mtext('Set 1', 2, line=-0.5, font=2)
#' y.p <- profileApply(sub, function(x) {
#'   s <- sample(1:nrow(x), 1)
#'   h <- horizons(x); with(h[s,], (top+bottom)/2)
#'   })
#' points(1:25, y.p, bg='white', pch=21)
#'
#' # plot profiles, with arrows pointing to profile bottoms
#' sub <- sp5[26:50, ]
#' plot(sub, max.depth=300); mtext('Set 2', 2, line=-0.5, font=2)
#' y.a <- profileApply(sub, function(x) max(x))
#' arrows(1:25, y.a-50, 1:25, y.a, len=0.1, col='white')
#'
#' # plot profiles, with points connected by lines: ideally reflecting some kind of measured data
#' sub <- sp5[51:75, ]
#' plot(sub, max.depth=300); mtext('Set 3', 2, line=-0.5, font=2)
#' y.p <- 20*(sin(1:25) + 2*cos(1:25) + 5)
#' points(1:25, y.p, bg='white', pch=21)
#' lines(1:25, y.p, lty=2)
#'
#' # plot profiles, with polygons connecting horizons with max clay content (+/-) 10 cm
#' sub <- sp5[76:100, ]
#' y.clay.max <- profileApply(sub, function(x) {
#'   i <- which.max(x$clay)
#'   h <- horizons(x)
#'   with(h[i, ], (top+bottom)/2)
#'   } )
#'
#' plot(sub, max.depth=300); mtext('Set 4', 2, line=-0.5, font=2)
#' polygon(c(1:25, 25:1), c(y.clay.max-10, rev(y.clay.max+10)),
#' border='black', col=rgb(0,0,0.8, alpha=0.25))
#' points(1:25, y.clay.max, pch=21, bg='white')
#'
#' # close plot
#' dev.off()
#'
#'
#' # plotting parameters
#' yo <- 100 # y-offset
#' sf <- 0.65 # scaling factor
#' # plot profile sketches
#' par(mar=c(0,0,0,0))
#' plot(sp5[1:25, ], max.depth=300, y.offset=yo, scaling.factor=sf)
#' # optionally add describe plotting area above profiles with lines
#' # abline(h=c(0,90,100, (300*sf)+yo), lty=2)
#' # simulate an environmental variable associated with profiles (elevation, etc.)
#' r <- vector(mode='numeric', length=25)
#' r[1] <- -50 ; for(i in 2:25) {r[i] <- r[i-1] + rnorm(mean=-1, sd=25, n=1)}
#' # rescale
#' r <- rescale(r, to=c(80, 0))
#' # illustrate gradient with points/lines/arrows
#' lines(1:25, r)
#' points(1:25, r, pch=16)
#' arrows(1:25, r, 1:25, 95, len=0.1)
#' # add scale for simulated gradient
#' axis(2, at=pretty(0:80), labels=rev(pretty(0:80)), line=-1, cex.axis=0.75, las=2)
#' # depict a secondary environmental gradient with polygons (water table depth, etc.)
#' polygon(c(1:25, 25:1), c((100-r)+150, rep((300*sf)+yo, times=25)),
#' border='black', col=rgb(0,0,0.8, alpha=0.25))
#'
#'
#' ##
#' # sample 25 profiles from the collection
#' s <- sp5[sample(1:length(sp5), size=25), ]
#' # compute pair-wise dissimilarity
#' d <- NCSP(s, vars=c('R25','pH','clay','EC'), k = 0, maxDepth = 300)
#' # keep only the dissimilarity between profile 1 and all others
#' d.1 <- as.matrix(d)[1, ]
#' # rescale dissimilarities
#' d.1 <- rescale(d.1, to=c(80, 0))
#' # sort in ascending order
#' d.1.order <- rev(order(d.1))
#' # plotting parameters
#' yo <- 100 # y-offset
#' sf <- 0.65 # scaling factor
#' # plot sketches
#' par(mar=c(0,0,0,0))
#' plot(s, max.depth=300, y.offset=yo, scaling.factor=sf, plot.order=d.1.order)
#' # add dissimilarity values with lines/points
#' lines(1:25, d.1[d.1.order])
#' points(1:25, d.1[d.1.order], pch=16)
#' # link dissimilarity values with profile sketches via arrows
#' arrows(1:25, d.1[d.1.order], 1:25, 95, len=0.1)
#' # add an axis for the dissimilarity scale
#' axis(2, at=pretty(0:80), labels=rev(pretty(0:80)), line=-1, cex.axis=0.75, las=2)
#'
#'
#'
NULL

#' Soil Physical and Chemical Data from Manganiferous Soils
#'
#' Soil Physical and Chemical Data from Manganiferous Soils (Bourgault and
#' Rabenhorst, 2011)
#'
#' Selected soil physical and chemical data from (Bourgault and Rabenhorst,
#' 2011).
#'
#' @name sp6
#' @docType data
#' @format A data frame with 30 observations on the following 13 variables.
#' \describe{ \item{id}{pedon name} \item{name}{horizon
#' designation} \item{top}{horizon top boundary in cm}
#' \item{bottom}{horizon bottom boundary in cm}
#' \item{color}{moist soil color in Munsell notation}
#' \item{texture}{USDA soil texture class} \item{sand}{sand
#' content by weight percentage} \item{silt}{silt content by weight
#' percentage} \item{clay}{clay content by weight percentage}
#' \item{Fe}{DCB-extracted Fe in g/kg (see citation)}
#' \item{Mn}{DCB-extracted Mn in g/kg (see citation)}
#' \item{C}{total organic carbon as g/kg} \item{pH}{measured in
#' 1:1 H20 slurry} \item{Db}{bulk density (g/cc), clod method} }
#' @references Rebecca R. Bourgault, Martin C. Rabenhorst. 2011.  Genesis and
#' characterization of manganiferous soils in the Eastern Piedmont, USA.
#' Geoderma. 165:84-94.
#' @source http://www.sciencedirect.com/science/article/pii/S0016706111001972
#' @keywords datasets
#' @examples
#'
#'   # setup environment
#'   library(aqp)
#'   data(sp6)
#'
#'   # init SPC
#'   depths(sp6) <- id ~ top + bottom
#'   # convert non-standard Munsell colors
#'   sp6$soil_color <- getClosestMunsellChip(sp6$color)
#'
#'   # profile sketches
#'   par(mar=c(0,0,3,0))
#'   plot(sp6, color='soil_color')
#'   plot(sp6, color='Mn')
#'   plot(sp6, color='Fe')
#'   plot(sp6, color='pH')
#'   plot(sp6, color='texture')
#'
#'
NULL


#' Soil Physical and Chemical Data Related to Studies in the Sierra Nevada
#' Mountains, CA, USA.
#'
#' Soil physical and chemical data associated with two bio-climatic sequences
#' (granitic and andesitic parent material) from the western flank of the
#' Sierra Nevada mountains.
#'
#' These data were assembled from Dahlgren et al. (1997) and Rasmussen et al.
#' (2007), with permission granted by lead authors, by D.E. Beaudette.
#'
#' @name sierraTransect
#' @docType data
#' @usage data(sierraTransect)
#'
#' @references R.A. Dahlgren, J.L. Boettinger, G.L. Huntington, R.G. Amundson.
#' Soil development along an elevational transect in the western Sierra Nevada,
#' California, Geoderma, Volume 78, Issues 3–4, 1997, Pages 207-236.
#'
#' Rasmussen, C., Matsuyama, N., Dahlgren, R.A., Southard, R.J. and Brauer, N.
#' (2007), Soil Genesis and Mineral Transformation Across an Environmental
#' Gradient on Andesitic Lahar. Soil Sci. Soc. Am. J., 71: 225-237.
#' @source Original manuscripts and person communication with authors.
#' @keywords datasets
#' @examples
#'
#' data(sierraTransect)
#'
#' # tighter margins
#' op <- par(mar=c(0,0,0,0))
#'
#' # quick sketch
#' plotSPC(sierraTransect, name.style = 'center-center', width=0.3)
#'
#' # split by transect
#' par(mar=c(0,0,1,1))
#' groupedProfilePlot(
#' sierraTransect, groups='transect',
#' group.name.offset = -15, width=0.3,
#' name.style='center-center'
#' )
#'
#' # thematic
#' groupedProfilePlot(
#' sierraTransect, groups='transect',
#' group.name.offset = -15, width=0.3,
#' name.style='center-center', color='Fe_o_to_Fe_d'
#' )
#'
#' # horizon boundary viz
#' sierraTransect$hzd <- hzDistinctnessCodeToOffset(substr(sierraTransect$hz_boundary, 0, 1))
#'
#' groupedProfilePlot(
#' sierraTransect, groups='transect', group.name.offset = -15,
#' width=0.3, name.style='center-center', color='Fe_o_to_Fe_d',
#' hz.distinctness.offset='hzd')
#'
#'
#' # split transects
#' g <- subset(sierraTransect, transect == 'Granite')
#' a <- subset(sierraTransect, transect == 'Andesite')
#'
#' g.order <- order(g$elev)
#' a.order <- order(a$elev)
#'
#' # order (left -> right) by elevation
#' par(mar=c(2,0,0,2), mfrow=c(2,1))
#' plot(g, width=0.3, name.style='center-center', cex.names=0.75, plot.order=g.order)
#' axis(1, at=1:length(g), labels=g$elev[g.order], line=-1.5)
#'
#' plot(a, width=0.3, name.style='center-center', cex.names=0.75, plot.order=a.order)
#' axis(1, at=1:length(a), labels=a$elev[a.order], line=-1.5)
#'
#'
#' par(op)
#'
"sierraTransect"

#' Soil Data from the Central Sierra Nevada Region of California
#'
#' Site and laboratory data from soils sampled in the central Sierra Nevada
#' Region of California.
#'
#' These data were extracted from the NSSL database. `ca630` is a list composed
#' of site and lab data, each stored as `data.frame` objects. These data are modeled by a
#' 1:many (site:lab) relation, with the `pedon_id` acting as the primary key in
#' the `site` table and as the foreign key in the `lab` table.
#'
#' @name ca630
#' @docType data
#' @usage data(ca630)
#' @format List containing:
#'
#' $site : A data frame containing site information.  \describe{
#' \item{user_site_id}{national user site id} \item{mlra}{the
#' MLRA} \item{county}{the county} \item{ssa}{soil survey area}
#' \item{lon}{longitude, WGS84} \item{lat}{latitude, WGS84}
#' \item{pedon_key}{national soil profile id}
#' \item{user_pedon_id}{local soil profile id}
#' \item{cntrl_depth_to_top}{control section top depth (cm)}
#' \item{cntrl_depth_to_bot}{control section bottom depth (cm)}
#' \item{sampled_taxon_name}{soil series name} }
#'
#' $lab : A data frame containing horizon information.  \describe{
#' \item{pedon_key}{national soil profile id}
#' \item{layer_key}{national horizon id}
#' \item{layer_sequence}{horizon sequence number}
#' \item{hzn_top}{horizon top (cm)} \item{hzn_bot}{horizon
#' bottom (cm)} \item{hzn_desgn}{horizon name}
#' \item{texture_description}{USDA soil texture}
#' \item{nh4_sum_bases}{sum of bases extracted by ammonium acetate (pH
#' 7)} \item{ex_acid}{exchangeable acidity \[method ?]}
#' \item{CEC8.2}{cation exchange capacity by sum of cations method (pH
#' 8.2)} \item{CEC7}{cation exchange capacity by ammonium acetate (pH
#' 7)} \item{bs_8.2}{base saturation by sum of cations method (pH 8.2)}
#' \item{bs_7}{base saturation by ammonium acetate (pH 7)} }
#' @note These data are out of date. Pending some new data + documentation. Use
#' with caution
#' @source \url{https://ncsslabdatamart.sc.egov.usda.gov/}
#' @keywords datasets
#' @examples
#'
#' \dontrun{
#' library(tactile)
#' library(lattice)
#' library(Hmisc)
#' library(sp)
#'
#' # check the data out:
#' data(ca630)
#' str(ca630)
#'
#' # note that pedon_key is the link between the two tables
#'
#' # make a copy of the horizon data
#' ca <- ca630$lab
#'
#' # promote to a SoilProfileCollection class object
#' depths(ca) <- pedon_key ~ hzn_top + hzn_bot
#'
#' # add site data, based on pedon_key
#' site(ca) <- ca630$site
#'
#' # ID data missing coordinates: '|' is a logical OR
#' (missing.coords.idx <- which(is.na(ca$lat) | is.na(ca$lon)))
#'
#' # remove missing coordinates by safely subsetting
#' if(length(missing.coords.idx) > 0)
#' 	ca <- ca[-missing.coords.idx, ]
#'
#' # register spatial data
#' initSpatial(ca) <- ~ lon + lat
#'
#' # assign a coordinate reference system
#' prj(ca) <- 'EPSG:4269'
#'
#' # check the result
#' print(ca)
#'
#' # aggregate %BS 7 for all profiles into 1 cm slices
#' a <- slab(ca, fm= ~ bs_7)
#'
#' # plot median & IQR by 1 cm slice
#' xyplot(
#' top ~ p.q50, 
#' data = a, 
#' lower=a$p.q25, 
#' upper=a$p.q75,
#' alpha=0.5,
#' ylim=c(160,-5), 
#' scales = list(alternating = 1, y = list(tick.num = 7)),
#' panel = panel.depth_function, 
#' prepanel = prepanel.depth_function,
#' ylab='Depth (cm)', xlab='Base Saturation at pH 7',
#' par.settings = tactile.theme(superpose.line = list(col = 'black', lwd = 2))
#' )
#'
#' # aggregate %BS at pH 8.2 for all profiles by MLRA, along 1 cm slices
#' # note that mlra is stored in @site
#' a <- slab(ca, mlra ~ bs_8.2)
#'
#' # keep only MLRA 18 and 22
#' a <- subset(a, subset=mlra %in% c('18', '22'))
#'
#' # plot median & IQR by 1 cm slice, using different colors for each MLRA
#' xyplot(
#' top ~ p.q50, 
#' groups = factor(mlra), 
#' data = a,
#' lower=a$p.q25, 
#' upper=a$p.q75,
#' alpha=0.25,
#' sync.colors = TRUE,
#' ylim=c(160,-5), 
#' scales = list(alternating = 1, y = list(tick.num = 7)),
#' panel = panel.depth_function, 
#' prepanel = prepanel.depth_function,
#' ylab='Depth (cm)', xlab='Base Saturation at pH 7',
#' par.settings = tactile.theme(superpose.line = list(lwd = 2)),
#' auto.key = list(lines = TRUE, points = FALSE, columns = 2)
#' )
#'
#'
#'
#' # Extract the 2nd horizon from all profiles as SPDF
#' ca.2 <- ca[, 2]
#'
#' # subset profiles 1 through 10
#' ca.1.to.10 <- ca[1:10, ]
#'
#' # basic plot method: profile plot
#' par(mar = c(0, 0, 3, 1))
#' plotSPC(ca.1.to.10, name='hzn_desgn', color = 'CEC7')
#' }
#'
"ca630"

#' Soil Morphologic, Geochemical, and Mineralogy Data from Rowley et al. 2019.
#'
#' Data from Table 1 and Supplementary Tables 1 and 2 from "A cascading
#' influence of calcium carbonate on the biogeochemistry and pedogenic
#' trajectories of subalpine soils, Switzerland".
#'
#'
#' @name rowley2019
#' @docType data
#' @usage data(rowley2019)
#' @format A \code{SoilProfileCollection} object:
#'
#' site-level attributes \describe{ \item{id}{profile ID} \item{group}{profile
#' group} }
#'
#' horizon-level attributes \describe{ \item{sample_id}{sample ID}
#' \item{name}{horizon name} \item{pH}{pH} \item{Al_exch}{cmol(+) / kg,
#' exchangeable Al} \item{Ca_exch}{cmol(+) / kg, exchangeable Ca}
#' \item{CEC_sum}{cmol(+) / kg, cation exchange capacity calculated as the sum
#' of exchangeable cations, not including H+}
#' \item{Ca_exch_saturation}{percent} \item{Al_exch_saturation}{percent}
#' \item{TON}{percent, total nitrogen} \item{SOC}{percent, soil organic carbon}
#' \item{C_to_N}{carbon to nitrogen ratio} \item{Alo}{g/kg, oxalate-extractable
#' Al} \item{Feo}{g/kg, oxalate-extractable Fe} \item{Ald}{g/kg,
#' dithionite-extractable Al} \item{Fed}{g/kg, dithionite-extractable Fe}
#' \item{Feo_Fed}{Fe_o to Fe_d ratio} \item{id}{profile ID} \item{top}{horizon
#' top (cm)} \item{bottom}{horizon bottom (cm)} \item{Al}{g/kg by x-ray
#' fluorescence} \item{Ca}{g/kg by x-ray fluorescence} \item{Cr}{g/kg by x-ray
#' fluorescence} \item{Fe}{g/kg by x-ray fluorescence} \item{K}{g/kg by x-ray
#' fluorescence} \item{Mg}{g/kg by x-ray fluorescence} \item{Mn}{g/kg by x-ray
#' fluorescence} \item{Na}{g/kg by x-ray fluorescence} \item{Ni}{g/kg by x-ray
#' fluorescence} \item{P}{g/kg by x-ray fluorescence} \item{Si}{g/kg by x-ray
#' fluorescence} \item{Ti}{g/kg by x-ray fluorescence}
#' \item{Phyllosilicates}{percent by x-ray diffraction spectra}
#' \item{Quartz}{percent by x-ray diffraction spectra}
#' \item{K_Feldspar}{percent by x-ray diffraction spectra}
#' \item{Na_Plagioclase}{percent by x-ray diffraction spectra}
#' \item{Goethite}{percent by x-ray diffraction spectra}
#' \item{Unidentified}{percent by x-ray diffraction spectra}
#' \item{CCE_Total}{percent} \item{CCE_Reactive}{percent}
#' \item{Reactive_carbonate}{percent} \item{Sand}{percent <2um}
#' \item{Silt}{percent 2-50um} \item{Clay}{percent 50-2000um}
#'
#' \item{CaH2O}{Milliq ex: grams of Ca per kilogram of dry soil (g kg-1)}
#' \item{Ca2MKCl}{2M KCl: grams of Ca per kilogram of dry soil (g kg-1)}
#' \item{CaNa2EDTA}{0.05 M Na2EDTA: grams of Ca per kilogram of dry soil (g
#' kg-1)} \item{CaCuCl2}{0.5 M CuCl2: grams of Ca per kilogram of dry soil (g
#' kg-1)}
#'
#' \item{hzID}{horizon ID} }
#' @references Mike C. Rowley, Stephanie Grand, Thierry Adatte, Eric P.
#' Verrecchia, Cascading influence of calcium carbonate on the biogeochemistry and
#' pedogenic trajectories of subalpine soils), Switzerland, Geoderma, 2019,
#' 114065, ISSN 0016-7061, \doi{10.1016/j.geoderma.2019.114065}.
#' @keywords datasets
#' @examples
#'
#' library(lattice)
#'
#' # load data
#' data('rowley2019')
#'
#' # check first 5 rows and 10 columns of horizon data
#' horizons(rowley2019)[1:5, 1:10]
#'
#' # check site data
#' site(rowley2019)
#'
#' # graphical summary
#' par(mar=c(1,1,3,1))
#' plotSPC(rowley2019, color='Feo_Fed', name='name', cex.names=0.85)
#'
#' plotSPC(rowley2019, color='Ca_exch', name='name', cex.names=0.85)
#'
#' # grouped plot
#' groupedProfilePlot(rowley2019, groups = 'group', color='Ca_exch',
#' name='name', cex.names=0.85, group.name.offset = -10)
#'
#' # aggregate over 1cm slices, for select properties
#' a <- slab(rowley2019, group ~ Reactive_carbonate + Ca_exch + pH + K_Feldspar + Na_Plagioclase + Al)
#'
#' # plot styling
#' tps <- list(superpose.line=list(lwd=2, col=c('royalblue', 'firebrick')))
#'
#' # make the figure
#' xyplot(top ~ p.q50 | variable, data=a, ylab='Depth', groups=group,
#'        main='', as.table=TRUE,
#'        xlab='median bounded by 25th and 75th percentiles',
#'        lower=a$p.q25, upper=a$p.q75, ylim=c(55,-5),
#'        panel=panel.depth_function,
#'        prepanel=prepanel.depth_function,
#'        cf=a$contributing_fraction,
#'        alpha=0.33, sync.colors=TRUE,
#'        scales=list(x=list(relation='free', alternating=1)),
#'        par.settings=tps,
#'        auto.key=list(columns=2, lines=TRUE, points=FALSE),
#'        strip=strip.custom(bg=grey(0.9))
#' )
#'
#'
"rowley2019"

#' Soil Morphologic Data from Jacobs et al. 2002.
#'
#' Select soil morphologic data from "Redoximorphic Features as Indicators of
#' Seasonal Saturation, Lowndes County, Georgia". This is a useful sample
#' dataset for testing the analysis and visualization of redoximorphic
#' features.
#'
#'
#' @name jacobs2000
#' @docType data
#'
#' @format A \code{SoilProfileCollection} object.
#'
#' @references Jacobs, P. M., L. T. West, and J. N. Shaw. 2002. Redoximorphic
#' Features as Indicators of Seasonal Saturation, Lowndes County, Georgia. Soil
#' Sci. Soc. Am. J. 66:315-323. doi:10.2136/sssaj2002.3150
#' @usage data(jacobs2000)
#'
#' @keywords datasets
#'
#' @examples
#'
#' # load
#' data(jacobs2000)
#'
#' # basic plot
#' par(mar=c(0,1,3,3))
#' plot(jacobs2000, name='name', color='matrix_color', width=0.3)
#' # add concentrations
#' addVolumeFraction(jacobs2000, 'concentration_pct',
#' col = jacobs2000$concentration_color, pch = 16, cex.max = 0.5)
#'
#' # add depletions
#' plot(jacobs2000, name='name', color='matrix_color', width=0.3)
#' addVolumeFraction(jacobs2000, 'depletion_pct',
#' col = jacobs2000$depletion_color, pch = 16, cex.max = 0.5)
#'
#' # time saturated
#' plotSPC(jacobs2000, color='time_saturated', cex.names=0.8, col.label = 'Time Saturated')
#'
#' # color contrast: matrix vs. concentrations
#' cc <- colorContrast(jacobs2000$matrix_color_munsell, jacobs2000$concentration_munsell)
#' cc <- na.omit(cc)
#'
#' cc <- cc[order(cc$dE00), ]
#' cc <- unique(cc)
#'
#' par(bg='black', fg='white')
#' colorContrastPlot(cc$m1[1:10], cc$m2[1:10], labels = c('matrix', 'concentration'))
#' colorContrastPlot(cc$m1[11:21], cc$m2[11:21], labels = c('matrix', 'concentration'))
#'
#'
#' # color contrast: depletion vs. concentrations
#' cc <- colorContrast(jacobs2000$depletion_munsell, jacobs2000$concentration_munsell)
#' cc <- na.omit(cc)
#'
#' cc <- cc[order(cc$dE00), ]
#' cc <- unique(cc)
#'
#' par(bg='black', fg='white')
#' colorContrastPlot(cc$m1, cc$m2, labels = c('depletion', 'concentration'))
#'
#'
"jacobs2000"

#' Average Hydraulic Parameters from the ROSETTA Model by USDA Soil Texture
#' Class
#'
#' Average soil hydraulic parameters generated by the first stage predictions
#' of the ROSETTA model by USDA soil texture class. These data were extracted
#' from [ROSETTA documentation](https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-class-average-hydraulic-parameters/) and re-formatted for ease of use.
#'
#' Theoretical water-retention parameters for uniform soil material of each
#' texture class have been estimated via van Genuchten model.
#'
#' See [the related tutorial](https://ncss-tech.github.io/AQP/soilDB/fetchKSSL-VG-demo.html)
#'
#' @name ROSETTA.centroids
#' @docType data
#' @usage data(ROSETTA.centroids)
#' @format A data frame: \describe{
#'
#' \item{texture}{soil texture class, ordered from low to high
#' available water holding capacity} \item{theta_r}{average saturated
#' water content} \item{theta_s}{average residual water content}
#' \item{alpha}{average value, related to the inverse of the air entry
#' suction, log10-transformed values with units of cm}
#' \item{npar}{average value, index of pore size distribution,
#' log10-transformed values with units of 1/cm}
#'
#' \item{theta_r_sd}{1 standard deviation of \code{theta_r}}
#' \item{theta_s_sd}{1 standard deviation of \code{theta_s}}
#' \item{alpha_sd}{1 standard deviation of \code{alpha}}
#' \item{npar_sd}{1 standard deviation of \code{npar}}
#'
#' \item{sat}{approximate volumetric water content at which soil
#' material is saturated} \item{fc}{approximate volumetric water
#' content at which matrix potential = -33kPa} \item{pwp}{approximate
#' volumetric water content at which matrix potential = -1500kPa}
#' \item{awc}{approximate available water holding capacity: VWC(-33kPa)
#' - VWC(-1500kPa)} }
#'
#' @references van Genuchten, M.Th. (1980). "A closed-form equation for
#' predicting the hydraulic conductivity of unsaturated soils". Soil Science
#' Society of America Journal. 44 (5): 892-898.
#'
#' Schaap, M.G., F.J. Leij, and M.Th. van Genuchten. 2001. ROSETTA: A computer
#' program for estimating soil hydraulic parameters with hierarchical
#' pedotransfer functions. Journal of Hydrology 251(3–4): 163-176.
#' @source
#' [ROSETTA Class Average Hydraulic Parameters](https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-class-average-hydraulic-parameters/)
#' @keywords datasets
#' @examples
#'
#'
#' \dontrun{
#'
#' library(aqp)
#' library(soilDB)
#' library(latticeExtra)
#'
#' data("ROSETTA.centroids")
#'
#' # iterate over horizons and generate VG model curve
#' res <- lapply(1:nrow(ROSETTA.centroids), function(i) {
#'   m <- KSSL_VG_model(VG_params = ROSETTA.centroids[i, ], phi_min = 10^-3, phi_max=10^6)$VG_curve
#'   # copy generalized hz label
#'   m$hz <- ROSETTA.centroids$hz[i]
#'   # copy ID
#'   m$texture_class <- ROSETTA.centroids$texture[i]
#'   return(m)
#' })
#'
#' # copy over lab sample number as ID
#' res <- do.call('rbind', res)
#'
#' # check: OK
#' str(res)
#'
#'
#' # visual check: OK
#' xyplot(
#'   phi ~ theta | texture_class, data=res,
#'   type=c('l', 'g'),
#'   scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)),
#'   yscale.components=yscale.components.logpower,
#'   ylab=expression(Suction~~(kPa)),
#'   xlab=expression(Volumetric~Water~Content~~(cm^3/cm^3)),
#'   par.settings = list(superpose.line=list(col='RoyalBlue', lwd=2)),
#'   strip=strip.custom(bg=grey(0.85)),
#'   as.table=TRUE
#' )
#'
#'
#' }
#'
#'
"ROSETTA.centroids"


#'
#' @title US State Soils
#' @description A listing of the 50 US state soils, along with Puerto Rico and Virgin Islands.
#' @keywords datasets
#' @usage data(us.state.soils)
#' @format
#' \describe{
#'   \item{state}{state name}
#'   \item{abbreviated}{abbreviated state name}
#'   \item{series}{soil series name}
#' }
#'
"us.state.soils"

#' Munsell to sRGB Lookup Table for Common Soil Colors
#'
#' A lookup table of interpolated Munsell color chips for common soil colors.
#'
#' See \code{munsell2rgb} for conversion examples. Note that this table does
#' not currently have entries for values of 2.5--common in most soil color
#' books. These chips should be added in the next major release of aqp. Values
#' are referenced to the D65 standard illuminant.
#'
#' @name munsell
#' @docType data
#'
#' @format A data frame with 8825 rows.  \describe{ \item{hue}{Munsell
#' Hue, upper case} \item{value}{Munsell Value}
#' \item{chroma}{Munsell Chroma} \item{r}{sRGB "red" value
#' (0-1)} \item{g}{sRGB "green" value (0-1)} \item{b}{sRGB
#' "blue" value (0-1)} \item{L}{CIE LAB "L" coordinate}
#' \item{A}{CIE LAB "A" coordinate} \item{B}{CIE LAB "B"
#' coordinate} }
#'
#' @usage data(munsell)
#'
#' @references \describe{
#' \item{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}{Color
#' conversion equations}
#' \item{http://dx.doi.org/10.1016/j.cageo.2012.10.020}{Methods used to
#' generate this table} }
#' @source Color chip XYZ values:
#' http://www.rit.edu/cos/colorscience/rc_munsell_renotation.php
#' @keywords datasets
#' @examples
#'
#' data(munsell)
#'
"munsell"


#' Munsell Colors of Common Soil Minerals
#'
#' Munsell colors for some common soil minerals.
#'
#' Soil color and other properties including texture, structure, and
#' consistence are used to distinguish and identify soil horizons (layers) and
#' to group soils according to the soil classification system called Soil
#' Taxonomy. Color development and distribution of color within a soil profile
#' are part of weathering. As rocks containing iron or manganese weather, the
#' elements oxidize. Iron forms small crystals with a yellow or red color,
#' organic matter decomposes into black humus, and manganese forms black
#' mineral deposits. These pigments paint the soil (Michigan State Soil). Color
#' is also affected by the environment: aerobic environments produce sweeping
#' vistas of uniform or subtly changing color, and anaerobic (lacking oxygen),
#' wet environments disrupt color flow with complex, often intriguing patterns
#' and points of accent. With depth below the soil surface, colors usually
#' become lighter, yellower, or redder.
#'
#' @name soil_minerals
#' @docType data
#' @format A data frame with 20 observations on the following 5 variables.
#' \describe{ \item{mineral}{mineral name} \item{color}{Munsell
#' color} \item{hue}{Munsell hue} \item{value}{Munsell value}
#' \item{chroma}{Munsell chroma} }
#' @usage data(soil_minerals)
#' @references 1. Lynn, W.C. and Pearson, M.J., The Color of Soil, The Science
#' Teacher, May 2000. 2. Schwertmann, U. 1993. Relations Between Iron Oxides,
#' Soil Color, and Soil Formation. "Soil Color". SSSA Special Publication no.
#' 31, pages 51--69.
#' @keywords datasets
#' @examples
#'
#' \dontrun{
#' 
#' library(aqp)
#' library(ape)
#' library(cluster)
#' library(farver)
#' 
#' # load common soil mineral colors
#' data(soil_minerals)
#' 
#' # convert Munsell to R colors
#' soil_minerals$col <- munsell2rgb(
#'   soil_minerals$hue, 
#'   soil_minerals$value,
#'   soil_minerals$chroma
#' )
#' 
#' # make a grid for plotting
#' n <- ceiling(sqrt(nrow(soil_minerals)))
#' 
#' # read from top-left to bottom-right
#' g <- expand.grid(x=1:n, y=n:1)[1:nrow(soil_minerals),]
#' 
#' # convert Munsell -> sRGB -> LAB
#' col.rgb <- munsell2rgb(
#'   soil_minerals$hue,
#'   soil_minerals$value,
#'   soil_minerals$chroma,
#'   return_triplets = TRUE
#' )
#' 
#' # sRGB values expected to be in the range [0,255]
#' col.rgb <- col.rgb * 255
#' 
#' # convert from sRGB -> CIE LAB
#' col.lab <- convert_colour(
#'   col.rgb , from = 'rgb',
#'   to = 'lab', white_from = 'D65'
#' )
#' 
#' # keep track of soil mineral names
#' # in a way that will persist in a dist obj
#' row.names(col.lab) <- soil_minerals$mineral
#' 
#' # perceptual distance via CIE dE00
#' d <- compare_colour(
#'   from = col.lab,
#'   to = col.lab,
#'   from_space = 'lab',
#'   to_space = 'lab',
#'   white_from = 'D65',
#'   method = 'CIE2000'
#' )
#' 
#' # matrix -> dist
#' d <- as.dist(d)
#' 
#' # divisive hierarchical clustering of LAB coordinates
#' h <- as.hclust(diana(d))
#' p <- as.phylo(h)
#' 
#' # colors, in order based on clustering
#' # starting from top-left
#' min.cols <- rev(soil_minerals$col[h$order])
#' 
#' # mineral names, in order based on clustering
#' # starting from top-left
#' min.names <- rev(soil_minerals$mineral[h$order])
#' 
#' min.munsell <- rev(soil_minerals$color[h$order])
#' 
#' # plot grid of mineral names / colors
#' layout(matrix(c(1, 2), nrow = 1), widths = c(1.25, 1))
#' 
#' par(mar = c(1, 0, 0, 1))
#' plot(g$x, g$y, pch = 15, cex = 12, axes = FALSE, xlab = '', ylab = '',
#'      col = min.cols, 
#'      xlim = c(0.5, 5.5), ylim = c(1.5, 5.5)
#' )
#' text(g$x, g$y, min.names, adj = c(0.45, 5.5), cex = 0.75, font = 2)
#' text(g$x, g$y, min.munsell, col = invertLabelColor(min.cols), cex = 0.85, font = 2)
#' 
#' title(main = 'Common Soil Pigments', line = -1.75, cex.main = 2)
#' mtext('U. Schwertmann, 1993. SSSA Special Publication no. 31, pages 51--69', side = 1,
#'       cex = 0.75, line = -1.5)
#' 
#' # dendrogram + tip labels with mineral colors
#' plot(p, cex = 0.85, label.offset = 5, font = 1)
#' tiplabels(pch = 15, cex = 3, offset = 2, col = soil_minerals$col)

#'
#' }
#'
#'
"soil_minerals"

#'
#' @title Spectral Library of Munsell Colors
#'
#' @description
#'
#' The original database "SpectralReflectancesOf2007MunsellBookOfColorGlossy.txt" was provided by Paul Centore and downloaded July, 2020. Reflectance values for odd chroma have been interpolated from adjacent chips. See \code{aqp/misc/utils/Munsell/} for the entire set of processing steps.
#'
#' Munsell value typically ranges from 2-9, and chroma from 1-12. Ranges vary by hue. Run \code{aqp:::.summarizeMunsellSpectraRanges()} for a detailed listing by hue.
#'
#' The original database contains the following description:
#'
#' This file contains spectral reflectance measurements of X-Rite's 2007 Munsell Book of Color (Glossy Finish).  The measurements were made in 2012 with a ColorMunki spectrophotometer.  The first column is the Munsell name.  The remaining columns give reflectance values for 380 nm to 730 nm, in steps of 10 nm.  The reflectance is a value between 0 (indicating that no light at that wavelength is reflected) and 1 (indicating that all the light at that wavelength is reflected).  Occasionally an entry is slightly greater than 1.  The likely cause is random variability, and those entries can be adjusted to 1 with negligible loss. In all, 1485 colour samples were measured.  Researchers are invited to analyze the data in this file.
#'
#' @usage data(munsell.spectra)
#'
#' @references
#' Centore, Paul. Colour Tools for Painters. \url{https://www.munsellcolourscienceforpainters.com/}.
#'
#' @aliases munsell.spectra.wide
#'
#' @keywords datasets
#'
#' @format A data frame with 89496 rows and 10 variables:
#' \describe{
#'   \item{munsell}{munsell color}
#'   \item{hue}{hue component}
#'   \item{value}{value component}
#'   \item{chroma}{chroma component}
#'   \item{wavelength}{wavelength (nm)}
#'   \item{reflectance}{reflectance}
#' }
"munsell.spectra"

#' @title Indices of "equivalent" Munsell chips in the `munsell` data set
#'
#' @description
#' A pre-calculated lookup list (made with `farver::compare_colour`) based on pair-wise color contrast (`CIE2000` or `dE00`) evaluated over all "chips" in the `aqp::munsell` data set.
#'
#' The intention is to identify Munsell chips that may be "functionally equivalent" to some other given whole chip elsewhere in the Munsell color space -- as discretized in the \code{aqp::munsell} lookup table.
#'
#' "Equivalent" chips are based (fairly arbitrarily) on the 0.001 probability level of `dE00` (default Type 7 `quantile`) within the upper triangle of the 8467x8467 contrast matrix. This corresponds to a `dE00` threshold of approximately 2.15.
#'
#' This is a naive (to the subtleties of human color perception, and overall magnitude of contrast between some of the "chips") but computationally consistent approach. Using the lookup list, as opposed to manual contrast via e.g. \code{farver::compare_colour} may have some benefits for efficiency in certain applications where the exact contrast value is not as important as the concept of having some threshold that is non-zero, but very small.
#'
#' @usage data(equivalent_munsell)
#'
#' @aliases equivalent_munsell
#'
#' @seealso \code{\link{equivalentMunsellChips}}
#'
#' @references
#' Gaurav Sharma, Wencheng Wu, Edul N. Dalal. (2005). The CIEDE2000 Color-Difference Formula: Implementation Notes, Supplementary Test Data, and Mathematical Observations. COLOR research and application. 30(1):21-30. http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
#'
#'  Thomas Lin Pedersen, Berendea Nicolae and Romain Francois (2020). farver: High Performance Colour Space Manipulation. R package version 2.0.3. https://CRAN.R-project.org/package=farver
#'
#' Dong, C.E., Webb, J.B., Bottrell, M.C., Saginor, I., Lee, B.D. and Stern, L.A. (2020). Strengths, Limitations, and Recommendations for Instrumental Color Measurement in Forensic Soil Characterization. J Forensic Sci, 65: 438-449. https://doi.org/10.1111/1556-4029.14193
#'
#' @keywords datasets
#'
#' @format A named list with 8467 elements, each containing a numeric vector of indices corresponding to the \code{munsell} data set, which has 8467 rows (unique, whole-number chips). Names have the format \code{HUE VALUE/CHROMA}, e.g. \code{"7.5YR 4/4"}
#'
#' @examples
#' data(equivalent_munsell)
"equivalent_munsell"



#'
#' @title Standard Illuminants and Observers
#' 
#' @description D65 and F2 standard illuminant spectral power distributions, CIE1931 Standard Observer (2 degree), and CIE1964 Supplemental Standard Observer (10 degree)
#'
#'
#'
#' @keywords datasets
#' @usage data(spectral.reference)
#' 
#' @references 
#' 
#' Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color for Science, Art, and Technology (pp. 32-96). North-Holland.
#' 
#' CIE Colorimetry – Part 1: CIE standard colorimetric observers. CIES014-1/E:2006 – ISO 11664-1:2007(E)
#' 
#' CIE. (n.d.). CIE 15:2004 Tables Data. Retrieved from https://law.resource.org/pub/us/cfr/ibr/003/cie.15.2004.tables.xls
#' 
#' @examples 
#' 
#' data("spectral.reference")
#' 
#' matplot(
#'   x = spectral.reference[, 1],
#'   y = spectral.reference[, c('xbar_2', 'ybar_2', 'zbar_2')],
#'   ylim = c(0, 2),
#'   type = 'l',
#'   lwd = 2, 
#'   lty = 1,
#'   las = 1,
#'   xlab = 'Wavelength (nm)',
#'   ylab = 'Weight | Intensity',
#'   main = "CIE1931 (2\u00B0) and CIE1964 (10\u00B0) Standard Observers
#'   D65 and F2 Illuminant Power Spectrum (rescaled / offset for clarity)",
#'   cex.main = 0.9
#' )
#' 
#' matlines(
#'   x = spectral.reference[, 1],
#'   y = spectral.reference[, c('xbar_10', 'ybar_10', 'zbar_10')],
#'   type = 'l',
#'   lwd = 2, 
#'   lty = 2,
#'   las = 1,
#'   xlab = 'Wavelength (nm)',
#'   ylab = 'Weight | Intensity',
#'   main = 'CIE1931 Standard Observer Weights\nD65 Standard Illuminant'
#' )
#' 
#' lines(
#'   x = spectral.reference$w,
#'   y = (spectral.reference$D65 / 100) + 0.33,
#'   lty = 1,
#'   col = 'royalblue'
#' )
#' 
#' lines(
#'   x = spectral.reference$w,
#'   y = (spectral.reference$F2 / 25) + 0.4,
#'   lty = 1,
#'   col = 'violet'
#' )
#' 
#' legend(
#'   'topright',
#'   legend = c('X_2', 'Y_2', 'Z_2', 'X_10', 'Y_10', 'Z_10', 'D65', 'F2'),
#'   col = c(1, 2, 3, 1, 2, 3, 'royalblue', 'violet'),
#'   lwd = c(2, 2, 2, 2, 2, 2, 1, 1),
#'   lty = c(1, 1, 1, 2, 2, 2, 1, 1),
#'   bty = 'n',
#'   cex = 0.85
#' )
#'
"spectral.reference"



#'
#' @title Traditional Soil Color Names
#' 
#' @description Traditional soil color names associated with select Munsell colors.
#' 
#' @references 
#' 
#' Sourced from the "colorconverter" NASIS property script.
#'
#' @keywords datasets
#' @usage data(traditionalColorNames)
#' 
"traditionalColorNames"


#' @title Munsell Hue Position Reference
#' 
#' @description Position data for the 40 standard Munsell hues (and neutral). Data include angular positions (compass-style, origin at `[x = 0, y = 1]`, CW rotation) and Cartesian coordinates on the unit circle.
#' 
#' @references 
#' 
#' Munsell book of color. 1976. Macbeth, a Division of Kollmorgen Corp., Baltimore, MD.
#'
#' @keywords datasets
#' @usage data(munsellHuePosition)
#' 
"munsellHuePosition"


#' @title pH Reaction Classes
#' 
#' @description Levels of pH (reaction) classes including descriptive name, and range from low to high pH
#' @references Soil Science Division Staff. (2017) Soil Survey Manual. C. Ditzler, K. Scheffe, and H.C. Monger (eds.). USDA Handbook 18. Government Printing Office, Washington, D.C.
#' @keywords datasets
#' @usage data(reactionclass)
#' 
"reactionclass"




