library(aqp)
library(soilDB)
library(plyr)
library(lattice)
library(reshape2)

# load Rich's data
load('pedons.rda')

# get selected set of pedons, no removal of pedons with inconsistent horizonation
# x <- fetchNASIS(rmHzErrors = FALSE)

# compute soil depth and depth class for all pedons, using pattern matching of horizon names
sdc <- getSoilDepthClass(x, p = 'Cr|R|Cd')
head(sdc)

# join depth and depth class data to pedons
site(x) <- sdc

# normalize taxonname
soils <- c('Houston Black', 'Heiden', 'Ferris', 'Vertel')
for(i in soils)
  x$taxonname[grep(i, x$taxonname, ignore.case = TRUE)] <- i
table(x$taxonname)


# access diagnostict HZ data, 1:many per pedon
d <- diagnostic_hz(x)
# keep just records of "densic" anything
idx <- grep('densic', d$diag_kind, ignore.case = TRUE)
d <- d[idx, ]
table(d$diag_kind)

# convert long -> wide format: top depths
d.wide <- dcast(d, peiid ~ diag_kind, value.var='featdept')
# fix names
names(d.wide) <- c('peiid', 'densic.contact.top', 'densic.materials.top')
# join to SPC
site(x) <- d.wide

# convert long -> wide format: bottom depths
d.wide <- dcast(d, peiid ~ diag_kind, value.var='featdepb')
# fix names
names(d.wide) <- c('peiid', 'densic.contact.bottom', 'densic.materials.bottom')
# join to SPC
site(x) <- d.wide

# check depth via hz name pattern matching vs. diagnostic features
head(site(x)[, c('pedon_id', 'depth', 'densic.contact.top', 'densic.materials.top', 'densic.materials.bottom')], 10)

# rules for refinement of "soil depth"
x$depth_revised <- rep(NA, times=length(x))

# 1. "densic" diagnostic features not present or populated: use depth via hzname
idx <- which(is.na(x$densic.materials.top) & is.na(x$densic.contact.top))
x$depth_revised[idx] <- x$depth[idx]

# 2. densic contact present: use top depth of densic contact
idx <- which(!is.na(x$densic.contact.top))
x$depth_revised[idx] <- x$densic.contact.top[idx]

# 3. densic contact not present, but densic materials present: use bottom of densic materials
idx <- which(is.na(x$densic.contact.top) & !is.na(x$densic.materials.bottom))
x$depth_revised[idx] <- x$densic.materials.bottom[idx]

# what is left?
## 2017-03-30: no rows remaining
site(x)[is.na(x$depth_revised), c('pedon_id', 'depth', 'depth_revised', 'densic.contact.top', 'densic.materials.top', 'densic.materials.bottom')]

## Note: use depth_revised for subsequent calculations
## Note: previously estimated depth classes are no longer correct

# what fraction of pedons had the "wrong" soil depth?
# about 37% ! wow !
prop.table(table(x$depth_revised > x$depth))


# graphical check: 15 random pedons
# see ?plotSPC for more ideas
par(mar=c(1,1,3,1))
plot(sample(x, 15), label='pedon_id')
plot(sample(x, 15), label='pedon_id', color='effervescence')
plot(sample(x, 15), label='pedon_id', color='texture_class')


par(mar=c(1,1,2,1))
plot(subsetProfiles(x, s="taxonname == 'Houston Black'"), label='pedon_id')
title('Houston Black')
plot(subsetProfiles(x, s="taxonname == 'Heiden'"))
plot(subsetProfiles(x, s="taxonname == 'Ferris'"))
plot(subsetProfiles(x, s="taxonname == 'Vertel'"))


# # cross-tabulate depth classes by various factors:
# table(x$pmkind, x$depth.class)
# table(x$pmorigin, x$depth.class)
# table(x$hillslope_pos, x$depth.class)
# table(x$geompos_flats, x$depth.class)
# table(x$geompos_hill, x$depth.class)
# table(x$landform.string, x$depth.class)

# cross-tabulate other stuff
table(x$pmkind, x$taxonname)
table(x$pmkind, x$mollic.epipedon)
table(x$pmkind, x$calcic.horizon)
table(x$taxonname, x$densic.contact)
table(x$taxonname, x$densic.materials)

# revised depth
bwplot(pmkind ~ depth_revised, data=site(x))
bwplot(pmorigin ~ depth_revised, data=site(x))
bwplot(hillslope_pos ~ depth_revised, data=site(x))
bwplot(geompos_flats ~ depth_revised, data=site(x))
bwplot(geompos_hill ~ depth_revised, data=site(x))
bwplot(landform.string ~ depth_revised, data=site(x))
bwplot(taxonname ~ depth_revised, data=site(x), varwidth=TRUE)


# convert moist colors to CIE LAB
# https://en.wikipedia.org/wiki/Lab_color_space
# L: lightness
# A: redness
# B: yellowness
x.lab <- convertColor(horizons(x)[, c('m_r', 'm_g', 'm_b')], from='sRGB', to='Lab')

# copy over to SPC
x$L <- x.lab[, 1]
x$A <- x.lab[, 2]
x$B <- x.lab[, 3]

# slice-wise aggregation of quantities
a <- slab(x, taxonname ~ total_frags_pct + m_value + m_chroma + L + A + B)


xyplot(top ~ p.q50 | variable, groups=taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='25th-50th-75th Percentiles', par.settings=list(superpose.line=list(col=1:4, lwd=2)), strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(relation='free', alternating=1)), sync.colors=TRUE, alpha=0.25, auto.key = list(columns=4, lines=TRUE, points=FALSE))


xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='Moist Value', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'm_value', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='Moist Chroma', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'm_chroma', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='Coarse Fragments (%)', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'total_frags_pct', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='CIE LAB: L-coordinate', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'L', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='CIE LAB: A-coordinate', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'A', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

xyplot(top ~ p.q50 | taxonname, upper=a$p.q75, lower=a$p.q25, data=a, ylim=c(220,-5), ylab='Depth (cm)', xlab='CIE LAB: B-coordinate', strip=strip.custom(bg=grey(0.85)), as.table=TRUE, panel=panel.depth_function, prepanel=prepanel.depth_function, scales=list(y=list(tick.number=7, alternating=3), x=list(alternating=1)), subset=variable == 'B', cf=a$contributing_fraction, sync.colors=TRUE, alpha=0.33)

# slice-wise aggregation of categories: effervescence class
a <- slab(x, taxonname ~ effervescence, cpm=2)

# reshape into long format for plotting
a.long <- melt(a, id.vars=c('taxonname', 'top','bottom'), measure.vars=c('none', 'very.slight','slight','strong','violent'))
a.long$variable <- factor(a.long$variable, levels=c('none', 'very.slight', 'slight', 'strong', 'violent'))

# plot horizon type proportions using panels
xyplot(top ~ value | taxonname, groups=variable, data=a.long, subset=value > 0, ylim=c(250, -5), type=c('S','g'), horizontal=TRUE)

## plot slice-wise probabilities by taxonname
cols <- rev(rainbow(5))
xyplot(top ~ value | taxonname, groups=variable, data=a.long, type='l', ylim=c(250, -5), xlim=c(-0.1,1.2), auto.key=list(space='top', columns=5, points=FALSE, lines=TRUE, cex=0.75, title='Effervescence'), as.table=TRUE, par.settings=list(superpose.line=list(col=cols, lwd=2, lty=1), layout.heights=list(strip=1)), scales=list(cex=1, y=list(alternating=3, tick.number=10), x=list(alternating=1)), xlab=list('Probability'), ylab=list('Depth (cm)', cex=1.25), strip=strip.custom(par.strip.text=list(cex=1), bg=grey(0.85)), asp=1.5, panel=function(...) {
  panel.abline(h=seq(0, 240, by=20), v=seq(0, 1, by=0.2), col=grey(0.8), lty=3)
  panel.xyplot(...)
})


## plot slice-wise probabilities by effervescence class
cols <- 1:4
xyplot(top ~ value | variable, groups=taxonname, data=a.long, type='l', ylim=c(250, -5), xlim=c(-0.1,1.2), auto.key=list(space='top', columns=4, points=FALSE, lines=TRUE, cex=0.75), as.table=TRUE, par.settings=list(superpose.line=list(col=cols, lwd=2, lty=1), layout.heights=list(strip=1)), scales=list(cex=1, y=list(alternating=3, tick.number=10), x=list(alternating=1)), xlab=list('Probability'), ylab=list('Depth (cm)', cex=1.25), strip=strip.custom(par.strip.text=list(cex=1), bg=grey(0.85)), asp=1.5, panel=function(...) {
  panel.abline(h=seq(0, 240, by=20), v=seq(0, 1, by=0.2), col=grey(0.8), lty=3)
  panel.xyplot(...)
})


## plot slice-wise probabilities for "strong" effervescence class
cols <- 1:4
xyplot(top ~ value, groups=taxonname, data=a.long, type='l', main='Effervescence Class = "strong"', ylim=c(250, -5), xlim=c(-0.1,1.2), auto.key=list(space='top', columns=2, points=FALSE, lines=TRUE, cex=0.75), as.table=TRUE, par.settings=list(superpose.line=list(col=cols, lwd=2, lty=1), layout.heights=list(strip=1)), scales=list(cex=1, y=list(alternating=3, tick.number=10), x=list(alternating=1)), xlab=list('Probability'), ylab=list('Depth (cm)', cex=1.25), strip=strip.custom(par.strip.text=list(cex=1), bg=grey(0.85)), asp=1.25, subset=variable == 'strong', panel=function(...) {
  panel.abline(h=seq(0, 240, by=20), v=seq(0, 1, by=0.2), col=grey(0.8), lty=3)
  panel.xyplot(...)
})


# set factor levels for eff. class
x$effervescence <- factor(x$effervescence, levels=c('none', 'very slight', 'slight', 'strong', 'violent'), ordered=TRUE)

# subset Houston Black soils
hb <- subsetProfiles(x, s="taxonname == 'Houston Black'")

# compute distance matrix for Houston Black soils using L,A,B coords
hb.dist <- profile_compare(subsetProfiles(x, s="taxonname == 'Houston Black'"), vars=c('L', 'A', 'B', 'effervescence'), max_d=200, k=0, strict_hz_eval=FALSE)

# 
# hist(dd)
# m <- as.matrix(dd)
# idx.mat <- m > quantile(m, probs = c(0.9), na.rm=TRUE)
# high.D <- apply(idx.mat, 1, which)
# high.D.peiid <- unique(unlist(sapply(high.D, names)))
# 
# hb.odd <- hb[which(profile_id(hb) %in% high.D.peiid), ]
# plot(hb.odd, color='effervescence')
# 
# library(ape)
# library(cluster)
# 
# p <- as.phylo(hclust(dd))
# plot(p)
