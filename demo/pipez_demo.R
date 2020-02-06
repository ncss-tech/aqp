#
# %>% %>% %>% aqp is "smokin'" %>% %>% %>% 
#
# AUTHOR: andrew g. brown
# EMAIL:  andrew.g.brown@usda.gov
# GITHUB:  https://github.com/brownag
#
# DATE: 2020/02/05
#
### A thought experiment on magrittr "pipelines"
#   for SoilProfileCollections. A first foray,
#   using three subsetting "verbs":
#
#   * regular expression - grepSPC()
#   * logical comparison - subSPC()
#   * functional evaluation - subApply()
#
### SETUP
#
# install aqp "pipez" development branch off github:
#   remotes::install_github("ncss-tech/aqp@pipez", dependencies=F, build=F)
#
# you will also need: magrittr, rlang and stringr -- all in t*dyverse...
########################################################################

# load required packages
library(aqp)
library(soilDB)

# pipe operator
# titlecase on taxonname
# tidy evaluation
library(tidyverse, quietly = TRUE)

# load sierra nevada foothills soils
data("loafercreek")
data("gopheridge")

# create loafergopher dataset with aqp::union
loafergopher <- aqp::union(list(loafercreek, gopheridge))

# create taxon categorical variable for plotting
loafergopher$taxon <- factor(str_to_title(loafergopher$taxonname))

# define a detailed filtering function
# returns true if calculated PSCS clay+frags are >35%
is_clayey_sk <- function(p) {
  
  # estimate particle size control section
  pscs <- estimatePSCS(p)
  
  # glom horizons intersecting PSCS
  hz <- horizons(glom(p, z1 = pscs[1], z2 = pscs[2]))
  
  # truncate ragged edges to PSCS
  hz[hz$hzdept < pscs[1], 'hzdept'] <- pscs[1]
  hz[hz$hzdepb > pscs[2], 'hzdepb'] <- pscs[2] 
  
  # calculate weights
  hz$w <- hz$hzdepb - hz$hzdept
  
  # calculate weighted averages and test
  clay <- weighted.mean(hz$clay, w = hz$w) > 35
  frag <- weighted.mean(hz$clay, w = hz$w) > 35
  
  return(clay & frag)
}

# lets get loafercreek and gopheridge taxadjuncts
#   but constrain it to 'alfisols' soil order
#   estimatePSCS and filter out non cl-sk pedons
f <- loafergopher %>%
      grepSPC(taxonname, "^Loaf|^Goph") %>%
      subSPC(taxonkind == 'taxadjunct', 
             taxorder == "alfisols") %>%
      subApply(is_clayey_sk)

## the new functions also work the non-smokin' way! boo
# grepSPC(loafergopher, attr = taxonname, pattern = "Loaf")

# visual inspection of clayey-skeletal taxadjuncts
par(mar=c(5,5,0,0))
groupedProfilePlot(f, groups="taxon", width=0.1, cex.names=0.4,
                   id.style="side", label="pedon_id")
#addVolumeFraction(f, "total_frags_pct")

# demonstrate horizon level subsetting -- 
#  one or more horizons clay >35% and one or more frags >35%
g <- loafergopher %>%
  subSPC(clay >= 35 & 
           total_frags_pct >= 35) 

# subSPC can evaluate arbitrary # of logical expressions 
# separated by comma, or you can do the traditional logical
# operators to combine multiple attributes as above with &

groupedProfilePlot(g, groups='taxon', 
                   color='clay')
addVolumeFraction(g, "total_frags_pct")

