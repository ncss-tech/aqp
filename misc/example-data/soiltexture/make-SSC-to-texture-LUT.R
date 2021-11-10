## Stephen Roecker
## re-make the `soiltexture` example data with:
## * {sand, silt, clay} -> USDA soil texture conversionLUT
##  * class means
## 
##

## requires latest version of aqp, with ssc_to_texcl()
library(aqp)

## empty container for LUTs
soiltexture = list()

## generate SSC -> texture class LUT
values <- expand.grid(sand = 0:100, silt = 0:100)
values <- subset(values, (sand + silt) < 101)
row.names(values) <- NULL
values$clay <- 100 - values$sand - values$silt
values$texcl <- ssc_to_texcl(sand = values$sand, clay = values$clay, as.is = TRUE)

## save to list element
soiltexture$values <- values


## generate class means
avgs <- aggregate(cbind(clay, sand) ~ texcl, data = values, function(x) round(mean(x)))

avgs[avgs$texcl == "c", "clay"] <- 55

avgs <- rbind(avgs,
              c(texcl = "cos",  avgs[avgs$texcl == "s",  -1]),
              c(texcl = "fs",   avgs[avgs$texcl == "s",  -1]),
              c(texcl = "vfs",  avgs[avgs$texcl == "s",  -1]),
              c(texcl = "lcos", avgs[avgs$texcl == "ls", -1]),
              c(texcl = "lfs",  avgs[avgs$texcl == "ls", -1]),
              c(texcl = "lvfs", avgs[avgs$texcl == "ls", -1]),
              c(texcl = "cosl", avgs[avgs$texcl == "sl", -1]),
              c(texcl = "fsl",  avgs[avgs$texcl == "sl", -1]),
              c(texcl = "vfsl", avgs[avgs$texcl == "sl", -1])
)

avgs$silt <- 100 - avgs$clay - avgs$sand


## save to list element
soiltexture$averages <- avgs

##
texmod <- data.frame(
  texmod     = c("gr", "grf", "grm", "grc", "grv", "grx",
                 "cb", "cbv", "cbx",
                 "st", "stv", "stx",
                 "by", "byv", "byx",
                 "cn", "cnv", "cnx",
                 "fl", "flv", "flx",
                 "pgr", "pgrv", "pgrx",
                 "pcb", "pcbv", "pcbx",
                 "pst", "pstv", "pstx",
                 "pby", "pbyv", "pbyx",
                 "pcn", "pcnv", "pcnx",
                 "pfl", "pflv", "pflx"
  ),
  fragvoltot_l  = c(15, 15, 15, 15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60,
                    15, 35, 60
  ),
  fragvoltot_r  = c(25, 25, 25, 25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75,
                    25, 48, 75
  ),
  fragvoltot_h = c(34, 34, 34, 34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89,
                   34, 59, 89
  ),
  stringsAsFactors = FALSE
)

## 
texmod <- within(texmod, {
  fragvoltot_l_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_l)
  fragvoltot_r_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_r)
  fragvoltot_h_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_h)
})

## save to list element
soiltexture$texmod <- texmod


## append labels from soilDB metadata
sub <- metadata[metadata$ColumnPhysicalName == "texcl", ]
temp <- data.frame(texcl = sub$ChoiceName, texcl_label = tolower(sub$ChoiceLabel))
soiltexture$averages <- merge(temp, soiltexture$averages, by = "texcl", sort = FALSE)

sub <- metadata[metadata$ColumnPhysicalName == "texmod", ]
temp <- data.frame(texmod = sub$ChoiceName, texmod_label = tolower(sub$ChoiceLabel))
soiltexture$texmod <- merge(temp, soiltexture$texmod, by = "texmod", sort = TRUE)


## save
save(soiltexture, file = "../../../data/soiltexture.rda")
