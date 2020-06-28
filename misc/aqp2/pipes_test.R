library(aqp)
library(soilDB)
library(magrittr)
library(data.table)

data("loafercreek")

# try with various df classes (data.frame, data.table, tbl_df)
metadata(loafercreek)$aqp_df_class <- "data.table"

# filter to get profiles with taxorder populated
# glom the PSCS from each profile
# calculate PSCS weighted average clay content
# group resulting SPC by taxorder
res <- loafercreek %>%
    filter(!is.na(taxorder)) %>%
    glomApply(estimatePSCS, truncate = TRUE) %>%
    mutate_profile(pscs_clay = weighted.mean(clay, hzdepb - hzdept)) %>%
    group_by(taxorder)

# calculate taxorder-level summaries of PSCS clay content
res %>% summarize(clay_mean = mean(pscs_clay, na.rm = TRUE),
                  n_obs = sum(!is.na(pscs_clay)))

####
#### Trying to break stuff...
####
# compose a deliberately reordered SPC list with subsets of profiles
#  this is ordered with respect to the phiid ID, not calculated
#  note: look at character ordering of idname versus the character
#        ordering of hzidname for profiles in index 1 and 2
#        i.e. sort(profile_id(union(l)))
l <- list(glom(loafercreek[1,], 25, 100) %>%
            group_by(taxorder),
          glom(loafercreek[2,], 25, 100) %>%
            group_by(taxorder))

# check metadata of first element
metadata(l[[1]])

# check metadata of union result
metadata(union(l))

# ensure glom is working correctly
p <- union(l[1])
glom(p, 25, 100, ids = TRUE)

# spc subset preserving hzid
p <- union(l)
pg <- glom(p[1,], 25, 100)
hzID(pg)

# verify order in horizon slot
horizons(p)

# optional slots preserved across glom+union
hztexclname(pg)
