
library(aqp)
library(soilDB)

load("C:/Users/stephen.roecker/Box/properties_team/nasis_pedons_20210325.RData")


# check for missing fragvoltot ----
texmod <- texture_to_texmod(spc$texture)
spc$texmod <- texmod
texmod_rf <- texmod_to_fragvoltot(texmod)
texmod_rf[3:8] <- lapply(texmod_rf[3:8], function(x) ifelse(is.na(x), 0, x))

h <- horizons(spc)
s <- site(spc)


q  <- paste0("areasymbol LIKE '", c("WI%"), "'")
ch <- do.call("rbind", lapply(q, get_chorizon_from_SDA, childs = TRUE))
co <- do.call("rbind", lapply(q, get_component_from_SDA, childs = FALSE))
mu <- do.call("rbind", lapply(q, get_mapunit_from_SDA))


texmod2 <- fragvol_to_texmod(ch, as.is = FALSE)
texmod <- texture_to_texmod(ch$texture)
texmod <- factor(texmod, levels = levels(texmod2$texmod))

tb <- table(pred = texmod2$texmod, obs = texmod)
cm <- caret::confusionMatrix(tb)

tb
cm$overall


vars <- c("gravel", "cobbles", "stones", "boulders", "channers", "flagstones", "paragravel", "paracobbles", "parastones", "paraboulders", "parachanners", "paraflagstones")
idx <- as.character(texmod) != as.character(texmod2$texmod) & complete.cases(data.frame(texmod, texmod2$texmod)) & !duplicated(tidyr::unite(ch[vars], "test")[1])
df <- cbind(ch[c("cokey", "chkey", "hzname", "hzdept_r", vars)], texmod, texmod2$texmod)
df$sum <- rowSums(df[vars[1:6]])
df <- merge(co[c("cokey", "nationalmusym")], df, all.y = TRUE, by = "cokey")
View(df[idx, ])

writeClipboard(paste0(unique(df$nationalmusym), collapse = "', '"))
  
  

# check totals ----
n_h          <- nrow(h)
n_frags      <- sum(h$fragvoltot > 0)
n_frags_15   <- sum(h$fragvoltot >= 15)
n_frags_calc <- sum(h$total_frags_pct_nopf > 0.5)
n_frags_c15  <- sum(h$total_frags_pct_nopf >= 15)
n_texmod     <- sum(!is.na(h$texmod))


n_frags / n_h
n_frags_calc / n_h
n_frags_c15 / n_h
n_texmod / n_h
# half of the horizons with >15% frags don't have texmod populated
sum(as.integer(h$fragvoltot) != as.integer(h$total_frags_pct))

## check accuracy
caret::RMSE(pred = h$total_frags_pct, obs = h$fragvoltot, na.rm = TRUE)

round(prop.table(table(texmod = !is.na(h$texmod), frags_15  = h$fragvoltot >= 15))      * 100)
round(prop.table(table(texmod = !is.na(h$texmod), frags_c15 = h$total_frags_pct >= 15)) * 100)


# replace missing fragvoltot ----
h$fragvoltot2 <- ifelse(h$fragvoltot < 0.5 | is.na(h$fragvoltot), h$total_frags_pct, h$fragvoltot)

## check accuracy
caret::RMSE(pred = h$total_frags_pct, obs = h$fragvoltot2, na.rm = TRUE)

round(prop.table(table(texmod = !is.na(h$texmod), frags_15  = h$fragvoltot2 >= 15)) * 100)
round(prop.table(table(texmod = !is.na(h$texmod), frags_c15 = h$total_frags_pct >= 15)) * 100)


# check texmod matches ----
idx_match    <- (
  h$fragvoltot2 >= texmod_rf$fragvoltot_l & 
  h$fragvoltot2 <= texmod_rf$fragvoltot_h
  ) |
  (h$fragvoltot2 < 15 & is.na(h$texmod))


sum(!idx_match,    na.rm = TRUE)
nomatch_peiid <- unique(h$peiid[!idx_match])
length(nomatch_peiid)
write.csv(cbind(nomatch_peiid), "nomatch_texmod_peiid.csv")


round(prop.table(table(
  match_texmod = idx_match,    
  frags_15     = h$fragvoltot2 >= 15
  )) * 100
  )


# spot check values
vars <- c("peiid", "hzname", "texcl", "texture", "texmod", "fragvoltot")
View(h[!idx_match, vars])


# plot years that don't match
s$posixlt <- as.POSIXlt(s$obsdate)
s$year <- s$posixlt$year + 1900
hist(s$year[s$year < 2021 & s$peiid %in% nomatch_peiid])

# don't impute, remove




