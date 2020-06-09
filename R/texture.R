# # this creates a lookup from ssc to texcl using the soiltexture package function TT.points.in.classes, which is not really inituitive
# soiltexture <- expand.grid(clay = 0:100, sand = 0:100, silt = 0:100)
# soiltexture <- subset(soiltexture, (clay + silt + sand) == 100)
# soiltexture$texcl <- apply(soiltexture, 1, FUN = function(x) {
# 
#   y <- soiltexture::TT.points.in.classes(data.frame(CLAY = x[1], SILT = x[3], SAND = x[2]), class.sys = "USDA-NCSS.TT")
#   texcl <- names(y[, y > 0])
# 
#   return(texcl[1])
# })
# 
# soiltexture$texcl <- tolower(soiltexture$texcl)
# 
# idx <- with(soiltexture, clay == 40 & sand == 45)
# soiltexture$texcl[idx] <- "sc"
# row.names(soiltexture) <- NULL
# 
# soiltexture <- list(values = soiltexture)
# 
# 
# library(compositions)
# 
# st <- soiltexture
# split(st, st$texcl) ->.;
# lapply(., function(x) {
# 
#   co <- clo(x, parts = c("sand", "silt", "clay"), total = 100)
#   rco <- rplus(co, total = 100)
#   ds  <- mean(rco)
#   df <- data.frame(
#     texcl    = x$texcl[1],
#     avg_clay = round(mean(x$clay)),
#     ravg_clay = round(ds[3]),
#     avg_sand = round(mean(x$sand))
#     ravg_sand = round(ds[1])
#   )
#   df$silt <- 100 - df$clay - df$sand
# }) ->.;
# do.call("rbind", .) ->.;
# st2 <- .
# # I see no difference in the compositional statistics
# 
#
# # logic from the particle size estimator NASIS calculation
# tex <- expand.grid(sand = 0:100, silt = 0:100)
# tex <- subset(tex, (sand + silt) < 101)
# row.names(tex) <- NULL
# tex$clay <- 100 - tex$sand - tex$silt
# tex$texcl <- ssc_to_texcl(tex, as.is = TRUE)
# 
# soiltexture = list(values = tex)
# 
# st <- aggregate(cbind(clay, sand) ~ texcl, data = soiltexture$values, function(x) round(mean(x)))
# st[st$texcl == "c", "clay"] <- 55
# st <- rbind(st,
#             c(texcl = "cos",  st[st$texcl == "s",  -1]),
#             c(texcl = "fs",   st[st$texcl == "s",  -1]),
#             c(texcl = "vfs",  st[st$texcl == "s",  -1]),
#             c(texcl = "lcos", st[st$texcl == "ls", -1]),
#             c(texcl = "lfs",  st[st$texcl == "ls", -1]),
#             c(texcl = "lvfs", st[st$texcl == "ls", -1]),
#             c(texcl = "cosl", st[st$texcl == "sl", -1]),
#             c(texcl = "fsl",  st[st$texcl == "sl", -1]),
#             c(texcl = "vfsl", st[st$texcl == "sl", -1])
#             )
# st$silt <- 100 - st$clay - st$sand
# 
# soiltexture$averages <- st
# 
# save(soiltexture, file = "C:/workspace2/github/ncss-tech/aqp/data/soiltexture.rda")




# convert sand, silt and clay to texture class
ssc_to_texcl <- function(df, rmHzErrors = TRUE, as.is = FALSE, droplevels = TRUE) {
  
  if (all(! grepl("clay|CLAY", names(df))) & all(! grepl("sand|SAND", names(df)))) {
    stop("missing columns with clay|CLAY|sand|SAND in the heading", call. = FALSE)
  }
  
  # standardize inputs
  names(df) <- tolower(names(df))
  
  df$clay <- df[, grep("clay", names(df))]
  df$sand <- df[, grepl("sand", names(df))]
  
  
  # check if silt is missing
  if (all(! grepl("silt", names(df)))) {
    message("missing columns with silt|SILT in the heading, therefore silt will be calculated as 100 - clay - sand")
    df$silt <- 100 - df$clay - df$sand
  }
  
  
  # round and index
  df <- df[c("clay", "silt", "sand")]
  df <- round(df)
   
  
  # check sand, silt and clay sum to 100
  idx <- (df$sand + df$silt + df$clay) > 100 | (df$sand + df$silt + df$clay) < 100
  if (any(idx)) {
    message("some records do not sum to 100 %")
    if (rmHzErrors == TRUE) {
      df$silt <- 100 - df$clay - df$sand
    } else message("if records do not sum to 100% the returned values will be NA")
  }
  
  
  # logic from the particle size estimator calculation from NASIS
  df <- within(df, {
    texcl = NA
    texcl[silt >= 79.99 & clay <  11.99] = "si"
    texcl[silt >= 49.99 & clay <  26.99 & (silt < 79.99 | clay >= 11.99)] = "sil"
    texcl[clay >= 26.99 & clay <  39.99 & sand <= 20.01] = "sicl"
    texcl[clay >= 39.99 & silt >= 39.99] = "sic"
    texcl[clay >= 39.99 & sand <= 45.01 & silt <  39.99] = "c"
    texcl[clay >= 26.99 & clay <  39.99 & sand >  20.01 & sand <= 45.01] = "cl"
    texcl[clay >=  6.99 & clay <  26.99 & silt >= 27.99 & silt < 49.99 & sand <= 52.01] = "l"
    texcl[clay >= 19.99 & clay <  34.99 & silt <  27.99 & sand > 45.01] = "scl"
    texcl[clay >= 34.99 & sand >  45.01] = "sc"
    texcl[sand > 89.9] = "s"
    texcl[(silt + 1.5 * clay) >= 14.99 & (silt + 2 * clay) < 29.99] = "ls"
    texcl[!is.na(sand) & !is.na(clay) & is.na(texcl)] = "sl"
  })
  
  
  if (as.is == FALSE) {
    df$texcl <- factor(df$texcl, levels = c("s",  "si", "ls", "sl", "sil", "l", "scl",  "cl", "sicl", "sc", "sic", "c"), ordered = TRUE)
  }
  
  if (droplevels == TRUE & as.is == FALSE) {
    df$texcl <- droplevels(df$texcl)
  }
  
  
  return(df$texcl)
}
  


# impute sand, silt, and clay with texcl averages
texcl_to_ssc <- function(texcl, clay = NULL) {
  
  if (any(is.na(texcl))) {
    message("some texcl records missing")
  }
  
  # clay is not NULL
  clay_not_null <- all(!is.null(clay))
  
  # standardize the inputs
  df <- data.frame(texcl = tolower(as.character(texcl)), 
                   stringsAsFactors = FALSE
                   )
  if (clay_not_null) {
    df$clay <- as.integer(round(clay))
  }
  df$rn <- row.names(df)
  
  
  if (TRUE) {
    load(system.file("data/soiltexture.rda", package="aqp")[1])
  }
  
  
  # check for texcl that don't match
  idx <- ! df$texcl %in% unique(soiltexture$averages$texcl)
  if (any(idx)) {
    message("not all the texcl supplied match the lookup table, removing nomatches")
    df$texcl <- ifelse(idx, NA, df$texcl)
  }
  
  # check clay ranges 0-100
  if (clay_not_null & any(clay < 0, na.rm = TRUE) & any(clay > 100, na.rm = TRUE)) {
    message("some clay records < 0 or > 100%")
    }
  
  # if clay is present
  if (clay_not_null) {
    df <- within(df, {
      texcl = ifelse(texcl %in% c("cos",  "fs", "vfs"),   "s",  texcl)
      texcl = ifelse(texcl %in% c("lcos", "lfs", "lvfs"), "ls", texcl)
      texcl = ifelse(texcl %in% c("cosl", "fsl", "vfsl"), "sl", texcl)
    })
    
    st <- aggregate(sand ~ texcl + clay, data = soiltexture$values, function(x) as.integer(round(mean(x))))
    st$silt <- 100 - st$clay - st$sand
    
    df <- merge(df[c("texcl", "clay", "rn")], st, by = c("texcl", "clay"), all.x = TRUE, sort = FALSE)
  } else {
    df <- merge(df[c("texcl", "rn")], soiltexture$averages, by = "texcl", all.x = TRUE, sort = FALSE)
  }
  
  vars <- c("sand", "silt", "clay")
  df <- df[(order(as.integer(df$rn))), vars]
  df$rn    <- NULL
  df$texcl <- NULL
  
  return(df)
}
  


# # convert sand, silt and clay to the family particle size class
# psc_to_taxpartsize <- function(df) {
# 
#   df$frags[is.na(df$frags)] <- 0
#   df$fpsc <- NA
#   
#   sandytextures <- c("cos", "s", "fs", "lcos", "ls", "lfs")
# 
#   idx <- df$frags >= 35
#   if (any(idx)) {
#     df[idx,] <- within(df[idx,], {
#       fpsc = NA
#       fpsc[texcl %in% sandytextures
#            ] = "sandy-skeletal"
#       fpsc[clay < 35] = "loamy-skeletal"
#       fpsc[clay >= 35] = "clayey-skeletal"
#       })
#   }
# 
#   idx <- df$frags < 35 & df$texcl %in% sandytextures
#   if (any(idx)) {
#     df[idx, ]$fpsc <- "sandy"
#   }
# 
#   idx <- df$frags < 35 & ! df$texcl %in% sandytextures
#   if (any(idx)) {
#     df[idx, ] <- within(df[idx,], {
#       fpsc[clay < 18 & sand >= 15] = "coarse-loamy"
#       fpsc[clay < 18 & sand <  15] = "coarse-silty"
#       fpsc[clay >= 18 & clay < 35] = "fine-loamy"
#       fpsc[clay >= 18 & clay < 35 & sand < 15] = "fine-silty"
#       fpsc[clay >= 35 & clay < 60] = "fine"
#       fpsc[clay > 60] = "very-fine"
#       })
#   }
# 
#   return(df$fpsc)
# }
# 
