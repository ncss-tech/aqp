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
#
# soiltexture$texmod <- data.frame(
#   texmod     = c("gr", "grf", "grm", "grc", "grv", "grx",
#                  "cb", "cbv", "cbx",
#                  "st", "stv", "stx",
#                  "by", "byv", "byx",
#                  "cn", "cnv", "cnx",
#                  "fl", "flv", "flx",
#                  "pgr", "pgrv", "pgrx",
#                  "pcb", "pcbv", "pcbx",
#                  "pst", "pstv", "pstx",
#                  "pby", "pbyv", "pbyx",
#                  "pcn", "pcnv", "pcnx",
#                  "pfl", "pflv", "pflx"
#                  ),
#   fragvoltot_l  = c(15, 15, 15, 15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60,
#                  15, 35, 60
#                  ),
#   fragvoltot_r  = c(25, 25, 25, 25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75,
#                  25, 48, 75
#   ),
#   fragvoltot_h = c(34, 34, 34, 34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89,
#                  34, 59, 89
#   ),
#   stringsAsFactors = FALSE
# )
# soiltexture$texmod <- within(soiltexture$texmod, {
#   fragvoltot_l_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_l)
#   fragvoltot_r_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_r)
#   fragvoltot_h_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_h)
#   })
#
# 
# save(soiltexture, file = "C:/workspace2/github/ncss-tech/aqp/data/soiltexture.rda")




# convert sand, silt and clay to texture class
ssc_to_texcl <- function(sand = NULL, clay = NULL, as.is = FALSE, droplevels = TRUE) {
  
  # check lengths
  idx <- all(length(clay) != length(sand))
  if (idx) {
    stop("length of inputs do not match")
  }
  
  
  # standardize inputs
  df <- data.frame(sand = as.integer(round(sand)), 
                   clay = as.integer(round(clay)), 
                   stringsAsFactors = FALSE
                   )
  df$silt <- 100 - df$clay - df$sand
  
  
  # check sand, silt and clay sum to 100
  idx <- (df$sand + df$silt + df$clay) > 100 | (df$sand + df$silt + df$clay) < 100
  if (any(idx)) {
    warning("some records sand, silt, and clay do not sum to 100 %")
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
    texcl[(silt + 1.5 * clay) < 15] = "s"
    texcl[(silt + 1.5 * clay) >= 15 & (silt + 2 * clay) < 29.99] = "ls"
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
texcl_to_ssc <- function(texcl = NULL, clay = NULL) {
  
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
    warning("not all the texcl supplied match the lookup table, removing nomatches")
    df$texcl <- ifelse(idx, NA, df$texcl)
  }
  
  # check clay ranges 0-100
  if (clay_not_null & any(clay < 0, na.rm = TRUE) & any(clay > 100, na.rm = TRUE)) {
    warning("some clay records < 0 or > 100%")
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
  


# modifer to fragvoltot
texmod_to_fragvoltot <- function(texmod = NULL, lieutex = NULL) {
  
  # check
  if (any(!is.na(texmod) & !is.na(lieutex))) {
    warning("texmod and lieutex should not both be present, they are mutually exclusive, only the texmod will be returned")
  }
  
  
  # standardize inputs
  df <- data.frame(texmod = tolower(texmod),
                   stringsAsFactors = FALSE
                   )
  df$rn = row.names(df)
  
  
  # load lookup table
  load(system.file("data/soiltexture.rda", package="aqp")[1])
  
  
  # check for texmod and lieutex that don't match
  idx <- any(! df$texmod %in% soiltexture$texmod$texmod)
  if (idx) {
    message("not all the texmod supplied match the lookup table, removing nomatches")
    df$texmod <- ifelse(idx, NA, df$texmod)
  }
  
  idx <- all(!is.null(lieutex)) & any(! toupper(lieutex) %in% c("GR", "CB", "ST", "BY", "CN", "FL", "PG", "PCB", "PST", "PBY", "PCN", "PFL", "BR", "HMM", "MPM", "SPM", "MUCK", "PEAT", "ART", "CGM", "FGM", "ICE", "MAT", "W"))
  if (idx) {
    message("not all the lieutex supplied match the lookup table")
    df$texmod <- ifelse(idx, NA, df$texmod)
  }
  

  # merge
  df <- merge(df, soiltexture$texmod, by = "texmod", all.x = TRUE, sort = FALSE)
  df <- df[(order(as.integer(df$rn))), ]
  df$rn     <- NULL
  
  
  # lieutex
  if (all(!is.null(lieutex))) {
    
    idx1 <- is.na(texmod) & !is.na(lieutex) & grepl("GR|CB|ST|BY|CN|FL", lieutex)
    idx2 <- is.na(texmod) & !is.na(lieutex) & grepl("PG|PCB|PST|PBY|PCN|PFL", lieutex)
    
    df$lieutex <- toupper(lieutex)
    
    df <- within(df, {
      fragvoltot_l = ifelse(idx1, 90, fragvoltot_l)
      fragvoltot_r = ifelse(idx1, 95, fragvoltot_l)
      fragvoltot_h = ifelse(idx1, 100, fragvoltot_l)
      
      fragvoltot_l_nopf = ifelse(idx2, 0,  fragvoltot_l)
      fragvoltot_r_nopf = ifelse(idx2, 0,  fragvoltot_l)
      fragvoltot_h_nopf = ifelse(idx2, 0, fragvoltot_l)
      })
    df$lieutex <- lieutex
  }
  
  
  
  return(df)
}



# convert sand, silt and clay to the family particle size class
texture_to_taxpartsize <- function(texcl = NULL, clay = NULL, sand = NULL, fragvoltot = NULL) {
  
  # check lengths
  idx <- all(length(texcl) == length(clay) & length(clay) == length(sand) & length(sand) == length(fragvoltot))
  if (!idx) {
    stop("length of inputs do not match")
  }
    
    
  # standarize inputs
  df <- data.frame(texcl      = tolower(texcl), 
                   clay       = as.integer(round(clay)), 
                   sand       = as.integer(round(sand)), 
                   fragvoltot = as.integer(round(fragvoltot)),
                   fpsc       = as.character(NA),
                   stringsAsFactors = FALSE
                   )
  df$silt <- 100 - df$sand - df$clay
  
  sandytextures <- c("cos", "s", "fs", "lcos", "ls", "lfs")
  
  
  # check texcl lookup
  var <- c("c", "cl", "l", "ls", "s", "sc", "scl", "si", "sic", "sicl", "sil", "sl", "cos", "fs", "vfs", "lcos", "lfs", "lvfs", "cosl", "fsl", "vfsl")
  idx <- ! df$texcl %in% var
  if (any(idx)) {
    warning("not all the texcl supplied match the lookup table")
  }
  
  
  # check percentages
  idx <- df$silt > 100 | df$silt < 0 | df$clay > 100 | df$clay < 0 | df$sand > 100 | df$sand < 0 | df$fragvoltot > 100 | df$fragvoltot < 0
  if (any(idx)) {
    warning("some records are > 100 % or < 0%, or the calcuated silt fraction is > 100 or < 0")
  }
  
  
  # check ssc_to_texcl() vs texcl
  df$texcl_calc <- suppressMessages(ssc_to_texcl(sand = df$sand, clay = df$clay, as.is = TRUE))
  
  df <- within(df, {
    texcl_calc = ifelse(texcl_calc == "s"  & grepl("^cos$|^fs$|^vfs$",    texcl), texcl, texcl_calc)
    texcl_calc = ifelse(texcl_calc == "ls" & grepl("^lcos$|^lfs$|^lvfs$", texcl), texcl, texcl_calc)
    texcl_calc = ifelse(texcl_calc == "sl" & grepl("^cosl$|^fsl$|^vfsl$", texcl), texcl, texcl_calc)
  })
  
  idx <- any(df$texcl != df$texcl_calc)
  if (idx) {
    warning("some of the texcl records don't match the calculated texcl via ssc_to_texcl()")
  }
  
  
  # calculate family particle size control section
  df$fpsc <- ifelse(df$fragvoltot > 90, "fragmental", df$fpsc)
  
  idx <- df$fragvoltot >= 35 & df$fragvoltot <= 90
  if (any(idx)) {
    df[idx,] <- within(df[idx,], {
      fpsc[texcl %in% sandytextures
           ] = "sandy-skeletal"
      fpsc[clay < 35] = "loamy-skeletal"
      fpsc[clay >= 35] = "clayey-skeletal"
      })
  }

  idx <- df$fragvoltot < 35 & df$texcl %in% sandytextures & df$fragvoltot <= 90
  if (any(idx)) {
    df[idx, ]$fpsc <- "sandy"
  }

  idx <- df$fragvoltot < 35 & ! df$texcl %in% sandytextures & df$fragvoltot <= 90
  if (any(idx)) {
    df[idx, ] <- within(df[idx,], {
      fpsc[clay < 18 & sand >= 15] = "coarse-loamy"
      fpsc[clay < 18 & sand <  15] = "coarse-silty"
      fpsc[clay >= 18 & clay < 35] = "fine-loamy"
      fpsc[clay >= 18 & clay < 35 & sand < 15] = "fine-silty"
      fpsc[clay >= 35 & clay < 60] = "fine"
      fpsc[clay > 60] = "very-fine"
      })
  }

  return(df$fpsc)
}

