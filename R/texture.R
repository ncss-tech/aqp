# # this creates a map from ssc to texcl using the soiltexture package function TT.points.in.classes, which is not really inituitive
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
# save(soiltexture, file = "C:/workspace2/github/ncss-tech/aqp/data/soiltexture.rda")


# convert sand, silt and clay to texture class
ssc_to_texcl <- function(df) {
  
  load(system.file("data/soiltexture.rda", package="aqp")[1])
  
  if (all(! grepl("clay|CLAY", names(df))) & all(! grepl("sand|SAND", names(df)))) {
    stop("missing columns with clay|CLAY|sand|SAND in the heading", call. = FALSE)
  }
  
  df$rn <- row.names(df)
  names(df) <- tolower(names(df))
  df$clay <- df[, grep("clay", names(df))]
  df$sand <- df[, grepl("sand", names(df))]
  
  if (all(! grepl("silt", names(df)))) {
    message("missing columns with silt|SILT in the heading, therefore silt will be calculated as 100 - clay - sand")
    df$silt <- 100 - df$clay - df$sand
  }
  
  df <- df[c("rn", "clay", "silt", "sand")]
  df[2:4] <- round(df[2:4])
  df$idx <- with(df, paste(clay, silt, sand))
  
  soiltexture$idx <- with(soiltexture, paste(clay, silt, sand))
  
  df <- merge(df, soiltexture[c("idx", "texcl")], by = "idx", all.x = TRUE, sort = FALSE)
  df <- df[order(as.integer(df$rn)), ]
  df$rn  <- NULL
  df$idx <- NULL
  
  
  return(df$texcl)
}
  

# # convert sand, silt and clay to the family particle size class
# ssc_to_fpsc <- function(df) {
#   
#   df$frags[is.na(df$frags)] <- 0
#   df$pscs <- NA
#   
#   idx <- df$frags >= 35
#   if (any(idx)) {
#     df[idx,] <- within(df[idx,], {
#       pscs = NA
#       pscs[texcl %in% c("cos", "s", "fs", "lcos", "ls", "lfs")
#            ] = "sandy-skeletal"
#       pscs[clay < 35] = "loamy-skeletal"
#       pscs[clay >= 35] = "clayey-skeletal"
#       })
#   }
#   
#   idx <- df$frags < 35 & df$texcl %in% c("cos", "s", "fs", "lcos", "ls", "lfs")
#   if (any(idx)) {
#     df[idx, ]$pscs <- "sandy"
#   }
#   
#   idx <- df$frags <- 35 & (df$texcl %in% "lvfs" | ! df$texcl %in% c("cos", "s", "fs", "lcos", "ls", "lfs"))
#   if (any(idx)) {
#     df[idx, ] <- within(df[idx,], {
#       pscs[clay < 18 & sand >= 15] = "coarse-loamy"
#       pscs[clay < 18 & sand <  15] = "coarse-silty"
#       pscs[clay >= 18 & clay < 35] = "fine-loamy"
#       pscs[clay >= 18 & clay < 35 & sand < 15] = "fine-silty"
#       pscs[clay >= 35 & clay < 60] = "fine"
#       pscs[clay > 60] = "very-fine"
#       })
#   }
#   
#   return(df)
# }

