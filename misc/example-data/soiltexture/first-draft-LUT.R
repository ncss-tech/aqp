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

