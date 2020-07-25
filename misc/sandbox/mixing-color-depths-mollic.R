library(aqp)
library(soilDB)

## aqp_mixed_colors
#
#  The goal here is to compare dry and moist colors when numerically "mixed" over certain depth intervals 
#  `aqp_mixed_colors` is vectorized over profiles -- performs color "mixing" by profile on interval [ztop, zbot] 
#  
#  And to compare "mixing" methods -- using a simple depth-weighted average / dominant hue approach
#                                   and the powerful, but computationally costly, method in aqp::aggregateColor
## Methods
##   1. depth-weighted average value and chroma + dominant hue in interval ("simple" aggregation)
##   2. conversion of Munsell -> RGB -> CIELAB2000 mix -> RGB -> Munsell (via aqp::aggregateColor)
# 
# Returning results in a three part list reflecting the various methods and color data used.
#

aqp_mixed_colors <- function(p, ztop = 0, zbot = 18,
                         m_hue = "m_hue", m_value = "m_value", m_chroma = "m_chroma",
                         d_hue = "d_hue", d_value = "d_value", d_chroma = "d_chroma") { 
  
  # trunc: new wrapper function around glomApply for constant top and bottom depth
  p.trc <- trunc(p, ztop, zbot)
  
  # calculate depth weights (horizon thickness)
  hzd <- horizonDepths(p)
  p.trc$wt <- p.trc[[hzd[2]]] - p.trc[[hzd[1]]]
  
  # iterate over profiles, calculating depth-weighted average value/chroma + dominant hues
  mixed <- profileApply(p.trc, 
                        frameify = TRUE, # frameify = TRUE means return data.frame result for _each profile_
                        function(p.sub) {
                          
    dwt <- aggregate(p.sub[["wt"]], by=list(p.sub[[d_hue]]), sum)
    mwt <- aggregate(p.sub[["wt"]], by=list(p.sub[[m_hue]]), sum)
    
    if (!nrow(dwt))
      dwt <- data.frame(Group=NA, x = 1)
    
    if (!nrow(mwt))
      mwt <- data.frame(Group=NA, x = 1)
    
    # construct result
    res <- data.frame(
        id = profile_id(p.sub), # profile ID
        n_hz = nrow(p.sub),     # number of horizons
        dominant_d_hue = dwt[which(dwt$x == max(dwt$x, na.rm = TRUE))[1], 1],
        dominant_m_hue = mwt[which(mwt$x == max(mwt$x, na.rm = TRUE))[1], 1],
        wt_d_value  = weighted.mean(p.sub[[d_value]],  p.sub[["wt"]]),
        wt_m_value  = weighted.mean(p.sub[[m_value]],  p.sub[["wt"]]),
        wt_d_chroma = weighted.mean(p.sub[[d_chroma]], p.sub[["wt"]]),
        wt_m_chroma = weighted.mean(p.sub[[m_chroma]], p.sub[["wt"]]))
    
    # put idname into ID name slot in result
    names(res)[1] <- idname(p.sub)
    
    return(res)
  })
  
  ### calculate colors mixed in LAB space
  
  ## moist
  p.trc$m_lab <- aqp::munsell2rgb(p.trc[[m_hue]], p.trc[[m_value]], p.trc[[m_chroma]])
  m_res <- aqp::aggregateColor(p.trc, groups = idname(p.trc), k = 1, col = "m_lab")$aggregate.data
  m_res_missing <- which(!profile_id(p.trc) %in% m_res[[idname(p.trc)]])
  
  # deal with missing values
  if(length(m_res_missing)) {
    m_res.tmp <- m_res[NA,][1:length(m_res_missing),]
    m_res.tmp[[idname(p.trc)]] <- profile_id(p.trc)[m_res_missing]
    m_res <- rbind(m_res, m_res.tmp)
    m_res <- m_res[match(profile_id(p.trc), m_res[[idname(p.trc)]]),] # reapply original order
  }
  
  ## dry
  p.trc$d_lab <- aqp::munsell2rgb(p.trc[[d_hue]], p.trc[[d_value]], p.trc[[d_chroma]])
  d_res <- aqp::aggregateColor(p.trc, groups = idname(p.trc), k = 1, col = "d_lab")$aggregate.data
  d_res_missing <- which(!profile_id(p.trc) %in% d_res[[idname(p.trc)]])
  
  # deal with missing values
  if(length(d_res_missing)) {
    d_res.tmp <- m_res[NA,][1:length(d_res_missing),]
    d_res.tmp[[idname(p.trc)]] <- profile_id(p.trc)[d_res_missing]
    d_res <- rbind(d_res, d_res.tmp)
    d_res <- d_res[match(profile_id(p.trc), d_res[[idname(p.trc)]]),] # reapply original order
  }
  
  # construct final result
  res <- list()
  res$depth_weighted <- mixed
  res$m_aggregate <- m_res
  res$d_aggregate <- d_res
  
  return(res)
}

# a fake profile
dat0 <- data.frame(id       = 1, 
                   top      = 0, 
                   bottom   = 50, 
                   d_hue    = "10YR", 
                   m_hue    = "10YR",
                   d_value  = 5, 
                   m_value  = 3, 
                   d_chroma = 3, 
                   m_chroma = 3)

# a real profile
dat1 <- data.frame(id       = 2, 
                   top      = c(0, 7, 27, 43), 
                   bottom   = c(7, 27, 43, 59), 
                   d_hue    = c("10YR","10YR","10YR","10YR"), 
                   m_hue    = c("10YR","10YR","10YR","10YR"),
                   d_value  = c(7, 4,     4, 5), 
                   m_value  = c(6, 2.5, 2.5, 4), 
                   d_chroma = c(2, 3,     3, 3), 
                   m_chroma = c(2, 2,     2, 3))

# same color data, shuffled depths a bit so more horizons in surface 0-18cm
dat2 <- data.frame(id       = 3, 
                   top      = c(0, 5, 12, 16), 
                   bottom   = c(5, 12, 16, 59), 
                   d_hue    = c("10YR","10YR","10YR","10YR"), 
                   m_hue    = c("10YR","10YR","10YR","10YR"),
                   d_value  = c(7, 4,     4, 5), 
                   m_value  = c(6, 2.5, 2.5, 4), 
                   d_chroma = c(2, 3,     3, 3), 
                   m_chroma = c(2, 2,     2, 3))

dat <- rbind(dat0, dat1, dat2)
depths(dat) <- id ~ top + bottom

dat_test <- aqp_mixed_colors(dat)

#data("loafercreek", package = "soilDB")
#res <- aqp_mixed_colors(loafercreek)

f <- fetchNASIS()
res <- aqp_mixed_colors(f)

#' Construct a Munsell Hue Value/Chroma Code
#'
#' @param the_hue Character vector of hue values (in the Munsell notation e.g. "10YR")
#' @param the_value Numeric vector of value values
#' @param the_chroma Numeric vector of chroma values
#' @param digits Number of digits to round value and chroma off to (default: 0; integer values)
#'
#' @return A character vector of Munsell color codes in the format \code{HUE VALUE/CHROMA}
#' @export hvc_to_munsell
#'
#' @examples
#' 
#' hvc_to_munsell("10YR", 2, 2)
#' # [1] "10YR 2/2"
#' 
hvc_to_munsell <- function(the_hue, the_value, the_chroma, digits = 0, allow57chroma = FALSE) {
  chroma <- the_chroma
  if(!allow57chroma) {
    # chromas of 5 and 7 are not chips available in soil color book
    # so divide input chromas in two, round, 
    # multipy by 2 to turn 5 -> [4 or 6] or 7 -> [6 or 8]
    idx <- which(round(chroma, digits) %in% c(5,7))
    if(length(idx))
      chroma[idx] <- 2 * round(chroma[idx] / 2, digits)
  }  
  res <- sprintf("%s %s/%s", the_hue, round(the_value, digits), round(chroma, digits))
  # allow for null chroma, convert other missing values to NA
  res[is.na(the_hue) | is.na(the_value)] <- NA

  return(res)
}

# this will round to nearest integer as needed
d_depthweight <- hvc_to_munsell(res$depth_weighted$dominant_d_hue,
                                res$depth_weighted$wt_d_value, 
                                res$depth_weighted$wt_d_chroma)

m_depthweight <- hvc_to_munsell(res$depth_weighted$dominant_m_hue,
                                res$depth_weighted$wt_m_value, 
                                res$depth_weighted$wt_m_chroma)

good.m.idx <- which(!grepl("NA", m_depthweight) & 
                      !is.na(m_depthweight))

good.d.idx <- which(!grepl("NA", d_depthweight) & 
                      !is.na(d_depthweight))


# compare depth-weighted dry versus moist -- just as a general sanity check
idx <- good.m.idx[good.m.idx %in% good.d.idx][5:10]

# idx <- c(7, 23, 70, 78, 96) # nice test IDs for loafercreek that demonstrate some things

test <- data.frame(id = res$depth_weighted$peiid[idx],
                   dry = d_depthweight[idx],
                   moist = m_depthweight[idx])

aqp::colorContrastPlot(test$dry, test$moist, # the moist colors (m2) are darker, as expected
                       labels = c("Dry", "Moist"))  
                                              
# this is a known "easter egg" in the loafercreek dataset 
# representing a fairly easy-to-commit data entry error (v.s. 7.5YR)
# note that the test indices includes one bogus "7.5R" hue in dry and has prominent contrast/high
                                              
# get the aggregated munsell chips from the 0-18cm interval
d_aqp <- hvc_to_munsell(res$d_aggregate$munsell.hue,
                        res$d_aggregate$munsell.value, 
                        res$d_aggregate$munsell.chroma)

m_aqp <- hvc_to_munsell(res$m_aggregate$munsell.hue,
                        res$m_aggregate$munsell.value, 
                        res$m_aggregate$munsell.chroma)

# compare depth-weighted dry versus aqp-mixed dry
moist_data <- data.frame(id = res$depth_weighted$peiid,
                         dwt = m_depthweight, 
                         aqp = m_aqp,
                         ord = res$m_aggregate$munsell.sigma)

dry_data <- data.frame(id = res$depth_weighted$peiid,
                       dwt = d_depthweight, 
                       aqp = d_aqp,
                       ord = res$d_aggregate$munsell.sigma)

# remove NA values (mostly from weighted.average missing values)
moist_data <- moist_data[good.m.idx,]
dry_data <- dry_data[good.d.idx,]

# order the data based on munsell.sigma
moist_data <- moist_data[order(moist_data$ord),]
dry_data <- dry_data[order(dry_data$ord),]

idx2 <- seq(1,nrow(moist_data),8)
aqp::colorContrastPlot(moist_data$dwt[idx2], moist_data$aqp[idx2], 
                       printMetrics = F, col.cex = 0.0,
                       labels = c("nearest chip\ndepth-weighted mean V + C\nw/ dominant H", 
                                  "aggregateColor"))
text(0.3,0.42, "Comparison of 0-18cm Numerical Mixing Methods for the Mollic/Umbric Epipedon")

idx3 <- seq(1,nrow(dry_data),8)
aqp::colorContrastPlot(dry_data$dwt[idx3], dry_data$aqp[idx3], printMetrics = F, col.cex = 0.0,
                       labels = c("nearest chip\ndepth-weighted mean V + C\nw/ dominant H", 
                                  "aggregateColor"))
text(0.3,0.42, "Comparison of 0-18cm Numerical Mixing Methods for the Mollic/Umbric Epipedon")
                       
