#' Estimate available water capacity for fine-earth fraction
#'
#' @param texcl character, USDA textural class fine earth fraction
#' @param omcl integer, Organic matter class. 1: less than 1.5 percent, 2: less than 5, 3: greate than 5
#' @param precision integer, Number of decimal places in result default: 2
#' @param FUN Function for interpolating between table values default: \code{mean}
#' @param ... Additional arguments to \code{FUN}
#'
#' @return A numeric vector double containing estimated available water capacities for fine-earth fraction.
#' @export estimateAWC
#'
#' @examples
#' 
#' # organic matter, loam texture, low medium and high OM
#' base.awc <- estimateAWC(c("l","l","l"), c(1, 2, 3), na.rm = TRUE)
#' base.awc
#' 
estimateAWC <- function(texcl, omcl, precision = 2,
                        FUN = mean, ...) {

  #awc_lut, loaded from region2 textureclass-awc lookup table
  awc_lut <- data.frame(texcl = c(4L, 8L, 16L, 6L, 24L, 11L, 13L, 12L, 14L, 5L, 22L, 7L, 25L, 10L, 21L, 20L, 18L, 3L, 19L, 17L, 1L, 2L, 9L, 15L, 23L), 
                        texcl_label =c("COS", "GR", "S", "FS", "VFS", "LCOS", "LS", "LFS", "LVFS", 
                                       "COSL", "SL", "FSL", "VFSL", "L", "SIL", "SICL", "SCL", "CL", 
                                       "SIC", "SC", "C", "C_heavy", "HPM", "MPM", "SPM"), 
         loOM_l = c(0.02, 0.02, 0.05, 0.05, 0.05, 0.05, 0.06, 0.08, 0.09, 0.09, 0.1,  0.13, 0.14, 0.17, 0.15, 0.17, 0.14, 0.17, 0.14, 0.14, 0.14, 0.12, NA, NA, NA), 
         loOM_h = c(0.04, 0.04, 0.08, 0.08, 0.08, 0.07, 0.08, 0.11, 0.11, 0.12, 0.13, 0.15, 0.17, 0.18, 0.2,  0.21, 0.18, 0.21, 0.17, 0.16, 0.16, 0.15, NA, NA, NA), 
         mdOM_l = c(0.03, 0.03, 0.06, 0.06, 0.06, 0.06, 0.07, 0.09, 0.1,  0.1,  0.12, 0.15, 0.16, 0.17, 0.2,  0.18, 0.16, 0.15, 0.11, 0.15, 0.15, 0.13, NA, NA, NA), 
         mdOM_h = c(0.05, 0.05, 0.08, 0.08, 0.08, 0.07, 0.09, 0.12, 0.12, 0.13, 0.14, 0.17, 0.19, 0.19, 0.22, 0.2,  0.18, 0.19, 0.13, 0.17, 0.17, 0.16, NA, NA, NA), 
         hiOM_l = c(0.04, 0.04, 0.07, 0.07, 0.07, 0.07, 0.08, 0.1,  0.11, 0.11, 0.13, 0.16, 0.18, 0.2,  0.22, 0.21, 0.18, 0.17, 0.12, 0.13, 0.13, 0.15, 0.35, 0.45, 0.55), 
         hiOM_h = c(0.06, 0.06, 0.09, 0.09, 0.09, 0.08, 0.1,  0.13, 0.13, 0.14, 0.15, 0.18, 0.2,  0.22, 0.24, 0.23, 0.2,  0.19, 0.14, 0.15, 0.15, 0.18, 0.45, 0.55, 0.65), stringsAsFactors = FALSE)
  
  if (length(texcl) != length(omcl))
    stop("Error: Mismatch in length of input vectors `texcl` and `omcl`")
  
  out <- rep(NA, length(texcl))
  idx <- match(toupper(texcl), toupper(awc_lut$texcl_label))
  
  for (i in seq_along(idx)) {
    x <- awc_lut[idx[i],]
    idx.lo <- 2*omcl[i] + 1
    idx.hi <- idx.lo + 1 
    if (!is.na(idx.hi) & !is.na(idx.lo)) {
      out[i] <- FUN(as.numeric(x[,idx.lo:idx.hi]))
    } else out[i] <- NA
  }
  return(round(out, precision))
}


#' Apply rock fragment or salt correction to available water content
#'
#' @param awc Numeric vector of available water capacities (e.g. from \code{estimateAWC})
#' @param total_rf Numeric vector  of rock fragment volume percentage, 0 - 100 
#' @param gravel Numeric vector of gravel volume percentage, 0 - 100 
#' @param ec Numeric vector of electrical conductivity, mmhos/cm
#'
#' @return A numeric vector (double) containing estimated available water capacities corrected for rock fragments and salts
#' @export correctAWC
#'
#' @examples
#' 
#' # medium organic matter, loam texture 
#' base.awc <- 0.18 # estimateAWC(texcl = "l", omcl = 2, na.rm = TRUE)
#' 
#' # medium organic matter, loam texture w/ 23% rock fragments by volume 
#' corrected.awc <- correctAWC(base.awc, total_rf = 23)
#' corrected.awc
#' 
#' # medium organic matter, loam texture w/ 0% frags by volume and 8 mmhos/cm salts
#' salty.awc <- correctAWC(base.awc, total_rf = 0, ec = 8)
#' salty.awc
correctAWC <- function(awc, total_rf, gravel = NULL, ec = NULL) {
  
  # rf_lut from region2 rock fragment/salt correction lookup table
  rf_lut <- .get_rf_ec_lut_RO2()
  
  if (length(awc) != length(total_rf)) {
    if (length(awc) == 1)
      awc <- rep(awc, length(total_rf))
    else if (length(total_rf) == 1)
      total_rf <- rep(total_rf, length(awc))
    else 
      stop("Error: Mismatch in length of input vectors.")
  }
  use_salt_correction <- FALSE
  
  lut.row.idx <- .get_rf_lut_row(rf_lut, total = total_rf, gravel = gravel)
  lut.col.idx <- .get_rf_lut_column(rf_lut, awc)
  
  awc_c <- length(lut.row.idx)
  awc_c[is.na(lut.col.idx)] <- NA
  
  if (length(lut.row.idx) != length(lut.col.idx)) {
    stop("unable to lookup rock fragment correction", call. = FALSE)
  }
  
  for (i in 1:length(lut.row.idx)) {
    if (!is.na(lut.col.idx[i])) {
      awc_c[i] <- rf_lut[lut.row.idx[i], lut.col.idx[i]]
    } else { 
      awc_c[i] <- NA
    }
  }
  
  if (!is.null(ec) & (length(ec) == 1 | length(ec) == length(awc)) & any(ec >= 2)) 
    use_salt_correction = TRUE
    
  if (length(ec) == 1)
    ec <- rep(ec, length(awc))
    
  if (use_salt_correction) {
    # use EC to identify degree of reduction
    lut.row.idx2 <- .get_rf_lut_row(rf_lut, ec = ec)
    
    # use RF-reduced AWC for salt correction
    lut.col.idx2 <- .get_rf_lut_column(rf_lut, awc_c)
    
    awc_c2 <- length(lut.row.idx2)
    awc_c2[is.na(lut.col.idx2)] <- NA
    
    # use lookup table and return result
    for (i in 1:length(lut.row.idx2)) {
      if (!is.na(lut.col.idx2[i])) {
        awc_c2[i] <- rf_lut[lut.row.idx2[i], na.omit(lut.col.idx2[i])]
      } else { 
        awc_c2[i] <- NA
      }
    }
    return(unlist(awc_c2))
  }
  return(unlist(awc_c))
}

# returns most limiting RF-reduction based on total RF, gravel or electrical conductivity
.get_rf_lut_row <- function(rf_lut, total = NULL, gravel = NULL, ec = NULL) {
  n <- max(length(total), length(gravel), length(ec))
  buf <- numeric(n)
  for (i in 1:n) {
    idx1 <- idx2 <- idx3 <- -2
    if (!is.null(total[i])) 
      idx1 <- which(rf_lut$rf_byvol_l <= total[i] & rf_lut$rf_byvol_h > total[i])
    if (!is.null(gravel[i])) 
      idx2 <- which(rf_lut$gravel_l <= gravel[i] & rf_lut$gravel_h > gravel[i])
    if (!is.null(ec[i])) 
      idx3 <- which(rf_lut$salts_mmhos_l <= ec[i] & rf_lut$salts_mmhos_h > ec[i])
    buf[i] <- max(idx1, idx2, idx3) + 1
  }
  return(buf)
}

.get_rf_lut_column <- function(rf_lut, awc_) {
  # TODO: relies on 1:1 match of AWC to column in LUT - in future, use ranges and interpolate
  return(match(round(awc_,2), rf_lut[1,7:ncol(rf_lut)], nomatch = NA) + 7)
}

.get_rf_ec_lut_RO2 <-  function() {
  # In this form the relationship is apparent
  structure(list(   rf_byvol_l = c(   0,    5,   10,   20,   30,   40,   45,   50,   55,   60,   65,   70,   75,   80,   80, 82.5,   85, 87.5,   90,   95,  100), 
                    rf_byvol_h = c(   5,   10,   20,   30,   40,   45,   50,   55,   60,   65,   70,   75,   80,   80, 82.5,   85, 87.5,   90,   95,   99,  100), 
                 salts_mmhos_l = c(   0,    2,    4,    6,    8,   10,   12,   14,   16,   17,   18,   19,   20, 85/4, 22.5, 95/4,   25, 27.5,   30,   35, 37.5), 
                 salts_mmhos_h = c(   2,    4,    6,    8,   10,   12,   14,   16,   17,   18,   19,   20, 85/4, 22.5, 95/4,   25, 27.5,   30,   35, 37.5,   40), 
                      gravel_l = c(   0,    5,   10,   15,   20,   25,   30,   35,   40,   45,   50,   55,   60,   65,   70,   75,   80,   85,   90,   95,  100), 
                      gravel_h = c(   5,   10,   15,   20,   25,   30,   35,   40,   45,   50,   55,   60,   65,   70,   75,   80,   85,   90,   95,   99,  100), 
                            c1 = c(0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,    0,    0,    0), 
                            c2 = c(0.04, 0.04, 0.04, 0.03, 0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01,    0,    0,    0), 
                            c3 = c(0.05, 0.05, 0.05, 0.04, 0.04, 0.04, 0.04, 0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01,    0,    0), 
                            c4 = c(0.06, 0.06, 0.05, 0.05, 0.05, 0.05, 0.04, 0.04, 0.04, 0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01,    0,    0),       
                            c5 = c(0.07, 0.07, 0.06, 0.06, 0.06, 0.05, 0.05, 0.05, 0.04, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01,    0,    0), 
                            c6 = c(0.08, 0.08, 0.07, 0.07, 0.06, 0.06, 0.06, 0.05, 0.05, 0.04, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.02, 0.01, 0.01,    0,    0),                                    c7 = c(0.09, 0.09, 0.08, 0.08, 0.07, 0.07, 0.06, 0.06, 0.05, 0.05, 0.05, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.01, 0.01, 0.01,    0),       
                            c8 = c(0.1,  0.1,  0.09, 0.09, 0.08, 0.08, 0.07, 0.07, 0.06, 0.06, 0.05, 0.05, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.01, 0.01,    0), 
                            c9 = c(0.11, 0.11, 0.1,  0.09, 0.09, 0.08, 0.08, 0.07, 0.07, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.01, 0.01,    0), 
                           c10 = c(0.12, 0.11, 0.11, 0.1,  0.1,  0.09, 0.08, 0.08, 0.07, 0.07, 0.06, 0.05, 0.05, 0.04, 0.04, 0.03, 0.02, 0.02, 0.01, 0.01,    0), 
                           c11 = c(0.13, 0.12, 0.12, 0.11, 0.1,  0.1,  0.09, 0.09, 0.08, 0.07, 0.07, 0.06, 0.05, 0.05, 0.04, 0.03, 0.03, 0.02, 0.01, 0.01,    0), 
                           c12 = c(0.14, 0.13, 0.13, 0.12, 0.11, 0.11, 0.1,  0.09, 0.08, 0.08, 0.07, 0.06, 0.05, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.01,    0), 
                           c13 = c(0.15, 0.14, 0.14, 0.13, 0.12, 0.11, 0.1,  0.1,  0.09, 0.08, 0.08, 0.07, 0.05, 0.05, 0.05, 0.04, 0.03, 0.02, 0.02, 0.01,    0), 
                           c14 = c(0.16, 0.15, 0.14, 0.14, 0.13, 0.12, 0.11, 0.1,  0.1,  0.09, 0.08, 0.07, 0.06, 0.06, 0.05, 0.04, 0.03, 0.02, 0.02, 0.01,    0), 
                           c15 = c(0.17, 0.16, 0.15, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1,  0.1,  0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.03, 0.02, 0.01,    0), 
                           c16 = c(0.18, 0.17, 0.16, 0.15, 0.14, 0.14, 0.13, 0.12, 0.11, 0.1,  0.09, 0.08, 0.07, 0.06, 0.05, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c17 = c(0.19, 0.18, 0.17, 0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.11, 0.1,  0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c18 = c(0.2,  0.19, 0.18, 0.17, 0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1,  0.09, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c19 = c(0.21, 0.2,  0.19, 0.18, 0.17, 0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1,  0.09, 0.08, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c20 = c(0.22, 0.21, 0.2,  0.19, 0.18, 0.17, 0.16, 0.15, 0.13, 0.13, 0.11, 0.11, 0.09, 0.08, 0.07, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c21 = c(0.23, 0.22, 0.21, 0.2,  0.19, 0.18, 0.17, 0.15, 0.14, 0.13, 0.12, 0.11, 0.09, 0.08, 0.07, 0.05, 0.04, 0.03, 0.02, 0.01,    0), 
                           c22 = c(0.24, 0.23, 0.22, 0.2,  0.19, 0.19, 0.17, 0.16, 0.14, 0.14, 0.12, 0.11, 0.1,  0.09, 0.08, 0.06, 0.04, 0.03, 0.02, 0.01,    0), 
                           c23 = c(0.25, 0.24, 0.23, 0.21, 0.2,  0.19, 0.18, 0.16, 0.15, 0.14, 0.13, 0.11, 0.1,  0.09, 0.08, 0.06, 0.05, 0.04, 0.03, 0.01,    0), 
                           c24 = c(0.3,  0.29, 0.27, 0.26, 0.24, 0.23, 0.21, 0.2,  0.18, 0.17, 0.15, 0.14, 0.12, 0.11, 0.09, 0.08, 0.06, 0.05, 0.03, 0.02,    0)), 
                      class = "data.frame", row.names = c(NA, -21L))
}
