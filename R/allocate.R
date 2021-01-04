allocate <- function(..., to = NULL, droplevels = TRUE) {
  
  tos <- c(ss = "FAO Salt Severity", 
           bh = "FAO Black Soil", 
           dh = "ST Diagnostic Features"
           )
  
  # test
  if (is.null(to)) {
    stop(paste('the "to" argument should equal one of the following:', paste0('"', tos, '"', collapse = ', ')))
  }
  if (length(to) > 1) {
    stop('the length of the "to" argument should equal 1')
  }
  if (! to %in% tos) {
    stop(paste('the argument "to" currently supports allocating soil properties to one of the following classification schemes\\:', paste0('"', tos, '"', collapse = ', ')))
  }
    
  
  # allocate
  if (to == "FAO Salt Severity") {
    a <- .rank_salts(..., system = to, droplevels = droplevels)
  }
  
  if (to == "FAO Black Soil") {
    a <- .black_soil(...)
  }
  
  if (to == "ST Diagnostic Features") {
    
    # object = object; pedonid = "peiid"; hzname = "hzname"; hzdept = "hzdept"; hzdepb = "hzdepb"; texture = "texture"; hz_pat = ""; tex_pat = "br"; featkind = "argillic horizon"
    
    featkind <- c("lithic contact", "paralithic contact", "densic contact", "petrocalcic horizon", "calcic horizon", "secondary carbonates", "fragic soil properties", "reduced matrix")
    
    g <- lapply(featkind[1:6], function(x) {
      # g <- .guess_df(pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texture = "texture", featkind = "lithic contact")
      g <- .guess_df(..., featkind = x)
    })
    g <- do.call("rbind", g)
  }
  

  return(a)  
}
    

# To do add USDA and other salt classes
.rank_salts <- function(EC = NULL, pH = NULL, ESP = NULL, system = "FAO Salt Severity", droplevels = TRUE) {
  
  # EC = 1; pH = 3; ESP = 50
  l <- list(EC = EC, pH = pH, ESP = ESP)
  
  # tests
  # minimum dataset
  if (any(sapply(l, is.null))) {
    warning("the minimum dataset of soil properites for allocating to the Salt Severity classes are: EC (aka Electrial Conductivity), pH, ESP (aka Exchangable Sodium Percentage")
  }
  # length
  n <- sapply(l, length)
  if (! all(max(n) == n)) {
    stop("all arguments must have the same length")
  }
  
  
  fao_lev <- c(
    c("nonsaline", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
    )
  
  sc <- rep("none", times = length(EC))
  # saline soils
  sc <- ifelse(ESP <= 15, # & EC > 4 & pH <= 8.5, 
               as.character(
                 cut(EC,
                     breaks = c(-1, 0.75, 2, 4, 8, 15, 1000), 
                     labels = fao_lev[1:6],
                     right  = FALSE
                     )),
               sc
               )
  # sodic soils
  sc <- ifelse(EC <= 4 & (ESP > 15 | pH > 8.2),  
               as.character(
                 cut(ESP,
                     # breaks = c(0, 15, 30, 50, 70, 100),
                     breaks = c(-2, 30, 50, 70, 102),
                     # labels = fao_lev[7:11],
                     labels = fao_lev[8:11],
                     right  = FALSE
                     )),
               sc
               )
  # saline-sodic soils
  sc <- ifelse(EC > 4 & ESP > 15, "saline-sodic", sc)
  
  
  # convert to factor
  sc <- factor(sc, levels = c(fao_lev[1:6], "saline-sodic", fao_lev[8:11]))

  
  # droplevels
  if (droplevels == TRUE) {
    sc <- droplevels(sc)
  }
  
  return(sc)
}



.codify <- function(x, system = "salt severity", droplevels = TRUE) {

  if (system == "salt severity") {

    .codify_salt_severity(x, droplevels = droplevels)
    }
  }


.codify_salt_severity <- function(x, droplevels = TRUE) {
  
  # set levels
  fao_lev <- c(
    c("nonsaline", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
  )
  
  # test
  if (!is.integer(x)) stop("x is not an integer")
  if (!all(unique(x) %in% c(1:11, NA))) warning("some x values do not match the lookup table")

  sc <- factor(x, levels = 1:11, labels = c(fao_lev[1:6], "saline-sodic", fao_lev[8:11]))
  
  if (droplevels == TRUE) {
    sc <- droplevels(sc)
  }
  
  return(sc)
}
  


.black_soil <- function(object, pedonid = "peiid", hztop = "hzdept", hzbot = "hzdepb", OC = NULL, m_chroma = "m_chroma", m_value = "m_value", d_value = "d_value", CEC = NULL, BS = NULL, tropical = FALSE) { # thickness = NULL, horizon = TRUE 
  
  # OC = 1; chroma_moist = 3; value_moist = 3; value_dry = 5; thickness = 25; CEC = 20; BS = 50
  # pedonid = "123"; hztop = 0; hzbot = 25; OC = 1.6; m_chroma = 3; m_value = 3; d_value = 5; CEC = 25; BS = 50; tropical = FALSE
  # object <- data.frame(pedonid, hztop, hzbot, OC, m_chroma, m_value, d_value, CEC, BS)
  # pedonid = "pedonid"; hztop = "hztop"; hzbot = "hzbot"; OC = "OC"; m_chroma = "m_chroma"; m_value = "m_value"; d_value = "d_value"; CEC = "CEC"; BS = "BS"
  # pedonid = "idp"; hztop = "top"; hzbot = "bot"; OC = "oc"; m_chroma = "w_chroma"; m_value = "w_value"; d_value = "d_value"; CEC = "cec"; BS = NULL
  
  # test & standardize inputs
  vars <- list(pedonid = pedonid, hztop = hztop, hzbot = hzbot, OC = OC, m_chroma = m_chroma, m_value = m_value, d_value = d_value, CEC = CEC, BS = BS)
  
  # check object type
  if (class(object)[1] == "SoilProfileCollection") {
    df <- horizons(object)
  } else df <- object
  
  # check length of arguments
  if (any(sapply(vars, length) > 1)) {
    stop("the length of all arguments must be 1, except for object")
    # this will drop NULL arguments, which is ok for CEC & BS
  } else vars <- unlist(vars)
  
  # check arguments match df colnames & subset
  # no vars should be NA, but this will catch them if they are
  idx <- !is.na(vars) 
  if (! all(vars[idx] %in% names(df))) {
    stop("column names in object must match the other character vector input arguments")
  } else {
    df <- df[vars[idx]]
    vars2 <- names(vars[idx])
    names(df) <- vars2
    }
  
  # criteria
  # 2nd category of Black Soils
  # minimum dataset
  if (any(sapply(df[vars2][1:7], function(x) all(is.na(x))))) {
    stop("the minimum dataset of soil properites for allocating to the 2nd category of Black Soils are: OC (aka Organic Carbon), m_chroma, m_value, and d_value") # and thickness
  }
  bs2 <- with(df,
              (OC <= 20 & (OC >= 1.2 | (tropical == TRUE & OC >= 0.6))) 
              & m_chroma <= 3 
              & (m_value <= 3 & d_value <= 5)
  )
  
  # 1st category of Black Soils
  # minimum dataset
  if (!is.null(CEC) & !is.null(BS) & all(c("CEC", "BS") %in% names(df))) {
    
    bs1 <- bs2 & df$CEC >= 25 & df$BS >= 50
    
    } else {
    message("the minimum dataset of soil properites for allocating to the 1nd category of Black Soils, in addition to the 2nd category, are: CEC (aka Cation Exchange Capacity), BS (aka Base Saturation)")
    bs1 <- NA[1:nrow(df)]
  }
  
  # combine results and subset to 0-25cm
  df_bs  <- cbind(df[vars2[1:3]], BS1 = bs1, BS2 = bs2)
  df_bs  <- segment(df_bs, intervals = c(0, 25), hzdepcols = c("hztop", "hzbot"))
  df_bs  <- df_bs[df_bs$segment_id == "0-25", -6]
  
  # aggregate the horizons
  df_bs2 <- aggregate(cbind(BS1, BS2) ~ pedonid, data = df_bs, FUN = all, na.action = na.pass)
  df_bot <- aggregate(hzbot ~ pedonid, data = df_bs, FUN = function(x) max(x, na.rm = TRUE))
  
  # filter thickness > 25cm
  df_bs  <- merge(df_bs2, df_bot, by = "pedonid", all.x = TRUE)
  df_bs  <- within(df_bs, {
    BS1 = BS1 & as.integer(hzbot) == 25L
    BS2 = BS2 & as.integer(hzbot) == 25L
    hzbot = NULL
  })
  
  names(df_bs)[1] <- pedonid
  
  return(df_bs)
}



# guess diagnostic features
.guess_df <- function(object = NULL, pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texture = "texture", featkind = NULL) {
  
  # pedonid = "peiid"; hzname = "hzname"; hzdept = "hzdept"; hzdepb = "hzdepb"; texture = "texture"; hz_pat = ""; tex_pat = "br"; featkind = "argillic horizon"
  
  # standardize inputs
  vars <- list(pedonid = pedonid, hzname = hzname, hzdept = hzdept, hzdepb = hzdepb, texture = texture)
  
  # standardize inputs
  if (class(object)[1] == "SoilProfileCollection") {
    df <- horizons(object)
  } else df <- object
  
  # subset df columns
  df <- df[unlist(vars)]
  names(df) <- names(unlist(vars))
  df$texture <- tolower(df$texture)
  
  # match pattern
  
  # lithic contact
  if (featkind == "lithic contact") {
    message(paste("guessing", featkind))
    idx <- 
      grepl("R|Dr", df$hzname) |
      df$texture %in% c("br", "wb", "uwb")
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  # paralithic contact
  if (featkind == "paralithic contact") {
    message(paste("guessing", featkind))
    idx  <- 
      grepl("Cr|CR", df$hzname) &
      (df$texture %in% c("br", "wb", "uwb") | is.na(df$texture))
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  # densic contact
  if (featkind == "densic contact") {
    message(paste("guessing", featkind))
    idx <- grepl("d$|D$|d[1:9]|D[1:9]", df$hzname)
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  # petrocalcic horizon
  if (featkind == "petrocalcic horizon") {
    message(paste("guessing", featkind))
    idx  <- 
      grepl("kkm|kkqm", df$hzname) &
      ((grepl("cem", df$texture) & !grepl("-br", df$texture)) | is.na(df$texture))
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  # calcic horizon
  if (featkind == "calcic horizon") {
    message(paste("guessing", featkind))
    idx <- 
      grepl("kk$|kk[1:9]|kkq$|kkq[1:9]|kkb", df$hzname) & (!grepl("cem-", df$texture) | is.na(df$texture))
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  # secondary carbonates
  if (featkind == "secondary carbonates") {
    message(paste("guessing", featkind))
    idx <- grepl("k", df$hzname) & !grepl("kk", df$hzname)
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  
  df_sub <- df[is.na(df$featkind), ]
  
  # aggregate depths
  sp <- NULL
  if (any(!is.na(df$featkind))) {
    sp <- aggregate(hzdept ~ pedonid + featkind, data = df, FUN = function(x) min(x, na.rm = TRUE))
    sp$featdepb <- aggregate(hzdepb ~ pedonid + featkind, data = df, FUN = function(x) max(x, na.rm = TRUE))$hzdepb
    names(sp)[3] <- "featdept"
  }
  
  return(sp)
}

