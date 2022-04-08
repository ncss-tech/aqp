#' @title Allocate soil properties within various classification systems.
#' 
#' @description Generic function to allocate soil properties to different classification schemes.
#'
#' @param ... arguments to specific allocation functions, see details and examples
#' 
#' @param to character specifying the classification scheme: FAO Salt Severity, FAO Black Soil (see details for the required \code{...})
#' 
#' @param droplevels logical indicating whether to drop unused levels in factors. This is useful when the results have a large number of unused classes, which can waste space in tables and figures.
#' 
#' 
#' @details
#' This function is intended to allocate a set of soil properties to an established soil classification scheme, such as Salt Severity or Black Soil. Allocation is semantically different from classification. While classification is the 'act' of developing a grouping scheme, allocation is the assignment or identification of measurements to a established class (Powell, 2008). 
#'  
#' ## Usage Details
#' 
#' Each classification scheme (\code{to} argument) uses a different set of arguments.
#' 
#' - `FAO Salt Severity`
#'   + **EC:** electrical conductivity column name, dS/m
#'   + **pH:** pH column name, saturated paste extract
#'   + **ESP:** exchangeable sodium percentage column name, percent
#'   
#' - `FAO Black Soils`
#'   + **object:** a `data.frame` or `SoilProfileCollection`
#'   + **pedonid:** pedon ID column name, required when \code{object} is a \code{data.frame}
#'   + **hztop:** horizon top depth column name, required when \code{object} is a \code{data.frame}
#'   + **hzbot:** horizon bottom depth column name, required when \code{object} is a \code{data.frame}
#'   + **OC**: organic carbon column name, percent
#'   + **m_chroma:** moist Munsell chroma column name
#'   + **m_value:** moist Munsell value column name
#'   + **d_value:** dry Munsell value column name
#'   + **CEC:** cation exchange capacity column name (NH4OAc at pH 7), units of cmol(+)/kg soil
#'   + **BS:** base saturation column name (NH4OAc at pH 7), percent
#'   + **tropical:** logical, data are associated with "tropical soils"
#'   
#' - `ST Diagnostic Features`
#'   + **object:** a `data.frame` or `SoilProfileCollection`
#'   + **pedonid:** pedon ID column name, required when \code{object} is a \code{data.frame}
#'   + **hzname:** horizon name column, required when \code{object} is a \code{data.frame}
#'   + **hztop:** horizon top depth column name, required when \code{object} is a \code{data.frame}
#'   + **hzbot:** horizon bottom depth column name, required when \code{object} is a \code{data.frame}
#'   + **texture:** soil texture class (USDA) column name
#'   + **rupresblkcem:** rupture resistance column name
#'   + **m_value:** moist Munsell value column name
#'   + **m_chroma:** moist Munsell chroma column name
#'   + **d_value:** dry Munsell value column name
#'   + **BS:** base saturation column name (method ??), percent
#'   + **OC**: organic carbon column name, percent
#'   + **n_value:** ??
#'   + **featkind:** ??
#' 
#' @note The results returned by \code{allocate(to = "ST Diagnostic Features")} currently return a limited set of diagnostic features that are easily defined. Also, the logic implemented for some features does not include all the criteria defined in the Keys to Soil Taxonomy.
#' 
#' 
#'
#' @return A vector or \code{data.frame} object.
#' 
#' @references 
#' Abrol, I., Yadav, J. & Massoud, F. 1988. \href{https://www.fao.org/3/x5871e/x5871e00.htm}{Salt-affected soils and their management}. No. Bulletin 39. Rome, FAO Soils.
#' 
#' FAO. 2006. \href{https://www.fao.org/publications/card/en/c/903943c7-f56a-521a-8d32-459e7e0cdae9/}{Guidelines for soil description}. Rome, Food and Agriculture Organization of the United Nations.
#' 
#' FAO. 2020. DEFINITION | What is a black soil? (online). (Cited 28 December 2020). http://www.fao.org/global-soil-partnership/intergovernmental-technical-panel-soils/gsoc17-implementation/internationalnetworkblacksoils/more-on-black-soils/definition-what-is-a-black-soil/es/
#'   
#'   Powell, B., 2008. Classifying soil and land, in: McKenzie, N.J., Grundy, M.J., Webster, R., Ringrose-Voase, A.J. (Eds.), Guidelines for Survey Soil and Land Resources, Australian Soil and Land Survey Handbook Series. CSIRO, Melbourne, p. 572.
#' 
#' Richards, L.A. 1954. \href{https://www.ars.usda.gov/ARSUserFiles/20360500/hb60_pdf/hb60complete.pdf}{Diagnosis and Improvement of Saline and Alkali Soils}. U. S. Government Printing Office. 166 pp.
#' 
#' Soil Survey Staff, 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, D.C.
#' 
#' 
#' @export
#'
#' @examples
#' 
#' # Salt Severity
#' test <- expand.grid(
#'   EC  = sort(sapply(c(0, 0.75, 2, 4, 8, 15, 30), function(x) x + c(0, -0.05, 0.05))),
#'   pH  = c(8.1, 8.2, 8.3, 8.4, 8.5, 8.6),
#'   ESP = sort(sapply(c(0, 15, 30, 50, 70, 100), function(x) x + c(0, 0.1, -0.1)))
#' )
#' test$ss      <- with(test, allocate(EC = EC, pH = pH, ESP = ESP, to = "FAO Salt Severity"))
#' table(test$ss)
#' 
#' # Black Soil Category 1 (BS1)
#' test <- expand.grid(
#'   dept = seq(0, 50, 10),
#'   OC   = sort(sapply(c(0, 0.6, 1.2, 20, 40), function(x) x + c(0, -0.05, 0.05))),
#'   chroma_moist  = 2:4,
#'   value_moist   = 2:4,
#'   value_dry     = 4:6,
#'   thickness     = 24:26,
#'   CEC           = 24:26,
#'   BS            = 49:51,
#'   tropical      = c(TRUE, FALSE)
#' )
#' test$pedon_id <- rep(1:21870, each = 6)
#' test$depb     <- test$dept + 10
#' 
#' bs1 <- allocate(test, pedonid = "pedon_id", hztop = "dept", hzbot = "depb", 
#'                 OC = "OC", m_chroma = "chroma_moist", m_value = "value_moist", 
#'                 d_value = "value_dry", CEC = "CEC", BS = "BS", 
#'                 to = "FAO Black Soil"
#' )
#' 
#' table(BS1 = bs1$BS1, BS2 = bs1$BS2)
#' 
#' 
#' # SoilProfileCollection interface
#' 
#' data(sp3)
#' depths(sp3) <- id ~ top + bottom
#' hzdesgnname(sp3) <- 'name'
#' 
#' # fake base saturation
#' horizons(sp3)$bs <- 75
#' 
#' plotSPC(sp3)
#' 
#' allocate(
#'   sp3, 
#'   to = 'FAO Black Soil', 
#'   OC = 'tc', 
#'   m_chroma = 'chroma', 
#'   m_value = 'value', 
#'   d_value = 'value',
#'   CEC = 'cec',
#'   BS = 'bs'
#' )
#' 
#' # make a copy and edit horizon values
#' x <- sp3
#' x$value <- 2
#' x$chroma <- 2
#' x$cec <- 26
#' x$tc <- 2
#' 
#' x$soil_color <- munsell2rgb(x$hue, x$value, x$chroma)
#' 
#' plotSPC(x)
#' 
#' allocate(
#'   x, 
#'   to = 'FAO Black Soil', 
#'   OC = 'tc', 
#'   m_chroma = 'chroma', 
#'   m_value = 'value', 
#'   d_value = 'value',
#'   CEC = 'cec',
#'   BS = 'bs'
#' )
#' 
#' 
#' # Soil Taxonomy Diagnostic Features
#' data(sp1)
#' df <- allocate(object = sp1, pedonid = "id", hzname = "name", 
#'                hzdept = "top", hzdepb = "bot", texture = "texture", 
#'                to = "ST Diagnostic Features"
#' )
#' aggregate(featdept ~ id, data = df, summary)
#' 
allocate <- function(..., to = c("FAO Salt Severity", "FAO Black Soil", "ST Diagnostic Features"), droplevels = TRUE) {
  
  # sanity check
  to <- match.arg(to, several.ok = FALSE)
  
  # select the appropriate system
  a <- switch(
    to,
    "FAO Salt Severity" = {
      .rank_salts(..., system = to, droplevels = droplevels) 
    },
    "FAO Black Soil" = {
      .black_soil(...)  
    },
    "ST Diagnostic Features" = {
      # object = object
      # pedonid = "peiid"
      # hzname = "hzname"
      # hzdept = "hzdept"
      # hzdepb = "hzdepb"
      # texture = "texture"
      # hz_pat = ""
      # tex_pat = "br"
      # featkind = "argillic horizon"
      
      featkind <- c("lithic contact", "paralithic contact", "densic contact", "petrocalcic horizon", "calcic horizon", "secondary carbonates", "mollic epipedon") #, "reduced matrix")
      
      a <- lapply(featkind, function(x) {
        # a <- .guess_df(pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texture = "texture", featkind = "lithic contact")
        a <- .guess_df(..., featkind = x)
      })
      a <- do.call("rbind", a)
    }
  )
  
  return(a)  
}
    

# To do add USDA and other salt classes
## TODO consider optional object = NULL
## TODO safe handling of NA
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
  
  
  ## TODO: why?
  sc <- rep("none", times = length(EC))
  
  ## TODO: consider separate saline / sodic classifaction
  
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
  

## TODO: useful error message, this isn't helpful:
# Error in .black_soil(...) : 
# column names in object must match the other character vector input arguments

## TODO: there is currently no way to document these arguments, critical because each has an expected unit of measure


.black_soil <- function(object, pedonid = "peiid", hztop = "hzdept", hzbot = "hzdepb", OC = NULL, m_chroma = "m_chroma", m_value = "m_value", d_value = "d_value", CEC = NULL, BS = NULL, tropical = FALSE) { # thickness = NULL, horizon = TRUE 
  
  # OC = 1; chroma_moist = 3; value_moist = 3; value_dry = 5; thickness = 25; CEC = 20; BS = 50
  # pedonid = "123"; hztop = 0; hzbot = 25; OC = 1.6; m_chroma = 3; m_value = 3; d_value = 5; CEC = 25; BS = 50; tropical = FALSE
  # object <- data.frame(pedonid, hztop, hzbot, OC, m_chroma, m_value, d_value, CEC, BS)
  # pedonid = "pedonid"; hztop = "hztop"; hzbot = "hzbot"; OC = "OC"; m_chroma = "m_chroma"; m_value = "m_value"; d_value = "d_value"; CEC = "CEC"; BS = "BS"
  # pedonid = "idp"; hztop = "top"; hzbot = "bot"; OC = "oc"; m_chroma = "w_chroma"; m_value = "w_value"; d_value = "d_value"; CEC = "cec"; BS = NULL
  
  # check object type
  # SoilProfileCollection objects have a number of required arguments defined internally
  if (inherits(object, 'SoilProfileCollection')) {
    # extract relevant metadata from the SPC
    pID <- idname(object)
    hztb <- horizonDepths(object)
    
    # horizons as data.frame-like obj
    df <- horizons(object)
    
    # setup variables used later, using SPC metadata if possible
    vars <- list(pedonid = pID, hztop = hztb[1], hzbot = hztb[2], OC = OC, m_chroma = m_chroma, m_value = m_value, d_value = d_value, CEC = CEC, BS = BS)
    
  } else {
    # this is likely a data.frame
    df <- object
    
    # setup variables used later, all from provided arguments
    vars <- list(pedonid = pedonid, hztop = hztop, hzbot = hzbot, OC = OC, m_chroma = m_chroma, m_value = m_value, d_value = d_value, CEC = CEC, BS = BS)
  }
  
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
  df_bs  <- df_bs[df_bs$segment_id == "00-25", -6]
  
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
.guess_df <- function(object = NULL, pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texture = "texture", rupresblkcem = "rupresblkcem", m_value = "m_value", d_value = "d_value", m_chroma = "m_chroma", BS = "BS", OC = "OC", n_value = "n_value", featkind = NULL) {
  
  # pedonid = "peiid"; hzname = "hzname"; hzdept = "hzdept"; hzdepb = "hzdepb"; texture = "texture"; hz_pat = ""; tex_pat = "br"; featkind = "mollic epipedon"; rupresblkcem = "rupresblkcem"; m_value = "m_value"; d_value = "d_value"; m_chroma = "m_chroma"; BS = NA; OC = NA; n_value = "n_value"
  # object = sp1; pedonid = "id"; hzname = "name"; hzdept = "top"; hzdepb = "bot"; texture = "texture"
  # vars <- list(pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texture = "texture", rupresblkcem = "rupresblkcem", m_value = "m_value", d_value = "d_value", m_chroma = "m_chroma", OC = "OC", BS = "BS", n_value = "n_value")
  
  # standardize inputs
  vars <- list(pedonid = pedonid, hzname = hzname, hzdept = hzdept, hzdepb = hzdepb, texture = texture, rupresblkcem = rupresblkcem, m_value = m_value, d_value = d_value, m_chroma = m_chroma, OC = OC, BS = BS, n_value = n_value)
  
  # standardize inputs
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
  if (! all(vars %in% names(df))) {
    warning("the minimum dataset includes: pedonid, hzdept, hzdepb, and hzname; if texture or rupreblkcem are missing the resulting diagnostic features are inferred from the available information")
    idx <- vars %in% names(df)
    mis <- vars[! idx]
    df <- df[vars[idx]]
    df[names(mis)] <- NA
    vars2 <- names(vars)
    names(df) <- vars2
  } else {
    df <- df[vars]
    vars2 <- names(vars)
    names(df) <- vars2
  }
  df$texture      <- tolower(df$texture)
  df$rupresblkcem <- tolower(df$rupresblkcem)
  
  # match pattern
  
  # lithic contact
  if (featkind == "lithic contact") {
    message(paste("guessing", featkind))
    idx_hzn <-  grepl("R|Dr", df$hzname) & !grepl("\\/", df$hzname)
    idx_tex <- !grepl("Cr|CR", df$hzname) & (df$texture %in% c("br", "wb", "uwb") | is.na(df$texture))
    
    lev <- c("strongly cemented", "very strongly cemented", "indurated", "strongly", "extremely strongly", "H") 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have R horizons that do not meeting the texture or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # paralithic contact
  if (featkind == "paralithic contact") {
    message(paste("guessing", featkind))
    
    idx_hzn <-  grepl("Cr|CR", df$hzname)
    idx_tex <- !grepl("R|Dr", df$hzname) & (df$texture %in% c("br", "wb", "uwb") | is.na(df$texture))
    
    lev <- c("extremely weakly cememented", "very weakly cemented", "weakly cemented", "moderately cemented", "weakly", "moderately", "S") 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Cr horizons that do not meeting the texture or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }

    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
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
    
    idx_hzn  <- grepl("kkm|kkqm", df$hzname)
    idx_tex  <- ((grepl("cem", df$texture) & !grepl("-br", df$texture)) | is.na(df$texture))
    
    lev <- "noncemented" 
    idx_cem <- !df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Bkkm|Bkkqm horizons that do not meeting the texture or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # calcic horizon
  if (featkind == "calcic horizon") {
    message(paste("guessing", featkind))
    
    idx_hzn <-  grepl("kk$|kk[1:9]|kkq$|kkq[1:9]|kkb$|kkb[1:9]", df$hzname)
    idx_tex <- (!grepl("cem-", df$texture) | is.na(df$texture))
    
    lev <- "noncemented" 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Bkk horizons that do not meeting the texture or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # secondary carbonates
  if (featkind == "secondary carbonates") {
    message(paste("guessing", featkind))
    
    idx_hzn <- grepl("k", df$hzname) & !grepl("kk", df$hzname)
    
    df$featkind <- ifelse(idx_hzn, featkind, NA)
  }
  
  
  # mollic epipedon
   if (featkind == "mollic epipedon") {
     message(paste("guessing", featkind))
     
     idx_hzn <- !grepl("O|Ao|R|W|M|C|\\/", df$hzname)
     # need to add structure to fetchNASIS
     idx_col <- 
       (df$m_value  <= 3 | !is.na(df$m_value))  & 
       (df$d_value  <= 5 | !is.na(df$d_value))  &
        df$m_chroma <= 3                        &
       (!is.na(df$m_value) & !is.na(df$d_value))
     idx_bs  <- (df$BS >= 50 | is.na(df$BS)) # & (df$ph1to1 > 6 | is.na(df$ph1to1))
     idx_oc  <- 
       ((df$OC >= 2.5 | is.na(df$OC)) & (df$m_value %in% 4:5 | is.na(df$m_value))) |
       ((df$OC >= 0.6 | is.na(df$OC)) & (df$m_value < 4      | is.na(df$m_value)))
     idx_nv <- (df$n_value < 0.7 | is.na(df$n_value))
     
     df$featkind <- ifelse(idx_hzn & idx_col & idx_bs & idx_oc & idx_nv, featkind, NA)
   }
  
  
  # subset features
  idx <- "featkind" %in% names(df)
  if (idx) {
    
    df_sub <- df[!is.na(df$featkind), ]
    
    # aggregate depths
    idx <- !is.na(df_sub$featkind)
    if (any(idx) & sum(idx, na.rm = TRUE) > 1) {
      sp <- aggregate(hzdept ~ pedonid + featkind, data = df_sub, FUN = function(x) min(x, na.rm = TRUE))
      sp$featdepb <- aggregate(hzdepb ~ pedonid + featkind, data = df_sub, FUN = function(x) max(x, na.rm = TRUE))$hzdepb
      names(sp)[3] <- "featdept"
    } else {
      sp <- df_sub[c("pedonid", "featkind", "hzdept", "hzdepb")]
      names(sp)[3:4] <- c("featdept", "featdepb")
    }
  
    names(sp)[1] <- vars[1]
    
    
    if (featkind == "petrocalcic horizon") {
      sp <- sp[(sp$featdepb - sp$featdept) >= 10, ]
    }
    
    # need to add more logic to capture when < 10cm is cemented 
    if (featkind == "calcic horizon") {
      sp <- sp[(sp$featdepb - sp$featdept) >= 15, ]
    }
    
    
    # needs additional criteria to get a variable depth or use Andrew's function
    if (featkind == "mollic epipedon") {
      sp <- sp[(sp$featdepb - sp$featdept) >= 18, ]
    }
  
  } else sp <- NULL 
  
  return(sp)
}

