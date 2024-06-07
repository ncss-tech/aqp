
## TODO: add verbose = FALSE argument cut down on chatter


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
#'   + **texcl:** soil texture class (USDA) column name
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
#' @author Stephen Roecker
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
#' sp1$texcl = gsub("gr|grv|cbv", "", sp1$texture)
#' df <- allocate(object = sp1, pedonid = "id", hzname = "name", 
#'                hzdept = "top", hzdepb = "bottom", texcl = "texcl", 
#'                to = "ST Diagnostic Features"
#' )
#' aggregate(featdept ~ id, data = df, summary)
#' 
allocate <- function(..., to = c("FAO Salt Severity", "FAO Black Soil", "ST Diagnostic Features"), droplevels = FALSE) {
  
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
      # texcl = "texcl"
      # hz_pat = ""
      # tex_pat = "br"
      # featkind = "argillic horizon"
      
      featkind <- c("lithic contact", "paralithic contact", "densic contact", "petrocalcic horizon", "calcic horizon", "secondary carbonates", "mollic epipedon") #, "reduced matrix")
      
      a <- lapply(featkind, function(x) {
        # a <- .guess_df(pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texcl = "texcl", featkind = "lithic contact")
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
.rank_salts <- function(EC = NULL, pH = NULL, ESP = NULL, SAR = NULL, system = "FAO Salt Severity", droplevels = FALSE) {
  
  # EC = 1; pH = 3; ESP = 50
  l <- list(EC = EC, pH = pH, ESP = ESP, SAR = SAR)
  
  # tests ----
  # ESP vs SAR
  if (!is.null(SAR)) {
    warning("SAR will be converted to ESP via Richards (1958) conversion formula.")
  }
  if (!is.null(ESP) & !is.null(SAR)) {
    warning("Both ESP & SAR are present, SAR will only be used where ESP is missing.")
  }
  # minimum dataset
  if (any(sapply(l[c(1, 3)], is.null)) & any(sapply(l[c(1, 4)], is.null))) {
    warning("the minimum dataset of soil properites for allocating to the Salt Severity classes are: EC (aka Electrial Conductivity), and ESP (aka Exchangable Sodium Percentage) or SAR (aka Sodium Adsorption Ratio)")
  }
  # pH rule
  if (any(!complete.cases(EC, ESP)) | any(!complete.cases(EC, SAR))) {
    warning("pH is used in where both ESP and SAR are missing")
  }
  # length
  n <- sapply(l, length)
  if (! all(max(n) == n[1:3]) & ! all(max(n) == n[c(1:2, 4)])) {
    stop("all arguments must have the same length")
  }
  
  
  # levels ----
  fao_lev <- c(
    c("none", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
    )
  
  
  ## TODO: why?
  sc <- rep("none", times = length(EC))
  
  ## TODO: consider separate saline / sodic classification
  # estimate ESP from SAR ----
  if (is.null(ESP)) ESP <- rep(NA_real_, times = length(EC))
  if (is.null(SAR)) SAR <- rep(NA_real_, times = length(EC))
  
  .esp <- function(SAR) {
    (100 * (-0.0126 + 0.01475 * SAR)) / 
      (1   + (-0.0126 + 0.01475 * SAR))
  }
  ESPx <- .esp(SAR)
  ESP <- ifelse(is.na(ESP) & !is.na(SAR), ESPx, ESP)
  
  
  # rank ----
  # saline soils
  sc <- ifelse(EC > -1 & (ESP <= 15 | (is.na(ESP) & pH <= 8.2)), # & EC > 4 & pH <= 8.5, 
               as.character(
                 cut(EC,
                     breaks = c(-1, 0.75, 2, 4, 8, 15, 1500), 
                     labels = fao_lev[1:6],
                     right  = FALSE
                     )),
               sc
               )
  # sodic soils
  # ESP
  sc <- ifelse(EC <= 4 & ESP > 15,  # | pH > 8.2
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
  sc <- ifelse(EC > 4 & (ESP > 15 | (is.na(ESP) & pH > 8.2)), "saline-sodic", sc)
  
  
  # convert to factor
  sc <- factor(sc, levels = c(fao_lev[6:1], fao_lev[8:11], "saline-sodic"))

  
  # droplevels
  if (droplevels == TRUE) {
    sc <- droplevels(sc)
  }
  
  return(sc)
}



.codify <- function(x, system = "salt severity", droplevels = FALSE) {

  if (system == "salt severity") {

    .codify_salt_severity(x, droplevels = droplevels)
    }
  }


.codify_salt_severity <- function(x, droplevels = FALSE) {
  
  # set levels
  fao_lev <- c(
    c("none", "slightly saline", "moderately saline", "strongly saline", "very strongly saline", "extremely saline"),
    c("none", "slightly sodic", "moderately sodic", "strongly sodic", "very strongly sodic")
  )
  
  # test
  if (!is.integer(x)) stop("x is not an integer")
  if (!all(unique(x) %in% c(1:11, NA))) warning("some x values do not match the lookup table")

  sc <- factor(
    x, 
    levels = 1:11, 
    labels = c(fao_lev[6:1], fao_lev[8:11], "saline-sodic")
    )
  
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
  df_bs  <- hz_segment(df_bs, intervals = c(0, 25), depthcols = c("hztop", "hzbot"))
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
.guess_df <- function(object = NULL, pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texcl = "texcl", rupresblkcem = "rupresblkcem", m_value = "m_value", d_value = "d_value", m_chroma = "m_chroma", BS = "BS", OC = "OC", n_value = "n_value", featkind = NULL) {
  
  # pedonid = "peiid"; hzname = "hzname"; hzdept = "hzdept"; hzdepb = "hzdepb"; texcl = "texcl"; hz_pat = ""; tex_pat = "br"; featkind = "mollic epipedon"; rupresblkcem = "rupresblkcem"; m_value = "m_value"; d_value = "d_value"; m_chroma = "m_chroma"; BS = NA; OC = NA; n_value = "n_value"
  # object = sp1; pedonid = "id"; hzname = "name"; hzdept = "top"; hzdepb = "bot"; texcl = "texture"
  # vars <- list(pedonid = "peiid", hzname = "hzname", hzdept = "hzdept", hzdepb = "hzdepb", texcl = "texture", rupresblkcem = "rupresblkcem", m_value = "m_value", d_value = "d_value", m_chroma = "m_chroma", OC = "OC", BS = "BS", n_value = "n_value")
  
  # standardize inputs
  vars <- list(pedonid = pedonid, hzname = hzname, hzdept = hzdept, hzdepb = hzdepb, texcl = texcl, rupresblkcem = rupresblkcem, m_value = m_value, d_value = d_value, m_chroma = m_chroma, OC = OC, BS = BS, n_value = n_value)
  
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
    warning("the minimum dataset includes: pedonid, hzdept, hzdepb, and hzname; if texcl or rupreblkcem are missing the resulting diagnostic features are inferred from the available information")
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
  df$texcl      <- tolower(df$texcl)
  df$rupresblkcem <- tolower(df$rupresblkcem)
  
  # match pattern
  
  # lithic contact ----
  if (featkind == "lithic contact") {
    message(paste("guessing", featkind))
    idx_hzn <-  grepl("R|Dr", df$hzname) & !grepl("\\/", df$hzname)
    idx_tex <- !grepl("Cr|CR", df$hzname) & (df$texcl %in% c("br", "wb", "uwb") | is.na(df$texcl))
    
    lev <- c("strongly cemented", "very strongly cemented", "indurated", "strongly", "extremely strongly", "H", "moderately coherent", "strongly coherent", "very strongly coherent") 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have R horizons that do not meeting the texcl or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # paralithic contact ----
  if (featkind == "paralithic contact") {
    message(paste("guessing", featkind))
    
    idx_hzn <-  grepl("Cr|CR", df$hzname)
    idx_tex <- !grepl("R|Dr", df$hzname) & (df$texcl %in% c("br", "wb", "uwb") | is.na(df$texcl))
    
    lev <- c("extremely weakly cememented", "very weakly cemented", "weakly cemented", "moderately cemented", "weakly", "moderately", "S") 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Cr horizons that do not meeting the texcl or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }

    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # densic contact ----
  if (featkind == "densic contact") {
    message(paste("guessing", featkind))
    idx <- grepl("d$|D$|d[1:9]|D[1:9]", df$hzname)
    df$featkind <- ifelse(idx == TRUE, featkind, NA)
  }
  
  
  # petrocalcic horizon ----
  if (featkind == "petrocalcic horizon") {
    message(paste("guessing", featkind))
    
    idx_hzn  <- grepl("kkm|kkqm", df$hzname)
    idx_tex  <- ((grepl("cem", df$texcl) & !grepl("-br", df$texcl)) | is.na(df$texcl))
    
    lev <- "noncemented" 
    idx_cem <- !df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Bkkm|Bkkqm horizons that do not meeting the texcl or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # calcic horizon ----
  if (featkind == "calcic horizon") {
    message(paste("guessing", featkind))
    
    idx_hzn <-  grepl("kk$|kk[1:9]|kkq$|kkq[1:9]|kkb$|kkb[1:9]", df$hzname)
    idx_tex <- (!grepl("cem-", df$texcl) | is.na(df$texcl))
    
    lev <- "noncemented" 
    idx_cem <- df$rupresblkcem %in% lev | is.na(df$rupresblkcem)
    
    # error
    idx_err <- idx_hzn  & (!idx_tex | !idx_cem) 
    if (any(idx_err)) {
      message(paste("the following pedonid have Bkk horizons that do not meeting the texcl or rupture resistance cementation criteria and will be excluded: ", paste0(df$pedonid[idx_err], collapse = ", ")))
    }
    
    df$featkind <- ifelse(idx_hzn & idx_tex & idx_cem, featkind, NA)
  }
  
  
  # secondary carbonates ----
  if (featkind == "secondary carbonates") {
    message(paste("guessing", featkind))
    
    idx_hzn <- grepl("k", df$hzname) & !grepl("kk", df$hzname)
    
    df$featkind <- ifelse(idx_hzn, featkind, NA)
  }
  
  
  # mollic epipedon ----
   if (featkind == "mollic epipedon") {
     message(paste("guessing", featkind))
     
     idx_hzn <- !grepl("O|Ao|R|W|M|C|\\/", df$hzname)
     idx_tex <- df$texcl %in% levels(SoilTextureLevels()) | is.na(df$texcl)
     # need to add structure to fetchNASIS
     idx_col <- 
       (df$m_value  <= 3 | is.na(df$m_value))  & 
       (df$d_value  <= 5 | is.na(df$d_value))  &
        df$m_chroma <= 3                        &
       (!is.na(df$m_value) | !is.na(df$d_value))
     idx_bs  <- (df$BS >= 50 | is.na(df$BS)) # & (df$ph1to1 > 6 | is.na(df$ph1to1))
     idx_oc  <- 
       ((df$OC >= 2.5 | is.na(df$OC)) & (df$m_value %in% 4:5 | is.na(df$m_value))) |
       ((df$OC >= 0.6 | is.na(df$OC)) & (df$m_value < 4      | is.na(df$m_value)))
     idx_nv <- (df$n_value < 0.7 | is.na(df$n_value))
     
     df$featkind <- ifelse(idx_hzn & idx_tex & idx_col & idx_bs & idx_oc & idx_nv, featkind, NA)
   }
  
  
  # subset features
  idx <- "featkind" %in% names(df)
  if (idx) {
    
    df_sub <- df[!is.na(df$featkind), ]
    
    # aggregate depths ----
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


#' @title Allocate Particle Size Class for the Control Section.
#' 
#' @description This function aggregates information in the horizon table and allocates it to the particle size class for the control section.
#'
#' @param x a \code{data.frame} containing the original horizon table.
#' @param y a \code{data.frame} containing the particle size control section depths for each idcol.
#' @param taxpartsize \code{character} column name for taxonomic family particle size class.
#' @param clay \code{character} column name for clay percent.
# #' @param frags \code{character} column name for total rock fragments percent.
#' @param idcol character: column name of the pedon ID within the object.
#' @param depthcols a character vector of length 2 specifying the names of the horizon depths (e.g. `c("top", "bottom")`).
#' 
#' 
#' @details
#' This function differs from \code{\link{texture_to_taxpartsize}} in that is aggregates the results of \code{\link{texture_to_taxpartsize}}, and accounts for strongly contrasting particle size classes.
#'  
#'
#' @return A \code{data.frame} object containing the original idcol, the aggregated particle size control section allocation, and an aniso column to indicate more than one contrasting class.
#' 
#' @author Stephen Roecker
#' 
#' @seealso [texture_to_taxpartsize()], [lookup_PSCS()]
#' 
#' @export

#' @examples
#' 
#' h <- data.frame(
#' id = 1,
#' hzname = c("A", "BA", "Bw", "BC", "C"),
#' top    = c( 0, 10, 45, 60,  90),
#' bottom = c(10, 45, 60, 90, 150),
#' clay   = c(15, 16, 45, 20,  10),
#' sand   = c(10, 35, 40, 50,  90),
#' frags  = c( 0,  5, 10, 38,  40)
#' )
#'
#' h <- cbind(
#' h, 
#' texcl = ssc_to_texcl(clay = h$clay, sand = h$sand)
#' )
#' 
#' pscs <- data.frame(
#' id = 1, 
#' top = 25, 
#' bottom = 100
#' )
#' 
#' h <- cbind(
#' h, 
#' taxpartsize = texture_to_taxpartsize(
#' texcl = h$texcl,
#' clay  = h$clay, 
#' sand  = h$sand,
#' fragvoltot = h$frags
#' ))
#' 
#' depths(h) <- id ~ top + bottom
#' 
#' pscs <- data.frame(id = h$id, rbind(estimatePSCS(h)))
#' names(pscs)[2:3] <- c("top", "bottom")
#' 
#' hz_to_taxpartsize(horizons(h), pscs)
#'  
#' 
hz_to_taxpartsize <- function(x, y, taxpartsize = "taxpartsize", clay = "clay", idcol = "id", depthcols = c("top", "bottom")) {
  # need to incorporate fine sand for special cases of strongly contrasting classes and rock fragments (?)
  # frags = "frags", 
  
  # strongly contrasting
  
  x$rn <- 1:nrow(x)
  # xy <- hz_intersect(x, y, idcol = idcol, depthcols = depthcols)
  # x_sub <- x[x$rn %in% xy$rn, ]
  
  
  # check segment_id ----
  ## if it exists, overwrite it
  x_nm <- names(x)
  y_nm <- names(y)
  if (any(x_nm == "segment_id") | any(y_nm == "segment_id")) {
    x[x_nm == "segment_id"] <- NULL
    y[y_nm == "segment_id"] <- NULL
  }
  
  
  # check dissolve_id ----
  ## if it exists, overwrite it
  x_nm <- names(x)
  y_nm <- names(y)
  if (any(x_nm == "dissolve_id") | any(y_nm == "dissolve_id")) {
    x[x_nm == "dissolve_id"] <- NULL
    y[y_nm == "dissolve_id"] <- NULL
  }
  
  
  # standardize inputs ----
  vars <- c(idcol, depthcols, clay, taxpartsize)
  x <- x[vars]
  x_std <- .standardize_inputs(x, idcol = idcol, depthcols = depthcols, clay = clay, taxpartsize = taxpartsize)
  x <- x_std$x; x_conv <- x_std$x_conversion
  x_std <- NULL
  
  y <- y[c(idcol, depthcols)]
  y <- .standardize_inputs(y, idcol = idcol, depthcols = depthcols)$x
  
  
  # dissolve on pscs ----
  # calculate non-trimmed horizon thickness
  x_dis <- x |>
    hz_dissolve(by = "taxpartsize", idcol = "idcol", depthcols = c("top", "bot")) |>
    transform(thk_o = bot - top)
  
  
  # trim depths ----
  # calculate trimmed horizon thickness
  xy_dis <- x_dis |>
    hz_intersect(y, idcol = "idcol", depthcols = c("top", "bot")) |>
    transform(thk_t = bot - top)
  
  
  # rejoin dissolved pscs to the original horizon table ----
  xy <- hz_intersect(x, xy_dis, idcol = "idcol", depthcols = c("top", "bot")) |> suppressWarnings()
  x_dis  <- NULL
  xy_dis <- NULL
  

  # aggregate clay values within dissolved pscs ----
  top       <- NULL
  bot       <- NULL
  thk_o     <- NULL
  thk_t     <- NULL
  clay_wt   <- NULL
  # sandvf_wt <- NULL
  
  xy_agg <- data.table::as.data.table(xy)[,
    list(
      top       = min(top,                          na.rm = TRUE),
      bot       = max(bot,                          na.rm = TRUE),
      clay_wt   = weighted.mean(clay,   w = thk_t,  na.rm = TRUE),
      # sandvf_wt = weighted.mean(sandvf, w = thk_t,  na.rm = TRUE),
      # need to impute frags
      # frag_wt = weighted.mean(total_frags_pct_nopf, w = thk_t), na.rm = TRUE,
      thk_o   = sum(thk_o, na.rm = TRUE),
      thk_t   = sum(thk_t, na.rm = TRUE)
    ), by = c("idcol", "taxpartsize", "dissolve_id")
    ]
  data.table::setorder(xy_agg, idcol, top)
  xy_agg <- as.data.frame(xy_agg)
  
  
  # find adjacent horizons ----
  xy_lag <- xy_agg |> 
    hz_lag(idcol = "idcol", depthcols = c("top", "bot"))
  
  
  # address special cases of strongly contrasting classes ----
  clay_wt_bot.1     <- NULL
  sandvf_wt_bot.1   <- NULL
  taxpartsize_bot.1 <- NULL
  
  
  # still needs special cases for very fine sand
  xy_agg <- cbind(xy_agg, xy_lag) |> 
    within({
      clay_dif = clay_wt_bot.1 - clay_wt
      sc       = paste0(taxpartsize, " over ", taxpartsize_bot.1)
      sc       = gsub(" over NA$", "", sc)
      
      sc = gsub("^fine over|^very-fine over", "clayey over", sc)
      sc = gsub("over fine$|over very-fine$", "over clayey", sc)
      sc = gsub("over fine over|over very-fine over", "over clayey over", sc)
      sc = gsub("over sandy|over sandy-skeletal", "over sandy or sandy-skeletal", sc)
      # clay over loamy
      sc = ifelse( 
        abs(clay_dif) >= 25 & sc %in% c("clayey over fine-loamy", "clayey over coarse-loamy"),
        gsub("clayey over fine-loamy|clayey over coarse-loamy", "clayey over loamy", sc),
        sc
      )
      # clay over loamy-skeletal
      sc = ifelse( 
        sc == "clayey over loamy-skeletal" & abs(clay_dif) < 25, 
        taxpartsize,
        sc
      )
      # fine-silty over clayey
      sc = ifelse( 
        sc == "fine-silty over clayey" & abs(clay_dif) < 25, 
        taxpartsize,
        sc
      )
      # loamy material contains less than 50 percent, by weight
      # need to include a vfs percent in the function arguments, which is only present in the lab data, and otherwise you typically wouldn't assume the vfs percent is high enough to qualify for these special cases
      sc = ifelse( 
        sc %in% c("coarse-loamy over sandy or sandy-skeletal", "loamy over sandy or sandy-skeletal", "loamy-skeletal over sandy or sandy-skeletal", "sandy over loamy", "sandy-skeletal over loamy"),
        taxpartsize,
        sc
      )
      idx_sc = sc %in% .pscs_sc
      # # sandy over loamy
      # sc = ifelse(
      #   sc %in% c("sandy over coarse-loamy", "sandy over fine-loamy") & taxpartsize_bot.1 %in% c("coarse-loamy", "fine-loamy") & sandvf_wt_bot.1 > 50,
      #   "sandy over loamy",
      #   sc
      #   )
      # # sandy-skeletal over loamy
      # sc = ifelse(
      #   sc %in% c("sandy-skeletal over coarse-loamy", "sandy over fine-loamy") & taxpartsize_bot.1 %in% c("coarse-loamy", "fine-loamy") & sandvf_wt_bot.1 > 50,
      #   "sandy-skeletal over loamy",
      #   sc
      # )
      # idx_sc = grepl("over", sc)
      sc = ifelse(idx_sc, sc, taxpartsize)
    })
  xy_lag <- NULL
  
  
  # find multiple strongly contrasting ps classes within the control section
  n_sc    <- NULL
  n_peiid <- NULL
  
  test <- data.table::as.data.table(xy_agg)[, list(
    n_sc    = sum(idx_sc, na.rm = TRUE), # sum(grepl(" over ", sc), na.rm = TRUE),
    n_peiid = length(idx_sc)
  ),
  by = "idcol"
  ] |>
    as.data.frame()
  
  
  # pick the sc pscs with the largest contrast or pscs with the greatest thickness
  xy_res <- xy_agg |>
    merge(test, by = "idcol", all.x = TRUE, sort = FALSE) |>
    transform(
      idx_sc = sc %in% .pscs_sc,
      # idx_sc = grepl(" over ", sc),
      idx_c_ov_l = sc %in% c("clayey over fine-loamy")
    )
  
  xy_res <- data.table::as.data.table(xy_res)[, list(
    pscs1 = sc[n_sc == 0 & n_peiid == 1],
    pscs2 = sc[n_sc == 1 & n_peiid  > 1 & idx_sc],
    pscs3 = sc[which.max(thk_t[n_sc == 0 & n_peiid > 1])],
    pscs4 = sc[n_sc == 1 & idx_sc],
    pscs5 = sc[which.max(abs(clay_dif[n_sc > 1 & !is.na(sc)]))],
    taxpartsizemod = ifelse(max(n_sc) > 1, "aniso", "not used")
    ),
    by =  "idcol"
    ] |>
    as.data.frame() |>
    within({
      # need to add fix for special case of sandy over loamy which requires fine sand percent
      taxpartsize = paste(pscs1, pscs3, pscs4, pscs5, sep = "")
      taxpartsize = gsub("NA", "", taxpartsize)
      pscs1 = NULL
      pscs2 = NULL
      pscs3 = NULL
      pscs4 = NULL
      pscs5 = NULL
    })
  
  
  # reset inputs
  xy_res <- .reset_inputs(xy_res, x_conv[1])
  
  
  return(xy_res)
}
