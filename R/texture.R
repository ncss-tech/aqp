#' Textural conversions
#'
#' These functions consist of several conversions between sand, silt and clay
#' to texture class and visa versa, textural modifiers to rock fragments, and
#' grain size composition to the family particle size class.
#'
#' These functions are intended to estimate missing values or check existing
#' values. The \code{ssc_to_texcl()} function uses the same logic as the
#' particle size estimator calculation in NASIS to classify sand and clay into
#' texture class. The results are stored in \code{soiltexture} and used by
#' \code{texcl_to_ssc()} as a lookup table to convert texture class to sand,
#' silt and clay. The function \code{texcl_to_ssc()} replicates the
#' functionality described by Levi (2017).
#'
#' When \code{sample = TRUE}, the results can be used to estimate within-class,
#' marginal distributions of sand, silt, and clay fractions. It is recommended
#' that at least 10 samples be drawn for reasonable estimates.
#'
#' The function \code{texture_to_texmod()} parses texture (e.g. GR-CL) to extract the texmod values from it in the scenario where it is missing from
#' texmod column. If multiple texmod values are present (for example in the 
#' case of stratified textures) and \code{duplicates = "combine"} they will be combined in the output (e.g. GR & CBV). Otherwise if \code{duplicates = "max"} 
#' the texmod with the highest rock fragment (e.g. CBV) will be returned.
#'
#' Unlike the other functions, \code{texture_to_taxpartsize()} is intended to
#' be computed on weighted averages within the family particle size control
#' section. Also recall from the criteria that carbonate clay should be
#' subtracted from clay content and added to silt content. Similarly, if the
#' percent of very fine sand is known it should be subtracted from the sand,
#' and added to the silt content. Unlike the other functions,
#' \code{texture_to_taxpartsize()} is intended to be computed on weighted
#' averages within the family particle size control section. Also recall from
#' the criteria that carbonate clay should be subtracted from clay content and
#' added to silt content. Similarly, if the percent of very fine sand is known
#' it should be subtracted from the sand, and added to the silt content.

#' @param texcl vector of texture classes than conform to the USDA code
#' conventions (e.g. c|C, sil|SIL, sl|SL, cos|COS)
#' @param texmod vector of textural modifiers that conform to the USDA code
#' convenctions (e.g. gr|GR, grv|GRV)
#' @param lieutex vector of in lieu of texture terms that conform to the USDA
#' code convenctions (e.g. gr|GR, pg|PG), only used when fragments or artifacts
#' are > 90 percent by volume (default: NULL))
#' @param texture vector of combinations of texcl, texmod, and lieutex (e.g. CL, GR-CL, CBV-S, GR)
#'
#' @param clay vector of clay percentages
#' @param sand vector of sand percentages
#' @param fragvoltot vector of rock fragment percentages
#'
#' @param as.is logical: should character vectors be converted to factors?
#' (default: TRUE)
#'
#' @param droplevels logical: indicating whether to drop unused levels in
#' factors. This is useful when the results have a large number of unused
#' classes, which can waste space in tables and figures.

#' @param sample logical: should ssc be random sampled from the lookup table?
#' (default: FALSE)
#' 
#' @param duplicates character: specifying how multiple values should be handled, options are `"combined"` (e.g. 'GR & GRV) or `"max"`(e.g. 'GRV') 
#'
#' @return - `texcl_to_ssc`: A `data.frame` containing columns `"sand"`,`"silt"`, `"clay"`
#'
#' @author Stephen Roecker
#' 
#' @references Matthew R. Levi, Modified Centroid for Estimating Sand, Silt, and Clay from Soil Texture Class, Soil Science Society of America Journal, 2017, 81(3):578-588, ISSN 1435-0661, \doi{10.2136/sssaj2016.09.0301}.
#' 
#' @rdname texture
#' @keywords manip
#' @examples
#' \donttest{
#' # example of ssc_to_texcl()
#' tex <- expand.grid(sand = 0:100, clay = 0:100)
#' tex <- subset(tex, (sand + clay) < 101)
#' tex$texcl <- ssc_to_texcl(sand = tex$sand, clay = tex$clay)
#' head(tex)
#'
#' # example of texcl_to_ssc(texcl)
#' texcl <- c("cos", "s", "fs", "vfs", "lcos", "ls",
#'           "lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l",
#'           "sil", "si", "scl", "cl", "sicl", "sc", "sic", "c"
#'           )
#' test <- texcl_to_ssc(texcl)
#' head(test <- cbind(texcl, test), 10)
#'
#'
#' # example of texcl_to_ssc(texcl, clay)
#' data(soiltexture)
#' st <- soiltexture$values
#' idx <- sample(1:length(st$texcl), 10)
#' st <- st[idx, ]
#' ssc <- texcl_to_ssc(texcl = st$texcl)
#' head(cbind(texcl = st$texcl, clay = ssc$clay))
#'
#'
#' # example of texmod_to_fragvoltol()
#' frags <- c("gr", "grv", "grx", "pgr", "pgrv", "pgrx")
#' head(texmod_to_fragvoltot(frags))
#'
#'
#' # example of texture_to_taxpartsize()
#' tex <- data.frame(texcl = c("c", "cl", "l", "ls", "s"),
#'                   clay  = c(55, 33, 18, 6, 3),
#'                   sand  = c(20, 33, 42, 82, 93),
#'                   fragvoltot = c(35, 15, 34, 60, 91))
#' tex$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
#'                                    clay = tex$clay,
#'                                    sand = tex$sand,
#'                                    fragvoltot = tex$fragvoltot)
#' head(tex)
#'
#'
#' # example of texture_to_taxpartsize() with carbonate clay and very fine sand
#' carbclay <- rnorm(5, 2, 3)
#' vfs <- rnorm(5, 10, 3)
#' st$fpsc <- texture_to_taxpartsize(texcl = tex$texcl,
#'                                   clay = tex$clay - carbclay,
#'                                   sand = tex$sand - vfs,
#'                                   fragvoltot = tex$fragvoltot)
#' head(tex)
#'
#'
#' # example of sample = TRUE
#' texcl <- rep(c("cl", "sil", "sl"), 10)
#' ssc1 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = FALSE))
#' ssc2 <- cbind(texcl, texcl_to_ssc(texcl = texcl, sample = TRUE))
#' ssc1$sample <- FALSE
#' ssc2$sample <- TRUE
#' ssc  <- rbind(ssc1, ssc2)
#' aggregate(clay ~ sample + texcl, data = ssc, summary)
#' }
texcl_to_ssc <- function(texcl = NULL, clay = NULL, sample = FALSE) {
  # fix for R CMD check
  #  texcl_to_ssc: no visible binding for global variable ‘soiltexture’
  soiltexture <- NULL

  # clay is not NULL
  clay_not_null <- all(!is.null(clay))
  clay_is_null  <- !clay_not_null

  # standardize the inputs
  df <- data.frame(texcl = tolower(as.character(texcl)),
                   stringsAsFactors = FALSE
                   )
  if (clay_not_null) {
    df$clay <- as.integer(round(clay))
  }
  df$rn <- row.names(df)


  load(system.file("data/soiltexture.rda", package="aqp")[1])


  # check for texcl that don't match
  idx <- ! df$texcl %in% unique(soiltexture$averages$texcl)
  if (any(idx)) {
    warning("not all the user supplied texcl values match the lookup table")
  }


  # check clay values within texcl
  if (clay_not_null) {
    clay_not_na <- !is.na(df$clay)

    idx <- paste(df$texcl[clay_not_na], df$clay[clay_not_na]) %in% paste(soiltexture$values$texcl, soiltexture$values$clay)

    if (any(!idx)) {
      warning("not all the user supplied clay values fall within the texcl")
    }
  }


  # check clay ranges 0-100
  idx <- clay_not_null & any(clay < 0, na.rm = TRUE) & any(clay > 100, na.rm = TRUE)
  if (idx) {
    warning("some clay records < 0 or > 100%")
  }


  # convert fine sand classes to their generic counterparts
  df <- within(df, {
    texcl = ifelse(texcl %in% c("cos",  "fs", "vfs"),   "s",  texcl)
    texcl = ifelse(texcl %in% c("lcos", "lfs", "lvfs"), "ls", texcl)
    texcl = ifelse(texcl %in% c("cosl", "fsl", "vfsl"), "sl", texcl)
  })


  # if clay is present
  if (clay_not_null & sample == FALSE) {

    st <- aggregate(sand ~ texcl + clay, data = soiltexture$values, function(x) as.integer(round(mean(x))))
    st$silt <- 100 - st$clay - st$sand

    # some clay present (compute clay weighted averages)
    idx <- is.na(df$clay)
     if (any(idx)) {
       df_na <- merge(df[idx, c("texcl", "rn")], soiltexture$averages, by = "texcl", all.x = TRUE, sort = FALSE)[c("texcl", "clay", "rn")]
       df[idx, ] <- df_na
     }

    df <- merge(df[c("texcl", "clay", "rn")], st, by = c("texcl", "clay"), all.x = TRUE, sort = FALSE)
  } else {
    # clay missing (use average)
    df <- merge(df[c("texcl", "rn")], soiltexture$averages, by = "texcl", all.x = TRUE, sort = FALSE)
  }


  # randomly sample ssc from texcl lookup table
  if (sample == TRUE) {

    if (clay_is_null) df$clay <- NA

    split(df, df$texcl, drop = TRUE) ->.;
    lapply(., function(x) {

      st    <- soiltexture$values

      # clay present
      x$idx <- is.na(x$clay)
      x1 <- x[x$idx == FALSE, ]
      x2 <- x[x$idx == TRUE,  ]

      if (clay_not_null) {
        temp1 <- st[st$texcl == x1$texcl[1] &
                    st$clay %in% unique(x1$clay),
                    ]
        temp1 <- temp1[sample(1:nrow(temp1), size = nrow(x1), replace = TRUE), ]
        temp1 <- cbind(x1[x1$idx == FALSE, c("rn", "texcl")], temp1[c("clay", "sand")])
      } else temp1 <- NULL

      # clay missing
      temp2 <- st[st$texcl == x2$texcl[1], ]
      temp2 <- temp2[sample(1:nrow(temp2), size = nrow(x2), replace = TRUE), ]
      temp2 <- cbind(x2[x2$idx == TRUE, c("rn", "texcl")], temp2[c("clay", "sand")])

      return(rbind(temp1, temp2))
    }) ->.;
    do.call("rbind", .) -> df
    df$silt <- 100 - df$clay - df$sand
    row.names(df) <- NULL
  }


  # standardize outputs
  vars <- c("sand", "silt", "clay")
  df <- df[(order(as.integer(df$rn))), vars]
  df$rn    <- NULL
  df$texcl <- NULL

  return(df)
}

#' Convert sand, silt and clay to texture class
#'
#' @rdname texture
#' @return  - `ssc_to_texcl`: A `character` vector containing texture class
#' @export
#'
ssc_to_texcl <- function(sand = NULL, clay = NULL, as.is = FALSE, droplevels = TRUE) {
  # fix for R CMD check:
  #  ssc_to_texcl: no visible binding for global variable ‘silt’
  silt <- NULL

  # check lengths
  idx <- length(clay) != length(sand)
  if (idx) {
    stop("length of inputs do not match")
  }


  # standardize inputs
  df <- data.frame(sand = as.integer(round(sand)),
                   clay = as.integer(round(clay)),
                   stringsAsFactors = FALSE
  )
  df$silt <- 100 - df$clay - df$sand


  ## TODO: this needs some more work: sum will always be 100, but silt-by-difference may be illogical

  # # check sand, silt and clay sum to 100
  # idx <- (df$sand + df$silt + df$clay) > 100 | (df$sand + df$silt + df$clay) < 100
  # if (any(idx) & any(complete.cases(df[c("sand", "clay")]))) {
  #   warning("some records sand, silt, and clay do not sum to 100 %")
  #   }


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

  # encoding according to approximate AWC, from Red Book version 3.0
  if (as.is == FALSE) {
    df$texcl <- factor(df$texcl, levels = SoilTextureLevels(which = 'codes'), ordered = TRUE)

    if (droplevels == TRUE) {
      df$texcl <- droplevels(df$texcl)
    }
  }

  return(df$texcl)
}

#' Convert from fragment modifier to estimated total volume
#'
#' @param texmod vector of textural modifiers that conform to the USDA code
#' conventions (e.g. gr|GR, grv|GRV)
#'
#' @param lieutex vector of in lieu of texture terms that conform to the USDA
#' code conventions (e.g. gr|GR, pg|PG), only used when fragments or artifacts
#' are > 90 percent by volume (default: NULL))
#' @rdname texture
#' @return  - `texmod_to_fragvoltot`: A `numeric` vector containing total fragment volume
#' @export
texmod_to_fragvoltot <- function(texmod = NULL, lieutex = NULL) {
  # fix for R CMD check
  #  texmod_to_fragvoltot: no visible binding for global variable ‘soiltexture’
  soiltexture <- NULL

  # check
  idx <- any(!is.na(texmod) & !is.na(lieutex))
  if (idx) {
    warning("texmod and lieutex should not both be present, they are mutually exclusive, only the texmod will be returned")
  }


  # standardize inputs
  df <- data.frame(texmod = tolower(texmod), stringsAsFactors = FALSE)
  df$rn = row.names(df)


  # load lookup table
  load(system.file("data/soiltexture.rda", package="aqp")[1])


  # check for texmod and lieutex that don't match
  idx <- ! df$texmod %in% soiltexture$texmod$texmod
  if (any(idx)) {
    message("not all the texmod supplied match the lookup table")
  }

  idx <-  ! toupper(lieutex) %in% c("GR", "CB", "ST", "BY", "CN", "FL", "PG", "PCB", "PST", "PBY", "PCN", "PFL", "BR", "HMM", "MPM", "SPM", "MUCK", "PEAT", "ART", "CGM", "FGM", "ICE", "MAT", "W")
  if (all(!is.null(lieutex)) & any(idx)) {
    message("not all the texmod supplied match the lookup table")
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

#' Convert sand, silt and clay to the family particle size class
#'
#' @param texcl vector of texture classes than conform to the USDA code
#' conventions (e.g. c|C, sil|SIL, sl|SL, cos|COS)
#'
#' @param clay vector of clay percentages#'
#' @param sand vector of sand percentages
#'
#' @param fragvoltot vector of rock fragment percentages
#'
#' @return - `texture_to_taxpartsize`: a character vector containing `"taxpartsize"` classes
#' 
#' @rdname texture
#'
#' @export
#'
texture_to_taxpartsize <- function(texcl = NULL, clay = NULL, sand = NULL, fragvoltot = NULL) {

  # check lengths
  idx <- length(texcl) == length(clay) & length(clay) == length(sand) & length(sand) == length(fragvoltot)
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
  idx <- any(! df$texcl %in% SoilTextureLevels(which = 'codes'))
  if (idx) {
    warning("not all the texcl supplied match the lookup table")
  }


  # check percentages
  idx <- df$silt > 100 | df$silt < 0 | df$clay > 100 | df$clay < 0 | df$sand > 100 | df$sand < 0 | df$fragvoltot > 100 | df$fragvoltot < 0
  if (any(idx)) {
    warning("some records are > 100% or < 0%, or the calcuated silt fraction is > 100% or < 0%")
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

  idx <- df$fragvoltot >= 35
  if (any(idx)) {
    df[idx,] <- within(df[idx,], {
      fpsc[texcl %in% sandytextures] = "sandy-skeletal"
      fpsc[clay <  35]               = "loamy-skeletal"
      fpsc[clay >= 35]               = "clayey-skeletal"
      })
  }

  idx <- df$fragvoltot < 35 & df$texcl %in% sandytextures
  if (any(idx)) {
    df[idx, ]$fpsc <- "sandy"
  }

  idx <- df$fragvoltot < 35 & ! df$texcl %in% sandytextures
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

  df$fpsc <- ifelse(df$fragvoltot > 90, "fragmental", df$fpsc)

  return(df$fpsc)
}


texture_to_texmod <- function(texture, duplicates = "combine") {
  
  # test
  is_char <- is.character(texture)
  is_fact <- is.factor(texture)
  
  if (!is_char & !is_fact) stop("texture must be a character or a factor")
  
  if (is_fact) texture <- as.character(texture)
  
  
  # load lookup tables
  load(system.file("data/soiltexture.rda", package = "aqp")[1])
  texmod_df <- soiltexture$texmod
  texmod    <- texmod_df$texmod[grepl("gravel|cobbl|ston|boulder|channer|flag", texmod_df$texmod_label)]
  
  # lieutex <- metadata[metadata$ColumnPhysicalName == "lieutex", ]$ChoiceName
  
  tex   <- tolower(texture)
  tex_l <- strsplit(tex, "-| |_|\\/|&|,|;|\\.|\\*|\\+")
  
  
  # find which records have a texmod
  idx_mod  <- which(sapply(tex_l, function(x) any(x %in% texmod)))
  # idx_lieu <- which(sapply(tex_l, function(x) any(x %in% lieutex)))
  
  
  # create a logical matrix of texmod presence/absence
  tex_m  <- lapply(tex_l[idx_mod], function(x) texmod %in% x)
  tex_df <- as.data.frame(do.call("rbind", tex_m))
  names(tex_df) <- texmod
  
  # slightly slower simpler(/better?) alternative
  # not better, this mistakenly matches words entered into this field
  # I would rather have false negatives than false positives
  # tex_m  <- sapply(texmod, function(x) grepl(x, tex))
  
  
  # duplicates ----
  ## combine texmod (if multiple exist)
  if (duplicates == "combine") {
    texmod_parse <- apply(tex_df, 1, function(x) {
      paste0(names(x)[which(x)], collapse = " & ")
    })
  }
  
  
  ## find texmod with the highest rock fragment content (if multiple exist)
  if (duplicates == "max") {
    tex_m <- sapply(texmod, function(x) {
      vals <- suppressMessages(texmod_to_fragvoltot(x))
      rf   <- ifelse(tex_df[x] == TRUE, vals$fragvoltot_r, 0)
      })
    idx_col <- max.col(tex_m)
    texmod_parse <- names(tex_df)[idx_col]
  }
  
  
  # results ----
  n  <- length(texture)
  df <- data.frame(texmod = rep(NA, n)) #, lieutex = rep(NA, n))
  df[idx_mod, "texmod"] <- texmod_parse
  
  return(df$texmod)
}


# add feature to uncode
# texture_to_texcl <- function(texclLabel, droplevels = FALSE 
#                              # , stringsAsFactors = FALSE
#                              ) {
#   
#   # standardize inputs
#   texclLabel <- tolower(texclLabel)
#   
#   # load lookup table
#   load(system.file("data/soiltexture.rda", package = "aqp")[1])
#   sub <- soiltexture$averages
#   
#   # check
#   idx <- ! texclLabel %in% sub$texcl_label
#   if (any(idx)) {
#     warning("not all the user supplied texture values match the lookup table")
#   }
#   
#   # convert
#   texcl <- factor(texclLabel, levels = tolower(sub$texcl_label), label = sub$texcl, ordered = TRUE)
#   
#   if (droplevels == TRUE) {
#     texcl <- droplevels(texcl)
#   }
#   
#   # if (stringsAsFactors == TRUE) {
#   #   texcl <- as.character(texcl)
#   # }
#   
#   return(texcl)
# }

