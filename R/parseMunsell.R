# split standard Munsell hue into character | numeric parts
# function is vectorized

# additional ideas from munsellinterpol::HueNumberFromString

# output:
# hue.numeric hue.character
#     2.3            YR
.parseMunsellHue <- function(hue) {
  
  # NA not permitted, convert to ''
  hue <- ifelse(is.na(hue), '', hue)
  
  # extract numeric part from hue
  # danger! empty strings will result in an empty list element
  nm.part <- strsplit(hue, split='[^0-9.]+', )
  
  # replace empty list elements with ''
  nm.part[which(sapply(nm.part, length) < 1)] <- ''
  
  # the numeric part is always the first returned match
  # solves bogus rows associated with invalid hues
  # # https://github.com/ncss-tech/aqp/issues/66
  nm.part <- lapply(nm.part, '[', 1)
  
  # convert to vector
  hue.numeric <- unlist(nm.part)
  
  # extract character part from hue
  hue.character <- vector(mode='character', length = length(hue))
  for(i in seq_along(hue)){
    # check for NA
    if(is.na(hue.numeric[i])) {
      hue.character[i] <- NA
      next
    }
    # check for empty string
    if(hue.numeric[i] == '') {
      hue.character[i] <- NA
      next
    }
    # otherwise continue processing
    # remove numeric portion
    hue.character[i] <- gsub(hue.numeric[i], '', hue[i], fixed = TRUE)
  }
  
  # convert numeric part to numbers
  hue.numeric <- as.numeric(hue.numeric)
  
  # strip white space form character portion
  hue.character <- trimws(hue.character, which = 'both')
  
  res <- data.frame(hue.numeric, hue.character, stringsAsFactors = FALSE)
  return(res)
}




#' @title Parse Munsell Color Notation
#' 
#' @description Split Munsell color notation into "hue", "value", and "chroma", with optional conversion to sRGB hex notation, sRGB coordinates, and CIELAB coordinates. Conversion is performed by [`munsell2rgb`].
#'
#' @param munsellColor character vector of Munsell colors (e.g. `c('10YR 3/4', '5YR 4/6')`)
#' @param convertColors logical, convert colors to sRGB hex notation, sRGB coordinates, CIELAB coordinates
#' @param delim optional, specify the type of delimiter used between value and chroma parts of the Munsell code. By default ":", ",:, "'", and "/" are supported.
#' @param ... additional arguments to [`munsell2rgb`]
#'
#' @return a `data.frame` object
#' 
#' @importFrom stringr str_extract_all str_length str_trim
#' @export
#'
#' @author P. Roudier and D.E. Beaudette
#'
#' @examples
#' 
#' # just sRGB
#' parseMunsell("10YR 3/5", return_triplets = TRUE)
#' 
#' # sRGB + CIELAB (D65 illuminant)
#' parseMunsell("10YR 3/5", return_triplets = TRUE, returnLAB = TRUE)
#' 
#' # CIELAB only
#' parseMunsell("10YR 3/5", return_triplets = FALSE, returnLAB = TRUE)
#' 
#' # neutral hue
#' # note chroma encoded as '0'
#' parseMunsell('N 3/', convertColors = FALSE)
#' 
parseMunsell <- function(munsellColor, convertColors = TRUE, delim = NA, ...) {
  
  # now fully vectorized, no wrangling of tiny data.frames
  # https://github.com/ncss-tech/aqp/issues/230
  
  # empty result set for convertColors = FALSE
  empty <- data.frame(
    hue = NA_character_, 
    value = NA_real_, 
    chroma = NA_real_, 
    stringsAsFactors = FALSE
  )
  
  # sanity check: all NA
  if(all(is.na(munsellColor)) | all(is.null(munsellColor)) | all(munsellColor == '')) {
    # return empty data.frame for each entry
    if(convertColors) {
      res <- empty[rep(1, times = length(munsellColor)), ]
      row.names(res) <- NULL
      return(res)
      
    } else {
      # return NA for each entry
      res <- rep(NA, times = length(munsellColor))
      return()
    }
  }
  
  
  # ensure colours are all upper case
  munsellColor <- toupper(munsellColor)
  
  # total number of records, used to NA-padding
  n <- length(munsellColor)
  
  # Munsell notation validation
  
  # If the very first character of the Munsell string is not numeric OR 'N'
  not.numeric.idx <- grep('^[0-9|N]', munsellColor, invert = TRUE)
  if(length(not.numeric.idx) > 0) {
    # can't do anything with this value
    munsellColor[not.numeric.idx] <- NA
  }
  
  # index non-NA values
  not.na.idx <- which(!is.na(munsellColor))
  mn <- munsellColor[not.na.idx]
  
  
  ## split pieces
  
  # Extract hue number
  hue_number <- str_trim(sub("[A-Z].*", "", mn), side = "both")
  remaining <- substr(mn, str_length(hue_number) + 1, str_length(mn))
  hue_letter <- str_trim(sub("[0-9].*", "", remaining), side = "both") 
  remaining <- substr(remaining, str_length(hue_letter) + 1, str_length(remaining))
  remaining <- str_trim(remaining, side = "both")
  
  # re-constitute hue
  hue <- paste0(hue_number, hue_letter)
  
  # list, of typically 2-element vectors
  if (is.na(delim)) {
    value_chroma <- strsplit(remaining, "[:,'/_]")
  } else {
    value_chroma <- strsplit(remaining, delim)
  }
  
  # attempt to extract value and chroma
  value <- sapply(value_chroma, '[', 1)
  chroma <- sapply(value_chroma, '[', 2)
  
  # clean any remaining whitespace
  value <- str_trim(value, side = "both")
  chroma <- str_trim(chroma, side = "both")
  
  # convert NA chroma -> 0 for N hues
  chroma <- ifelse(is.na(chroma) & hue == 'N', 0, chroma)
  
  # init empty data.frame
  res <- empty[rep(1, times = length(munsellColor)), ]
  row.names(res) <- NULL
  
  # insert non-NA values
  # always character
  res$hue[not.na.idx] <- hue
  # always numeric
  res$value[not.na.idx] <- as.numeric(value)
  # always numeric
  res$chroma[not.na.idx] <- as.numeric(chroma)
  
  # parsed into columns within a data.frame, but not converted to colors
  if(convertColors == FALSE) {
    return(res)
  }
  
  # otherwise convert using NA-padded data.frame
  res <- munsell2rgb(res$hue, res$value, res$chroma, ...)
  
  return(res)
}
