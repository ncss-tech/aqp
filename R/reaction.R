.reactionclass <- function() {
  data.frame(
    DescriptiveTerm = c(
      "Ultra Acid",
      "Extremely Acid",
      "Very Strongly Acid",
      "Strongly Acid",
      "Moderately Acid",
      "Slightly Acid",
      "Neutral",
      "Slightly Alkaline",
      "Moderately Alkaline",
      "Strongly Alkaline",
      "Very Strongly Alkaline"
    ),
    pH_low = c(0, 3.55, 4.45, 5.05, 5.55, 6.05, 6.55, 7.35, 7.85, 8.45, 9.05),
    pH_high = c(3.55, 4.45, 5.05, 5.55, 6.05, 6.55, 7.35, 7.85, 8.45, 9.05, 14)
  )
}

.reactionclass2 <-  function() {
  # rda can be built using reactionclass <- .reactionclass(); save(reactionclass, file = "data/reactionclass.rda")
  reactionclass <- NULL
  load(system.file("data", "reactionclass.rda", package = "aqp"))
  reactionclass
}

.phclasses1 <- function(halfclass = FALSE) {
  lut1 <- .reactionclass()
  if (halfclass) {
    lut2 <- lut1
    lut2$pH_high <- lut2$pH_low + ((lut2$pH_high - lut2$pH_low) / 2)
    lut1$pH_low <- lut2$pH_high
    lut <- rbind(data.frame(id = "Low", lut2), data.frame(id = "High", lut1))
    lut <- lut[order(lut$pH_low),]
  } else {
    lut1$id <- ""
    lut <- lut1
  }
  lut
}

.phclass <- function(x, halfclass = FALSE) {
  lut1 <- .phclasses1(halfclass = halfclass)
  idx <- findInterval(x, lut1[["pH_low"]])
  trimws(paste(lut1[["id"]][idx], lut1[["DescriptiveTerm"]][idx]))
}

.phrange <- function(x, halfclass = FALSE) {
  if (any(grepl('high|low', x, ignore.case = TRUE))) halfclass <- TRUE
  lut1 <- .phclasses1(halfclass = halfclass)
  res <- lut1[trimws(tolower(paste(lut1[["id"]], lut1[["DescriptiveTerm"]]))) %in% tolower(x), c("pH_low", "pH_high")]
  res2 <- res[0,][1,]
  res2$pH_low <- min(res$pH_low)
  res2$pH_high <- max(res$pH_high)
  rownames(res2) <- NULL
  res2
}

#' Convert pH to/from Reaction Classes
#'
#' @param x input pH values (numeric; `ph_to_rxnclass()`) or reaction classes (character; `rxnclass_to_ph()`)
#' @param halfclass Split the standard classes in half for higher resolution? Default: `FALSE`
#' @return `ph_to_rxnclass()`: a vector of reaction classes
#' @export
#' @rdname reaction
#' @examples
#' ph_to_rxnclass(6.2)
ph_to_rxnclass <- function(x, 
                           halfclass = FALSE,
                           as.is = FALSE,
                           droplevels = TRUE) {
  .phclass(x, halfclass = halfclass)
}

#' @param digits Number of digits after decimal place; Default: `2`. Used only for `rxnclass_to_ph()`
#' @param simplify Simplify list result to numeric vector when length of result is 1? Default: `TRUE`
#' @export
#' @return `rxnclass_to_ph()`: a list of high/low values of reaction class 1:1 with input; if simplify=TRUE and result is length 1, a numeric vector containing low and high values.
#' @rdname reaction
#' @examples 
#' rxnclass_to_ph("slightly acid")
rxnclass_to_ph <- function(x, halfclass = FALSE, digits = 2, simplify = TRUE) {
  if (!is.list(x))
    x <- list(x)
  res <- lapply(x, function(y) round(.phrange(y, halfclass = halfclass), digits = digits))
  if (length(res) == 1 && simplify) {
    return(res[[1]]) 
  }
  res
}
