
#' @title Quickly Assemble a SoilProfileCollection
#' 
#' @description Quickly assemble a single-profile, `SoilProfileCollection` object from two possible templates. This function is a useful shortcut for creating theoretical `SoilProfileCollection` objects for testing or demonstrative purposes.
#'
#' @param x either a `list` or `character`, see Details and Examples
#' @param id character, specified when `x` is a `list`, name of ID list element
#' @param d character, specified when `x` is a `list`, name of depths list element
#' @param n character, specified when `x` is a `list`, name of horizon name list element
#' @param m character, specified when `x` is a `list`, name of list element containing Munsell color notation
#' @param interval, numeric, typically an integer and only specified when using character templates in mode 2. See Details.
#'
#' @return `SoilProfileCollection` object
#' @export
#' 
#' @details The list template for a single SPC allows for full specification of ID, horizon designation, bottom depths, and an arbitrary number of horizon-level attributes. A compact notation is used for profile ID (single value) and horizon depths (bottom depths, assuming datum of 0). Horizon designation and additional data (e.g. clay content) are specified as vectors all of equal length, matching the number of horizons in the profile.
#' 
#' The character template can be provided in one of several formats: 
#'   1. 'A-Bt1-Bt2-Bt3-Cr-R'
#'   2. 'ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC'
#'   
#'   Format 1 is interpreted as a horizon sequence delimited by '-'. Random integer thickness are assigned to horizons, and profile ID created via `digest::digest(..., algo = 'xxhash32')`. Iteration over templates in this format is automatic when `x` is a character vector of `length > 1`.
#'   
#'   Format 2 is interpreted as a horizon sequence delimited by '|'. Horizon thickness is proportional to replication of horizon designation and scaled by the `interval` argument. Profile ID is created via `digest::digest(..., algo = 'xxhash32')`. Iteration over templates in this format is automatic when `x` is a character vector of `length > 1`.
#' 
#' 
#' @examples
#' 
#' # list-based template
#' x <- list(
#' id = 'P1',
#' depths = c(25, 33, 100, 150),
#' name = c('A', 'Bw', 'Bt', 'Cr'),
#' clay = c(12, 15, 22, 25),
#' soil_color = '10YR 3/3'
#' )
#' 
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', cex.names = 1)
#' 
#' # character template, mode 1
#' # horizon thickness is generated at random (uniform [5,20])
#' x <- 'A-Bt1-Bt2-Bt3-Cr-R'
#' 
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', cex.names = 1)
#' 
#' 
#' # multiple templates
#' x <- c(
#' 'A-Bt1-Bt2-Bt3-Cr-R', 
#' 'A-C1-C2-C3-C4-Ab', 
#' 'Ap-A-A/E-E-Bhs-Cr'
#' )
#'
#' # this interface is vectorized 
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', cex.names = 1)
#' 
#' 
#' # specify profile IDs using "ID:" prefix
#' x <- c(
#' 'P1:A-Bt1-Bt2-Bt3-Cr-R',
#' 'P2:A-C1-C2-C3-C4-Ab',
#' 'P3:Ap-A-A/E-E-Bhs-Cr'
#' )
#' 
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', cex.names = 1)
#' 
#' 
#' # character template, mode 2
#' # horizon thickness is proportional to replication of 
#' # horizon designation and scaled by 'interval' argument
#' # default of 10 depth units
#' # e.g. A horizon is 3 * 10 = 30 depth units thick.
#' x <- c(
#'   'AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR'
#'   )
#' 
#' # each horizon label is '10' depth-units (default)
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', 
#'         cex.names = 1, plot.depth.axis = FALSE, 
#'         hz.depths = TRUE
#' )
#' 
#' # each horizon label is '5' depth-units
#' s <- quickSPC(x, interval = 5)
#' plotSPC(s, name.style = 'center-center', 
#'         cex.names = 1, plot.depth.axis = FALSE, 
#'         hz.depths = TRUE
#' )
#' 
#' # optionally specify some / all profile IDs with "ID:" prefix
#' x <- c(
#'   'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'P2:ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR'
#'   )
#' 
#' s <- quickSPC(x)
#' plotSPC(s, name.style = 'center-center', 
#'         cex.names = 1, plot.depth.axis = FALSE, 
#'         hz.depths = TRUE
#' )


## TODO: 
# * add vectorization for list-based template

quickSPC <- function(x, id = 'id', d = 'depths', n = 'name', interval = 10, m = 'soil_color') {
  
  # sanity check
  stopifnot(inherits(x, 'list') || inherits(x, 'character'))
  
  # mode switch
  .res <- switch(
    class(x), 
    # more expressive, list specification
    'list' = {
      .data <- .qSPC.list(x, id, d, n, m)
      .data
    }, 
    
    # simpler, hz designation only
    # template must be consistent across elements of x
    'character' = {
      
      # character template mode, detected from first element of x
      
      # mode 1: "A-Bt-Cr-R" -> random depths
      .m1 <- grepl(pattern = '-', x = x[1], fixed = TRUE)
      
      # mode 2: "A|BtBt|Cr|RRRR" -> proportional depths
      .m2 <- grepl(pattern = '|', x = x[1], fixed = TRUE)
      
      # ... mode N
      
      # assemble bit mask
      # overkill, but seemed like a fun idea
      .b <- paste(as.integer(c(.m1, .m2)), collapse = '')
      
      # parse based on mode encoded in bit mask
      switch(.b,
             # mode 1
             '10' = {
               # vectorization
               if(length(x) < 2) {
                 .data <- .qSPC.char.1(x)  
               } else {
                 .data <- lapply(x, .qSPC.char.1)
                 .data <- aqp::combine(.data)
                 .data
               }
             }, 
             # mode 2
             '01' = {
               # vectorization
               if(length(x) < 2) {
                 .data <- .qSPC.char.2(x, interval = interval)  
               } else {
                 .data <- lapply(x, .qSPC.char.2, interval = interval)
                 .data <- aqp::combine(.data)
                 .data
               }
             },
        # error condition: bad formatting
        stop('incorrect horizon sequence specified', call. = FALSE)
      )
      
      .data
    })
  
  return(.res)
}


.parseID <- function(x, d = ':') {
  # attempt splitting optional ID prefix
  .s <- strsplit(x, ':', fixed = TRUE)[[1]]
  
  # length > 1 indicates an ID prefix
  if(length(.s) >1) {
    .id <- .s[1]
    # ID now removed from hz sequence
    x <- .s[2]
  } else {
    .id <- NULL
  }
  
  return(list(id = .id, x = x))
}


# handle character-based templates, mode 1
# x <- 'A-C-R'
.qSPC.char.1 <- function(x) {
  
  # detect / extract ID prefix
  .s <- .parseID(x)
  x <- .s$x
  
  # split name sequence into horizons
  .names <- strsplit(x, '-', fixed = TRUE)[[1]]
  .nhz <- length(.names)
  
  # sanity check
  if(any(.names == '')) {
    stop('Empty horizon designation not allowed in this template', call. = FALSE) 
  }
  
  # random horizon thickness
  .thick <- round(runif(.nhz, min = 5, max = 20))
  
  # convert to top/bottom depths
  .bottom <- cumsum(.thick)
  .top <- c(0, .bottom[-.nhz])
  
  # assemble
  .data <- data.frame(
    top = .top,
    bottom = .bottom,
    name = .names
  )
  
  # IDs
  if(is.null(.s$id)) {
    # generate one via digest
    if(!requireNamespace("digest", quietly = TRUE)) {
      stop("package `digest` is required", call. = FALSE)
    }
    
    .data$id <- as.character(
      digest::digest(.data, algo = 'xxhash32')
    )
  } else {
    # use the ID prefix
    .data$id <- .s$id
  }
  
  # init SPC
  depths(.data) <- c('id', 'top', 'bottom')
  
  # attempt setting horizon designation
  hzdesgnname(.data) <- 'name'
  
  return(.data)
}

# handle character-based templates, mode 2
# x <- 'ApAp|AA|Bh|BhsBhs|Bt1|Bt2Bt2|CCCC|Cr'
.qSPC.char.2 <- function(x, interval = 10) {
  
  # detect / extract ID prefix
  .s <- .parseID(x)
  x <- .s$x
  
  # split name sequence into horizons
  .name.thick <- strsplit(x, '|', fixed = TRUE)[[1]]
  .nhz <- length(.name.thick)
  
  # extract names
  .names <- lapply(strsplit(.name.thick, ''), unique)
  .names <- sapply(.names, paste, collapse = '')
  
  # sanity check
  if(any(.names == '')) {
   stop('Empty horizon designation not allowed in this template', call. = FALSE) 
  }
  
  # proportional horizon thickness
  # count labels
  .counts <- stringr::str_count(.name.thick, .names)
  # interval-unit thickness per label instance
  .thick <- .counts * interval
  
  # convert to top/bottom depths
  .bottom <- cumsum(.thick)
  .top <- c(0, .bottom[-.nhz])
  
  # assemble
  .data <- data.frame(
    top = .top,
    bottom = .bottom,
    name = .names
  )
  
  # IDs
  if(is.null(.s$id)) {
    # generate one via digest
    if(!requireNamespace("digest", quietly = TRUE)) {
      stop("package `digest` is required", call. = FALSE)
    }
    
    .data$id <- as.character(
      digest::digest(.data, algo = 'xxhash32')
    )
  } else {
    # use the ID prefix
    .data$id <- .s$id
  }
  
  # init SPC
  depths(.data) <- c('id', 'top', 'bottom')
  
  # attempt setting horizon designation
  hzdesgnname(.data) <- 'name'
  
  return(.data)
}

# list-based template
.qSPC.list <- function(x, id, d, n, m) {
  
  # sanity check on column names
  stopifnot(id %in% names(x))
  stopifnot(d %in% names(x))
  
  # sanity check: accidentally included 0?
  if(0 %in% x[[d]]) {
    stop('depths should be specified as horizon bottom depths, did you accidentally include the top (0) of the profile?', call. = FALSE)
  }
  
  # extract top / bottom depths
  # x$d contains horizon bottom depths
  #     referenced to 0-datum
  .bottom <- x[[d]]
  .top <- c(0, .bottom[-length(.bottom)])
  
  # extract ID
  # single value, interpret as character
  .id <- as.character(x[[id]])
  
  # remove depths / id
  # leaving only hz data
  x[[id]] <- NULL
  x[[d]] <- NULL
  
  # list -> DF
  .data <- data.frame(
    .id, 
    top = .top,
    bottom = .bottom,
    as.data.frame(x)
  )
  
  # sanity check on number of rows vs. depths
  stopifnot(nrow(.data) == length(.top))
  
  # retain original ID name
  names(.data)[1] <- id
  
  # attempt to init soil color from Munsell notation
  if(!is.null(.data[[m]])) {
    .data[[m]] <- parseMunsell(.data[[m]])
  }
  
  # init SPC
  depths(.data) <- c(id, 'top', 'bottom')
  
  # attempt setting horizon designation
  try(hzdesgnname(.data) <- n, silent = TRUE)
  
  return(.data)
}


