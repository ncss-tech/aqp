segment <- function(object = NULL, intervals = NULL, trim = TRUE, hzdepcols = NULL) {
  
  dep <- data.frame(top = intervals[- length(intervals)],
                    bot = intervals[-1],
                    stringsAsFactors = FALSE
  )
  dep$id <- paste0(dep$top, "-", dep$bot)
  
  # tests
  test_spc <- class(object)[1] == "SoilProfileCollection"
  test_df  <- is.data.frame(object)
  test_hd  <- is.null(hzdepcols) & length(hzdepcols) == 2
  test_dep <- is.numeric(dep$top) & is.numeric(dep$bot) & all(dep$top < dep$bot)
  
  
  if (! any(test_spc, test_df)) {
    stop("the input must be either a SoilProfileCollection or data.frame")
  }
  
  if (!test_spc & test_df & test_hd) {
    stop("if the input is a data.frame then hzdepcols must not be NULL and length(hzdepcols) == 2")
  }
  
  if (!test_dep) {
    stop("intervals should be numeric and sequential (e.g. c(0, 1, 2, 3) or 0:100)")
  }
  
  
  # standardize inputs
  if (test_spc) {
    peid      <- idname(object)
    hzid      <- hzidname(object)
    hzdepcols <- horizonDepths(object)
    h  <- horizons(object)
    names(h)[names(h) %in% c(peid, hzid)] <- c("peid", "hzid")
  } else {
    h <- object
  }
  names(h)[names(h) %in% hzdepcols] <- c("hzdept", "hzdepb")
  
  # filter horizons and trim
  .slice <- function(h, top = NULL, bot = NULL) {
    idx <- h$hzdept <= bot & h$hzdepb >= top
    h <- h[idx, ]
    
    if (trim == TRUE) {
      h <- within(h, {
        hzdept = ifelse(hzdept < top, top, hzdept)
        hzdepb = ifelse(hzdepb > bot, bot, hzdepb)
        })
    h <- h[(h$hzdepb - h$hzdept) > 0, ]
    }
  }
  
  
  # slice spc by intervals
  dep$df <- lapply(1:nrow(dep), function(x) h[0, ])
  h <- {
    split(dep, dep$id) ->.;
    lapply(., function(x) {
      x$df[[1]] <- cbind(.slice(h, top = x$top, bot = x$bot), segment_id = x$id)
      return(x$df[[1]])
    }) ->.;
    do.call("rbind", .) ->.;
    }
  names(h)[names(h) %in% c("hzdept", "hzdepb")] <- hzdepcols
  
  
  if (test_spc) {
    h <- h[order(h$peid, h[hzdepcols[1]]), ]
    
    # merge to re-add spc with NA
    h_orig <- data.frame(peid = names(table(horizons(object)[peid])), stringsAsFactors = FALSE)
    h <- merge(h_orig, h, by = "peid", all.x = TRUE, sort = FALSE)
    rm(h_orig)
    
    # rebuild SPC
    names(h)[names(h) == "peid"] <- peid
    names(h)[names(h) == "hzid"] <- hzid
    h$hzID <- 1:nrow(h)
    
    replaceHorizons(object) <- h
    suppressMessages(hzidname(object) <- "hzID")
  }
  
  # return
  return(if (test_spc) object else h)
  }


