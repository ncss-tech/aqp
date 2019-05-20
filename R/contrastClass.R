

contrastClass <- function(v1, c1, v2, c2, dH, dV, dC, verbose=FALSE) {
  
  # sanity check, all inputs must have the same length
  l <- c(length(v1), length(c1), length(v2), length(c2), length(dH), length(dV), length(dC))
  if(length(unique(l)) > 1) {
    stop('inputs must all have the same length', call. = FALSE)
  }
  
  # init space for contrast classes
  res <- vector(mode='character', length=length(v1))
  
  
  ## Faint
  # case 1: difference in hue = 0, difference in value is <= 2, and difference in chroma is <= 1, or
  f.case1 <- (dH == 0) & (dV <= 2) & (dC <= 1)
  
  # case 2: difference in hue = 1, difference in value is <= 1, and difference in chroma is <= 1, or
  f.case2 <- (dH == 1) & (dV <= 1) & (dC <= 1)
  
  # case 3: difference in hue = 2, difference in value = 0, and difference in chroma = 0, or
  f.case3 <- (dH == 2) & (dV == 0) & (dC == 0)
  #
  res[f.case1 | f.case2 | f.case3] <- 'Faint'
  
  ## Distinct
  # case 1: difference in hue = 0, and
  #           a. difference in value is <= 2 and difference in chroma is > 1 to < 4, or
  #           b. difference in value is > 2 to < 4 and difference in chroma is < 4
  d.case1 <- (dH == 0) & ( ( (dV <= 2) & (dC > 1) & (dC < 4) ) | ( (dV > 2) & (dV < 4) & (dC < 4)) )
  
  # case 2: difference in hue = 1, and
  #           a. difference in value is <= 1 and difference in chroma is > 1 to < 3, or
  #           b. difference in value is > 1 to < 3, and difference in chroma is < 3
  d.case2 <- (dH == 1) & ( ( (dV <= 1) & (dC > 1) & (dC < 3) ) | ( (dV > 1) & (dV < 3) & (dC < 3)) )
  
  # case 3: difference in hue = 2, and
  #           a. difference in value = 0 and difference in chroma is > 0 to < 2, or
  #           b. difference in value is > 0 to < 2 and difference in chroma is < 2.
  d.case3 <- (dH == 2) & ( ( (dV == 0) & (dC > 0) & (dC < 2) ) | ( (dV > 0) & (dV < 2) & (dC < 2)) )
  #
  res[d.case1 | d.case2 | d.case3] <- 'Distinct'
  
  
  # low vlaue / chroma exception
  # both colors have values of <= 3 and chromas of <= 2
  low.value.chroma <- ((v1 <= 3) & (v2 <= 3)) & ((c1 <= 2) & (c2 <= 2))
  res[low.value.chroma] <- 'Faint'
  
  # anyhting else is Prominent
  res[res == ''] <- 'Prominent'
  
  # convert to ordered factor
  res <- factor(res, levels=c('Faint', 'Distinct', 'Prominent'), ordered = TRUE)
  
  # NA propagation
  idx <- apply(cbind(v1, c1, v2, c2, dH, dV, dC), 1, function(i) any(is.na(i)))
  res[idx] <- NA
  
  # conversion to factors happens elsewhere
  
  if(verbose){
    return(list(
      'faint'=data.frame(v1, c1, v2, c2, dH, dV, dC, f.case1, f.case2, f.case3, low.value.chroma, res),
      'distinct'=data.frame(v1, c1, v2, c2, dH, dV, dC, d.case1, d.case2, d.case3, res)
    ))
  } else{
    return(res)  
  }
  
  
}
