


# compute metrics of color contrast: delta-Hue, Value, Chroma and delta-E00
# m1: vector of Munsell colors ('10YR 3/3')
# m2: vector of Munsell colors ('10YR 3/4')
colorContrast <- function(m1, m2) {
  
  # in case colors are encoded as factors
  m1 <- as.character(m1)
  m2 <- as.character(m2)
  
  # if character vectors, split into data.frame of hue/value/chroma
  m1.pieces <- parseMunsell(m1, convertColors = FALSE)
  m2.pieces <- parseMunsell(m2, convertColors = FALSE)
  
  
  ## https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
  # difference in number of hue chips
  dH <- abs(huePosition(m1.pieces[[1]]) - huePosition(m2.pieces[[1]]))
  # difference in number of value chips
  dV <- abs(as.numeric(m1.pieces[[2]]) - as.numeric(m2.pieces[[2]]))
  # difference in number of chroma chips
  dC <- abs(as.numeric(m1.pieces[[3]]) - as.numeric(m2.pieces[[3]]))
  
  # get CIE LAB representation
  m1.lab <- parseMunsell(m1, convertColors = TRUE, returnLAB=TRUE)
  m2.lab <- parseMunsell(m2, convertColors = TRUE, returnLAB=TRUE)
  
  # delta E00
  #
  # we don't need the full distance matrix,
  # iterate over rows, much more scaleable
  d <- list()
  for(i in 1:nrow(m1.lab)){
    d[i] <- compare_colour(m1.lab[i, ], m2.lab[i, ], from_space='lab', method = 'CIE2000', white_from = 'D65')
  }
  dE00 <- unlist(d)
  
  # combine into DF and return
  res <- data.frame(m1, m2, dH, dV, dC, dE00, stringsAsFactors = FALSE)
  return(res)
}



