# determine index of Munsell hue from 5R ---> 5PB
# x: vector of Munsell hues
huePosition <- function(x) {
  # ordering via Tech Note #2
  # https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
  hues <- c('5R', '7.5R', '10R', 
            '2.5YR', '5YR', '7.5YR', '10YR', 
            '2.5Y', '5Y', '7.5Y', '10Y', 
            '2.5GY', '5GY', '7.5GY', '10GY', 
            '2.5G', '5G', '7.5G', '10G',
            '2.5BG', '5BG', '7.5BG', '10BG',
            '2.5B', '5B', '7.5B', '10B',
            '2.5PB', '5PB')
  
  # not really needed, but maybe useful later
  # hues <- factor(hues, levels = hues, ordered = TRUE)
  
  res <- match(x, hues)
  return(res)
}
