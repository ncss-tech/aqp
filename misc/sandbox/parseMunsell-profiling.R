library(aqp)
library(microbenchmark)


# 1 second
x1 <- rep('10YR 4/4', times = 1e3)
# 10 seconds
x2 <- rep('10YR 4/4', times = 1e4)
# 100 seconds
x3 <- rep('10YR 4/4', times = 1e5)

# impossibly long
x4 <- rep('10YR 4/4', times = 1e6)
x5 <- rep('10YR 4/4', times = 1e7)

microbenchmark(
  '1e3' = parseMunsell(x1, convertColors = FALSE), 
  '1e4' = parseMunsell(x2, convertColors = FALSE), 
  '1e5' = parseMunsell(x3, convertColors = FALSE), 
  '1e6' = parseMunsell(x4, convertColors = FALSE), 
  '1e7' = parseMunsell(x5, convertColors = FALSE), 
  times = 1
)






