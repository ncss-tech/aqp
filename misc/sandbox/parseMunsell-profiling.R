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
  parseMunsell(x1, convertColors = FALSE), 
  parseMunsell(x2, convertColors = FALSE), 
  parseMunsell(x3, convertColors = FALSE), 
  times = 1
)






