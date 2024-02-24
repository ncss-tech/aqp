# NCSP() optimization

# converted for-loop into data.table call for slice-wise .NCSP_distanceCalc

# costly operations in .NCSP_distanceCalc
#  - daisy() for each slice is the primary cost (~1/3 execution time)
#  - outer() twice for each slice adds up 
#  - conversion from dist->matrix->dist is somewhat costly (requires 2+ copy; adds up)

# opportunities
#  - can daisy/outer/outer/matrix conversion etc. be called only once--for all slices?

library(aqp)
#> This is aqp 2.0

# setup horizon-level data: data are from lab sampled pedons
d <- read.csv(
  textConnection('series,top,bottom,clay,frags,ph
auburn,0,3,21,6,5.6
auburn,3,15,21,13,5.6
auburn,15,25,20,9,5.8
auburn,25,47,21,28,5.8
dunstone,0,5,16,13,6
dunstone,5,17,17,19,6.3
dunstone,17,31,20,6,6.3
dunstone,31,41,21,15,6.3
sobrante,0,5,18,0,5.8
sobrante,5,10,16,2,5.7
sobrante,10,28,15,21,5.8
sobrante,28,51,18,13,6.2
sobrante,51,74,20,12,6.2'))

# establish site-level data
s <- data.frame(
  series = c('auburn', 'dunstone', 'sobrante'), 
  precip = c(24, 30, 32)
)

# generate fake horizon names with clay / frags / ph
d$name <- with(d, paste(clay, frags, ph, sep='/'))

# upgrade to SoilProfile Collection object
depths(d) <- series ~ top + bottom
site(d) <- s
hzdesgnname(d) <- 'name'


bench::mark(
  memory = FALSE,
  before = NCSP(
    d,
    vars = c('clay', 'ph', 'frags'),
    k = 0,
    maxDepth = 61
  ),
  after = aqp:::.NCSP2(
    d,
    vars = c('clay', 'ph', 'frags'),
    k = 0,
    maxDepth = 61
  )
)
#> Computing dissimilarity matrices from 3 profiles
#>  [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Computing dissimilarity matrices from 3 profiles [0.09 Mb]
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 before        517ms    517ms      1.93        NA     3.87
#> 2 after         114ms    131ms      7.78        NA     3.89

set.seed(123)
x <- aqp::combine(lapply(1:100, random_profile, SPC = TRUE))
bench::mark(
  memory = FALSE,
  before = NCSP(
    x,
    vars = c('p1', 'p2', 'p3', 'p4', 'p5'),
    k = 0,
    maxDepth = 143
  ),
  after = aqp:::.NCSP2(
    x,
    vars = c('p1', 'p2', 'p3', 'p4', 'p5'),
    k = 0,
    maxDepth = 143
  )
)
#> Computing dissimilarity matrices from 100 profiles [6 Mb]
#> Computing dissimilarity matrices from 100 profiles [6 Mb]
#> Computing dissimilarity matrices from 100 profiles [6 Mb]
#> Computing dissimilarity matrices from 100 profiles [6 Mb]
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 before        2.52s    2.52s     0.397        NA     18.3
#> 2 after            1s       1s     0.999        NA     14.0

set.seed(123)
x <- aqp::combine(lapply(1:1000, random_profile, SPC = TRUE))
bench::mark(
  memory = FALSE,
  before = NCSP(
    x,
    vars = c('p1', 'p2', 'p3', 'p4', 'p5'),
    k = 0,
    maxDepth = 177
  ),
  after = aqp:::.NCSP2(
    x,
    vars = c('p1', 'p2', 'p3', 'p4', 'p5'),
    k = 0,
    maxDepth = 177
  )
)
#> Computing dissimilarity matrices from 1000 profiles [700 Mb]
#> Computing dissimilarity matrices from 1000 profiles [700 Mb]
#> Computing dissimilarity matrices from 1000 profiles [700 Mb]
#> Computing dissimilarity matrices from 1000 profiles [700 Mb]
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 before        52.5s    52.5s    0.0191        NA     3.03
#> 2 after         40.9s    40.9s    0.0244        NA     3.86