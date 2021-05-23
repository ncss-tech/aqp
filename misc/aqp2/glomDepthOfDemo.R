# vectorized glom + depthOf
library(aqp)

# try with loafercreek
data(loafercreek, package = "soilDB")

# check data visually first
loafercreek |> 
  transform(is_t = grepl('t', hzname)) |> 
  {\(x, i) `[`(x,i)}(1:10) |> 
  plot(color = "is_t") 

# inspect depthOf output
loafercreek |> 
  depthOf(pattern =  "t") |> 
  head()

# some new ideas for more functional forms of aqp SPC methods
.data_dots <- aqp:::.data_dots
.sliceSPC <- \(x,i) `[`(x,.data_dots(compositeSPC(x), eval(substitute(i)))[[1]])

.grepBounds <- function(p, pattern) {
  hzd <- horizonDepths(p)
  p$z1 <- minDepthOf(p, pattern = pattern)[[hzd[1]]]
  p$z2 <- maxDepthOf(p, pattern = pattern, top = FALSE)[[hzd[2]]]
  p
}

.niceglom <- function(p, z1, z2 = NULL,
                     ids = FALSE, df = FALSE,
                     truncate = FALSE, invert = FALSE,
                     modality = "all") {
  .data <- compositeSPC(p)
  
  glom(
    p,
    z1 = .data_dots(.data, eval(substitute(z1)))[[1]],
    z2 = .data_dots(.data, eval(substitute(z2)))[[1]],
    ids = ids,
    df = df,
    truncate = truncate,
    invert = invert,
    modality = modality
  )
}

loafercreek |> 
  # subset(grepl("t", hzname)) |>
  .grepBounds("t") |> 
  .niceglom(z1, z2) |> 
  .sliceSPC(1:10) |> 
  plot()

# trying pipebind (=>)
Sys.setenv("_R_USE_PIPEBIND_" = "true")
loafercreek |>
  subset(grepl("t", hzname)) |>
  . => {
    \(x) {
      hzd <- horizonDepths(x)
      x$z1 <- minDepthOf(x, pattern = "t")[[hzd[1]]]
      x$z2 <- maxDepthOf(x, pattern = "t", top = FALSE)[[hzd[2]]]
      glom(x, x$z1, x$z2)
    }
  }(.) |>
  .sliceSPC(1:10) |> 
  plot()



