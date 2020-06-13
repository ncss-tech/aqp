\name{soiltexture}
\alias{soiltexture}
\docType{data}

\title{
Lookup tables for sand, silt, clay, texture class, and textural modifers.
}

\description{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, the average values per texture class, and average rock fragment values by textural modifer.
}

\usage{data("soiltexture")}

\format{
  A list with 3 data frames. The first named values which contains values for sand, silt and clay by texture class. The second with average values for sand, silt and clay per texture class. The third has fragvoltot low, rv and high values for texmod.
  \describe{
    \item{\code{clay}}{clay percentage of the fine earth fraction, a integer vector}
    \item{\code{sand}}{sand percentage of the fine earth fraction, a integer vector}
    \item{\code{silt}}{silt percentage of the fine earth fraction, a integer vector}
    \item{\code{texcl}}{texture class, a character vector}
    \item{\code{texmod}}{textural modifers, a character vector}
}}

\details{
A list that contains a snapshot of the values generated using the logic from the particle size estimator calculation in NASIS, and the average values per texture class.
}

\examples{
\donttest{

# how the lookup tables within data(soiltexture) are built

soiltexture = list()

values <- expand.grid(sand = 0:100, silt = 0:100)
values <- subset(values, (sand + silt) < 101)
row.names(values) <- NULL
values$clay <- 100 - values$sand - values$silt
values$texcl <- ssc_to_texcl(sand = values$sand, clay = values$clay, as.is = TRUE)

soiltexture$values <- values

avgs <- aggregate(cbind(clay, sand) ~ texcl, data = values, function(x) round(mean(x)))
avgs[avgs$texcl == "c", "clay"] <- 55
avgs <- rbind(avgs,
            c(texcl = "cos",  avgs[avgs$texcl == "s",  -1]),
            c(texcl = "fs",   avgs[avgs$texcl == "s",  -1]),
            c(texcl = "vfs",  avgs[avgs$texcl == "s",  -1]),
            c(texcl = "lcos", avgs[avgs$texcl == "ls", -1]),
            c(texcl = "lfs",  avgs[avgs$texcl == "ls", -1]),
            c(texcl = "lvfs", avgs[avgs$texcl == "ls", -1]),
            c(texcl = "cosl", avgs[avgs$texcl == "sl", -1]),
            c(texcl = "fsl",  avgs[avgs$texcl == "sl", -1]),
            c(texcl = "vfsl", avgs[avgs$texcl == "sl", -1])
            )
avgs$silt <- 100 - avgs$clay - avgs$sand

soiltexture$averages <- avgs


texmod <- data.frame(
  texmod     = c("gr", "grf", "grm", "grc", "grv", "grx",
                 "cb", "cbv", "cbx",
                 "st", "stv", "stx",
                 "by", "byv", "byx",
                 "cn", "cnv", "cnx",
                 "fl", "flv", "flx",
                 "pgr", "pgrv", "pgrx",
                 "pcb", "pcbv", "pcbx",
                 "pst", "pstv", "pstx",
                 "pby", "pbyv", "pbyx",
                 "pcn", "pcnv", "pcnx",
                 "pfl", "pflv", "pflx"
                 ),
  fragvoltot_l  = c(15, 15, 15, 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60,
                 15, 35, 60
                 ),
  fragvoltot_r  = c(25, 25, 25, 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75,
                 25, 48, 75
  ),
  fragvoltot_h = c(34, 34, 34, 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89,
                 34, 59, 89
  ),
  stringsAsFactors = FALSE
)
texmod <- within(texmod, {
  fragvoltot_l_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_l)
  fragvoltot_r_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_r)
  fragvoltot_h_nopf <- ifelse(grepl("^p", texmod), 0, fragvoltot_h)
  })

soiltexture$texmod <- texmod


save(soiltexture, file = "C:/workspace2/github/ncss-tech/aqp/data/soiltexture.rda")

}}
\keyword{datasets}
