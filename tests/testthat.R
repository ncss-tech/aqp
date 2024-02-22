library(testthat)
library(aqp)

# limit all tests involving data.table 
# to 2 threads for CRAN compliance
data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))

# Sys.setenv("_SP_EVOLUTION_STATUS_"=2)
test_check("aqp")
