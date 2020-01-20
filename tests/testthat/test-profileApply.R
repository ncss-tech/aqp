context("profileApply - SoilProfileCollection iterator")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

attr <- 'prop' # clay contents % 

test_that("profileApply - basic tests of output datatypes", {
  r1 <- profileApply(sp1, estimateSoilDepth, name="name", top="top", bottom="bottom")
  expect_equal(names(r1),c("P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009"))
  expect_equal(r1, c(P001 = 89L, P002 = 59L, P003 = 67L, P004 = 62L, P005 = 68L, P006 = 200L, P007 = 233L, P008 = 200L, P009 = 240L))
  
  r2 <- profileApply(sp1, estimateSoilDepth, name="name", top="top", bottom="bottom", simplify = FALSE)
  expect_equal(r2, list(P001 = 89L, P002 = 59L, P003 = 67L, P004 = 62L, P005 = 68L, 
                        P006 = 200L, P007 = 233L, P008 = 200L, P009 = 240L))
  
  r3 <- profileApply(sp1, function(p) {
      d <- estimateSoilDepth(p, name="name", top="top", bottom="bottom")
      res <- data.frame(profile_id(p), d)
      colnames(res) <- c(idname(p), "soildepthest")
      return(res)
    }, frameify = TRUE)
  expect_true(inherits(r3,'data.frame'))
})
