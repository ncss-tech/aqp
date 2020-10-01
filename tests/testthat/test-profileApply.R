context("SoilProfileCollection iterator (profileApply")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

attr <- 'prop' # clay contents %

test_that("profileApply - basic tests of output datatypes", {
  r1 <- profileApply(sp1, estimateSoilDepth, name="name")
  expect_equal(names(r1),c("P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009"))
  expect_equal(r1, c(P001 = 89L, P002 = 59L, P003 = 67L, P004 = 62L, P005 = 68L, P006 = 200L, P007 = 233L, P008 = 200L, P009 = 240L))

  r2 <- profileApply(sp1, estimateSoilDepth, name="name", simplify = FALSE)
  expect_equal(r2, list(P001 = 89L, P002 = 59L, P003 = 67L, P004 = 62L, P005 = 68L,
                        P006 = 200L, P007 = 233L, P008 = 200L, P009 = 240L))

  r3 <- profileApply(sp1, function(p) {
      d <- estimateSoilDepth(p, name="name")
      res <- data.frame(profile_id(p), d, stringsAsFactors = FALSE)
      colnames(res) <- c(idname(p), "soildepthest")
      return(res)
    }, frameify = TRUE)
  expect_true(inherits(r3,'data.frame'))
})

test_that("profileApply - frameify option", {

  # 1 row per profile
  expect_silent(r1 <- profileApply(sp1, frameify = TRUE, column.names = c("foo","bar"), function(p) {
    data.frame(id = profile_id(p),
                depth = estimateSoilDepth(p, name = "name"))
  }))
  expect_equal(nrow(r1), 9)

  # 1 row per horizon
  expect_silent(r1 <- profileApply(sp1, frameify = TRUE, column.names = c("id","foo","bar"), function(p) {
    data.frame(id = profile_id(p),
               hzID = hzID(p),
               depth = estimateSoilDepth(p, name = "name"))
  }))
  expect_equal(nrow(r1), 60)

  # some profiles with no result
  expect_silent(r2 <- profileApply(sp1, frameify = TRUE, function(p) {
    res <- data.frame(id = profile_id(p),
               depth = estimateSoilDepth(p, name = "name"))
    if (res$depth < 200)
      return(res)
  }))

  # non-data.frame first result
  expect_warning(r2 <- profileApply(sp1[1,], frameify = TRUE, function(p) {
   estimateSoilDepth(p, name = "name")
  }))
  expect_true(inherits(r2, "list"))
})
