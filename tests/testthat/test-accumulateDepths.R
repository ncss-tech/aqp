test_that("accumulateDepths works", {
  
  # sp4 is a data.frame
  data(sp4)
  
  # example using hzdatum argument data(sp4)
  hz <- accumulateDepths(sp4, "id", c("top", "bottom"), "name", hzdatum = 15)
  depths(hz) <- id ~ top + bottom
  expect_equal(min(hz$top), 15)
  
  # use the SPC interface by promoting sp4
  depths(sp4) <- id ~ top + bottom
  
  # and a custom hzdatum for each profile
  hz <- accumulateDepths(sp4,
                         id = "id",
                         hzdepths = c("top", "bottom"),
                         hzname = "name",
                         hzdatum = 5 * 1:length(sp4))
  
  # promote the result
  depths(hz) <- id ~ top + bottom
  
  # deepest horizon will be in second to last profile with a linearly increasing datum
  expect_true(max(hz$bottom) == 85 && sp4$id[which.max(hz$bottom)] == "shasta-trinity")
  # plot(hz) 
  
  # example using old-style O horizons
  hz <- read.table(text = "peiidref hzdept hzdepb hzname seqnum phiid
                                11      0      5      A      2   295
                                11      3      1     Oi      1   293
                                11      1      0     Oe      1   294
                                11      5     13     C1      3   296
                                11     13     58     C2      4   297
                                11     58    152     C3      5   298
                                13      0      5      A      2   303
                                13      1      0     Oe      1   302
                                13      5     25     Bw      3   304
                                13     25     61      C      4   305
                                13     61     NA      R      5   306
                               136      0     13     A1      3   695
                               136      1      0     Oe      2   694
                               136      2      1     Oi      1   693
                               136     13     61     C1      4   696
                               136     61     76     C2      5   697",
    header = TRUE, row.names = NULL)

  expect_warning({depths(hz) <- peiidref ~ hzdept + hzdepb})

  hz_fixed <- accumulateDepths(hz,
                                id = "peiidref",
                                hzdepths = c("hzdept", "hzdepb"),
                                hzname = "hzname")

  is_valid <- checkHzDepthLogic(hz_fixed)$valid

  test1 <- subset(hz_fixed, is_valid)
  origO <- subset(hz, grepl("O", hzname))
  fixedO <- subset(hz_fixed, grepl("O", hzname))
  
  # no O horizons are lost by the fixing
  expect_true(length(origO) == 3 && length(fixedO) == 3)
  
  # all original profiles that were invalid now have valid geometry
  expect_length(test1, 3)
  
  # and the correct O horizons have 0 top depth and appropriate bottom depths
  expect_true(all(test1[,1]$hzname == c("Oi", "Oe", "Oi") &
                  test1[,1]$hzdept == rep(0, 3) &
                  test1[,1]$hzdepb == c(2,1,1)))
})
