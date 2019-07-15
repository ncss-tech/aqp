context("plotSPC")

data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group


## tests

test_that("plotSPC: aqp.env settings", {
  
  # explainer
  explainPlotSPC(sp1)
  
  # get plotting details from aqp environment
  lsp <- get('last_spc_plot', envir=aqp.env)
  
  # should be a list
  expect_true(is.list(lsp))
  
  # check for required components
  expect_equal(names(lsp), c("width", "plot.order", "x0", "pIDs", "idname", "y.offset", "scaling.factor", 
                             "max.depth", "n", "extra_x_space", "extra_y_space"))
  
  # basic integrity checks
  expect_equal(profile_id(sp1), lsp$pIDs)
  expect_equal(idname(sp1), lsp$idname)
  expect_true(length(sp1) == lsp$n)
  expect_equal(1:length(sp1), lsp$plot.order)
})

test_that("plotSPC: figure settings", {
  
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, scaling.factor=0.5, width=0.8, y.offset=8, n=15, max.depth = 100)
  
  # check adjustments
  expect_equal(lsp$scaling.factor, 0.5)
  expect_equal(lsp$width, 0.8)
  expect_equal(lsp$y.offset, 8)
  expect_equal(lsp$n, 15)
  expect_equal(lsp$max.depth, 100)
})


test_that("plotSPC: re-ordering of profiles", {
  
  # re-order
  new.order <- c(1,3,5,7,9,2,4,6,8)
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, plot.order=new.order)
  
  # profile IDs should be sorted according to plot.order
  expect_equal(profile_id(sp1)[new.order], lsp$pIDs)
  
  # plotting order should be saved
  expect_equal(new.order, lsp$plot.order)
})


test_that("plotSPC: relative spacing", {
  
  # relative positions
  x.pos <- c(1+0, 2+0, 3+0, 4+0.1, 5+0.1, 6+0.1, 7+0.2, 8+0.2, 9+0.2)
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, relative.pos = x.pos)
  
  # plot order not affected
  expect_equal(lsp$plot.order, 1:length(sp1))
  
  # x0 adjusted as expected
  # 1:length(x) + offsets
  expect_equal(lsp$x0, x.pos)
})




test_that("plotSPC: re-ordering of profiles and relative spacing", {
  
  # new order
  new.order <- c(1,3,5,7,9,2,4,6,8)
  # relative positions: in the new ordering 
  x.pos <- c(1+0, 2+0, 3+0, 4+0.1, 5+0.1, 6+0.1, 7+0.2, 8+0.2, 9+0.2)
  
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, plot.order=new.order, relative.pos = x.pos)
  
  # profile IDs should be sorted according to plot.order
  expect_equal(profile_id(sp1)[new.order], lsp$pIDs)
  
  # plotting order should be saved
  expect_equal(new.order, lsp$plot.order)
  
  # x0 adjusted as expected
  expect_equal(lsp$x0, x.pos)
})


test_that("plotSPC: re-ordering via relative spacing", {
  
  # re-order by adjusting the relative positions
  x.pos <- length(sp1):1
  
  # explainer returns `lsp`
  lsp <- explainPlotSPC(sp1, relative.pos = x.pos)
  
  # plotting order / IDs are not modified!
  expect_equal(profile_id(sp1), lsp$pIDs)
  expect_equal(lsp$plot.order, 1:length(sp1))
  
  # x0 adjusted as expected
  expect_equal(lsp$x0, x.pos)
})





