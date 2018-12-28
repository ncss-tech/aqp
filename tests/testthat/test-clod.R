context("clod - horizon intersection and aggregation")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

test_that("aggregation of horizons by depth", {
    # intersection at a single depth should return only one horizon
    expect_equal(sum(clod(p, 50) %in% hzID(p)), 1)

    #hz.dz is 5-keystroke alias of intersect.horizon()
    expect_equal(clod(p, 25, 50), intersect.horizon(p, 25, 50))
  
    # spc.by.z clods() your input SPC's horizons p by depths specified
    foo <- spc.by.z(p, 25, 100)
    
    # and returns an SPC
    expect_equivalent(class(foo), 'SoilProfileCollection')
    
    #there should be only one profile
    expect_equal(length(foo), 1)
    
    #make sure it gets right number of horizons in 25-100cm
    expect_equal(nrow(foo), 4) 
})

