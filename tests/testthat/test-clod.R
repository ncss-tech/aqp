context("clod - ragged horizon intersection")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

test_that("intersection of horizons by depth", {
    # intersection at a single depth should return only one horizon
    expect_equal(sum(clod(p, 50) %in% hzID(p)), 1)

    # spc.by.z 'clods' your input SPC `p`'s horizons by depths specified
    foo <- spc.by.z(p, 25, 100)
    
    # and returns an SPC
    expect_equivalent(class(foo), 'SoilProfileCollection')
    
    # within that SPC there should be only one profile
    expect_equal(length(foo), 1)
    
    # and that profile should have 4 horizons in 25-100cm
    expect_equal(nrow(foo), 4) 
})

