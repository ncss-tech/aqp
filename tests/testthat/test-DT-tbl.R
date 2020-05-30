context("data.table and tbl_df inheritance from data.frame")

test_that("basic coercion", {
  ###  create empty frames
  empty_df <- data.frame()
  
  if (requireNamespace("data.table")) {
     empty_dt <- data.table::as.data.table(empty_df)
     expect_equal(dim(empty_dt), c(0, 0))
  } 
  
  if (requireNamespace("tibble")) {
    empty_tb <- tibble::as_tibble(empty_df)
    expect_equal(dim(empty_tb), c(0, 0))
  }
  
  ### .as.data.frame.aqp wrapper on data.frames
  expect_equal(aqp:::.as.data.frame.aqp(empty_df, "data.frame"), empty_df)
  ## data frame with 0 columns and 0 rows
  
  expect_equal(aqp:::.as.data.frame.aqp(empty_df, "data.table"), empty_dt)
  ## Null data.table (0 rows and 0 cols)
  
  expect_equal(aqp:::.as.data.frame.aqp(empty_df, "tbl_df"), empty_tb)
  ## A tibble: 0 x 0
  
  
  # as.data.frame wrapper on tibbles
  expect_equal(aqp:::.as.data.frame.aqp(empty_tb, "data.table"), empty_dt)
  # Null data.table (0 rows and 0 cols)
  #
  expect_equal(aqp:::.as.data.frame.aqp(empty_tb, "data.frame"), empty_df)
  # data frame with 0 columns and 0 rows
  #
  expect_equal(aqp:::.as.data.frame.aqp(empty_tb, "tbl_df"), empty_tb)
  # A tibble: 0 x 0
  
  # as.data.frame wrapper on data.table
  expect_equal(aqp:::.as.data.frame.aqp(empty_dt, "data.table"), empty_dt)
  # Null data.table (0 rows and 0 cols)
  
  expect_equal(aqp:::.as.data.frame.aqp(empty_dt, "data.frame"), empty_df)
  # data frame with 0 columns and 0 rows
  
  expect_equivalent(aqp:::.as.data.frame.aqp(empty_dt, "tbl_df"), empty_tb)
  # note: that because of slight internal differences (leftover from data.table?)
  #       cannot use expect_equal -- though they are clearly equivalent
  # 
  # > str(aqp:::.as.data.frame.aqp(empty_dt, "tbl_df"))
  # tibble [0 × 0] (S3: tbl_df/tbl/data.frame)
  # Named list()
  # - attr(*, ".internal.selfref")=<externalptr> 
  # 
  # > str(empty_tb)
  # tibble [0 × 0] (S3: tbl_df/tbl/data.frame)
  # Named list()
  
  })

dfclasses <- list("data.frame", "data.table", "tbl_df")

res <- lapply(dfclasses, function(use_class) {
  
  ## 
  ## 
  ## debug note -- this is vectorized over the data.frame classes
  ## 
  ## 
  
  # debug
  # use_class <- "data.frame"
  
  test_that(sprintf("basic functionality of %s", use_class), {
    # class to use (data.frame, data.table or tbl_df)
    
    cc <- function(l) {
      do.call('c', as.list(l))
    }
    
    # basic function for converting inputs btween tibble and data.frame
    test_object <- function(object, use_class) {
      aqp:::.as.data.frame.aqp(object, as.class = use_class)
    }
    
    # make some fake data
    df <- data.frame(
      id = cc(lapply(1:4, function(i)
        rep(i, 10))),
      top = cc(rep(0:9, 4)),
      bottom = cc(rep(1:10, 4)),
      siteprop = 8,
      prop = 18
    )
    
    # construct a test object of type use_class
    test <- test_object(object = df, use_class = use_class)
    
    # promote to SPC -- convert numeric ids to character
    expect_message(depths(test) <-
                     id ~ top + bottom,
                   c("converting profile IDs from integer to character"))
    
    # "normalize" (horizon -> site) a site-level attribute
    site(test) <- ~ siteprop
    
    # check that ids are in order
    expect_equal(profile_id(test), site(test)$id)
    
    # check that siteprop is in site
    expect_equal(site(test)$siteprop, rep(8, 4))
    
    # check that siteprop is not in horizons (anymore)
    switch(
      use_class,
      # data.frame and data.table
      expect_equal(horizons(test)$siteprop, NULL),
      
      # note: this is only to cover a warning and to document differences
      #       tbl_df will not return NULL -- warning and length(0)
      "tbl_df" = expect_warning(
        expect_equal(length(horizons(test)$siteprop), 0),
        "Unknown or uninitialised column: `siteprop`."
      )
    )
    
    # "denormalize" into a different-named horizon attribute
    horizons(test)$siteprop_hz <- denormalize(test, "siteprop")
    
    # check that siteprop_hz is in horizons 
    expect_equal(horizons(test)$siteprop_hz, rep(8, 40))
    
    # add some site data, for only two sites
    value <- test_object(data.frame(id = as.character(2:3),
                                   siteclass = state.abb[1:2]),
                        use_class)
    
    # left join 
    site(test) <- value
    
    # inspect site
    site(test)
    
    # inspect site table with new variable (first/last NA), sorted correct
    expect_equal(which(is.na(site(test)$siteclass)), c(1, 4))
    
    # inspect horizon table
    horizons(test)
    
    # test $ [[ setter/getter
    test$foo <- rep(100, nrow(test))
    expect_true(all(test[['foo']] == horizons(test)$foo))
    
    test[["foo"]] <- rep(200, nrow(test))
    expect_equal(horizons(test)$foo,  rep(200, nrow(test)))
    
    expect_equal(horizons(test)[[idname(test)]], as.character(cc(lapply(1:4, rep, 10))))
    expect_equal(horizons(test)[[horizonDepths(test)[2]]], cc(rep(1:10, 4)))
    
    # add some horizon data
    value <- test_object(data.frame(
                          id = as.character(2:3),
                          hzID = as.character(20:21),
                          hzclass = letters[1:2]
                         ), use_class)
    
    switch(
      use_class,
      # data.frame and tbl_df
      expect_message({
        horizons(test) <- value
      } ,
      "join condition resulted in sorting of horizons, re-applying original order"),
      
      # note: data.table sorts correctly without invoking re-order
      "data.table" = {
        expect_silent( { horizons(test) <- value } )
      }
    )
    
    expect_equal(horizons(test)[[idname(test)]], as.character(cc(lapply(1:4, rep, 10))))
    
    expect_equal(horizons(test)[[horizonDepths(test)[2]]], cc(rep(1:10, 4)))
    
  })
})
