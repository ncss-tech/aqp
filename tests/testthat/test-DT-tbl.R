context("data.table and tbl_df inheritance from data.frame")


test_that("basic coercion", {

  skip_on_cran()

  ###  create empty frames
  empty_df <- data.frame()

  if (requireNamespace("data.table", quietly = TRUE)) {
     empty_dt <- data.table::as.data.table(empty_df)
     expect_equal(dim(empty_dt), c(0, 0))
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
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
    test2 <- test

    # promote to SPC -- convert numeric ids to character
    expect_message(depths(test) <- id ~ top + bottom,
                   c("converting profile IDs from integer to character"))

    # add fake coordinates
    crds <- data.frame(id = profile_id(test),
                       y  = rnorm(length(test)),
                       x  = rnorm(length(test)))
    site(test) <- crds

    # promote to spatial
    initSpatial(test, crs = "+proj=longlat +datum=WGS84") <- ~ x + y

    # show method should be produce output without error
    expect_output(show(test))
    expect_output(show(test[0,]))

    # fill in diagnostics and restrictions with fake data
    diagnostic_hz(test) <- data.frame(id = profile_id(test),
                                   featkind = "foo",
                                   featdept = 0, featdepb = 10)
    restrictions(test) <- data.frame(id = profile_id(test),
                                  restrkind = "bar",
                                  restrdept = 0, restrdepb = 10)

    # try the character vector interface too
    expect_message(depths(test2) <- c("id", "top", "bottom"),
                   c("converting profile IDs from integer to character"))

    # test rebuild
    expect_message(rebuildSPC(test2), "using `hzID` as a unique horizon ID")

    # test enforce_df_class
    expect_silent(aqp:::.enforce_df_class(test2, use_class))

    # "normalize" (horizon -> site) a site-level attribute
    site(test) <- ~ siteprop
    
    # new random XY data
    test$y <- rnorm(length(test))
    test$x <- rnorm(length(test))
    test$newx <- denormalize(test, "x")
    test$newy <- denormalize(test, "y")
    
    # promote to spatial works from horizons
    # formula and character are allowed inputs on RHS
    # LHS specifies optional CRS
    initSpatial(test, crs = "OGC:CRS84") <- ~ newx + newy
    
    # siteprop removed from horizons
    if (use_class == "tbl_df") {
      expect_warning(expect_null(horizons(test)$siteprop))
    } else {
      expect_null(horizons(test)$siteprop)
    }
    
    # newx is still in horizon data (converted to site level only on coercion)
    expect_false(is.null(horizons(test)$newx))
    
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
    # print(str(names(horizons(test)$siteprop_hz)))
    if(use_class == "tbl_df") {
      expect_equivalent(horizons(test)$siteprop_hz, rep(8, 40))
      # note: tbl_df will have names attribute attached to result
      #       which precludes expect_equal
    } else {
      expect_equal(horizons(test)$siteprop_hz, rep(8, 40))
    }

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

    # creating and destroying site and horizon column vectors
    site(test)$a <- "foo"
    site(test)[["b"]] <- "bar"
    horizons(test)$c <- "baz"
    horizons(test)[["d"]] <- "qux"

    expect_equal(test$a, rep("foo", length(test)))
    expect_equal(test$b, rep("bar", length(test)))
    expect_equal(test$c, rep("baz", nrow(test)))
    expect_equal(test$d, rep("qux", nrow(test)))

    site(test)$a <- NULL
    site(test)[["b"]] <- NULL
    test$c <- NULL
    horizons(test)$d <- NULL

    expect_null(test$a)
    expect_null(test$b)
    expect_null(test$c)
    expect_null(test$d)

    # add some horizon data
    value <- test_object(data.frame(
                          id = as.character(2:3),
                          hzID = as.character(20:21),
                          hzclass = letters[1:2]
                         ), use_class)

    switch(
      use_class,
      # data.frame and tbl_df
      expect_silent({
        horizons(test) <- value
      }),
      # note: data.table sorts correctly without invoking re-order
      "data.table" = {
        expect_silent( { horizons(test) <- value } )
      }
    )

    expect_equal(horizons(test)[[idname(test)]], as.character(cc(lapply(1:4, rep, 10))))

    expect_equal(horizons(test)[[horizonDepths(test)[2]]], cc(rep(1:10, 4)))

    # deliberately mess up order without adding anything
    expect_silent(horizons(test) <- horizons(test)[c(1:10,20:30,11:19,31:40),])

    # horizons<- fixes the order
    expect_true(spc_in_sync(test)$valid)

    # if there is no new data and IDs don't match, a message is issued and object is unchanged
    hzbad <- horizons(test)
    hzbad[[idname(test)]] <- paste0("h",1:nrow(hzbad))
    expect_message(horizons(test) <- hzbad , "Some profile IDs in input data are not present in object and no new columns to merge. Doing nothing.")

    ## make sample data using current class type
    data(sp1, package = 'aqp')
    sp1df <- aqp:::.as.data.frame.aqp(sp1, use_class, stripFactors = TRUE)
    depths(sp1df) <- id ~ top + bottom
    site(sp1df) <- ~ group

    # add real coordinates
    sp1df$x <- seq(-119, -120, length.out = length(sp1df))
    sp1df$y <- seq(38, 39, length.out = length(sp1df))

    ## tests

    test_that(sprintf("SPC construction from a %s", use_class), {

      # did it work?
      expect_true(inherits(sp1df, 'SoilProfileCollection'))

      # ID correctly initialized?
      expect_equal(idname(sp1df), 'id')
      expect_true(length(profile_id(sp1df)) == length(sp1df))

      # ID in the correct order?
      expect_identical(profile_id(sp1df), site(sp1df)[[idname(sp1df)]])

      # depth names?
      expect_equal(horizonDepths(sp1df), c('top', 'bottom'))

      # site-level attributes correctly initialized?
      expect_true(length(sp1df$group) == length(sp1df))

      # correct number of profiles and horizons?
      expect_equal(length(sp1df), 9)
      expect_equal(nrow(sp1df), 60)

      # this should work
      foo <- explainPlotSPC(sp1df)
      expect_true(is.list(foo))
    })

    test_that(sprintf("SPC diagnostics and restrictions (%s)", use_class), {
      # diagnostic & restriction slot should be initialized as an empty data.frame
      sp1.dh <- diagnostic_hz(sp1df)
      expect_true(inherits(sp1.dh, 'data.frame'))
      expect_equal(nrow(sp1.dh), 0)

      sp1.rh <- restrictions(sp1df)
      expect_true(inherits(sp1.rh, 'data.frame'))
      expect_equal(nrow(sp1.rh), 0)
    })

    test_that(sprintf("SPC data.frame interface (%s)", use_class), {

      # init site-level data
      sp1df$x <- seq(-119, -120, length.out = length(sp1df))
      sp1df$y <- seq(38, 39, length.out = length(sp1df))

      # init hz-level data
      sp1df$z <- runif(n = nrow(sp1df))

      expect_equal(length(sp1df$x), length(sp1df))
      expect_equal(length(sp1df$z), nrow(sp1df))
    })

    test_that(sprintf("SPC deconstruction into a data.frame (%s)", use_class), {

      # do it here
      h <- horizons(sp1df)
      s <- site(sp1df)
      d <- aqp:::.as.data.frame.aqp(as(sp1df, 'data.frame'), use_class)

      expect_true(inherits(h, use_class))
      expect_true(inherits(s, use_class))
      expect_true(inherits(d, use_class))
    })


    test_that(sprintf("SPC deconstruction into a list (%s)", use_class), {

      # do it here
      l <- as(sp1df, 'list')

      # result should be a list
      expect_true(inherits(l, 'list'))

      # there should be no NULL data, e.g. missing slots
      res <- sapply(l, is.null)
      expect_false(any(res))

      # check internals
      expect_equivalent(l$idcol, idname(sp1df))
      expect_equivalent(l$hzidcol, hzidname(sp1df))
      expect_equivalent(l$depthcols, horizonDepths(sp1df))
      expect_equivalent(l$metadata, metadata(sp1df))

      expect_equivalent(l$horizons, horizons(sp1df))
      expect_equivalent(l$site, site(sp1df))
      expect_equivalent(l$sp, sp1df@sp)
      expect_equivalent(l$diagnostic, diagnostic_hz(sp1df))
      expect_equivalent(l$restrictions, restrictions(sp1df))

      # check internals after [-subsetting
      sp1.sub <- sp1df[2:3,]
      # none of these slots should change, the others will be subset
      # verifying these are transferred ensures key info slots are handled
      # by the SPC subset method
      expect_equivalent(l$idcol, idname(sp1.sub))
      expect_equivalent(l$hzidcol, hzidname(sp1.sub))
      expect_equivalent(l$depthcols, horizonDepths(sp1.sub))

      # TODO: better checking of metadata consistency
      #       offload more important things to slots
      #       use units package for depth units, and offer conversion?
      #       inches to centimeters could be usefl
      expect_equivalent(names(l$metadata), names(metadata(sp1.sub)))
    })


    test_that(sprintf("SPC subsetting (%s)", use_class), {

      # profile subsets
      expect_true(inherits(sp1df[1, ], 'SoilProfileCollection'))
      expect_true(inherits(sp1df[1:5, ], 'SoilProfileCollection'))

      # profile and horizon subsets
      expect_true(inherits(sp1df[1, 1], 'SoilProfileCollection'))

      # horizon subsets
      expect_true(inherits(sp1df[, 2], 'SoilProfileCollection'))

      # there should only be 1 profile and 1 horizon
      expect_equal(length(sp1df[1, 1]), 1)
      expect_equal(nrow(sp1df[1, 1]), 1)

      # there should be 5 profiles and 1 horizon / profile
      expect_equal(length(sp1df[1:5, 1]), 5)
      expect_equal(nrow(sp1df[1:5, 1]), 5)

    })

    test_that(sprintf("SPC subsetting with tidy verbs (%s)", use_class), {

      # filter works as expected
      expect_equal(length(subset(sp1df, structure_type == "PL")), 1)

      # ensure multiple expressions yields same result as single expression
      l1 <-  subset(sp1df, !is.na(texture),
                    prop > mean(prop, na.rm = TRUE))
      l2 <-  subset(sp1df, !is.na(texture) &
                    prop > mean(prop, na.rm = TRUE))
      expect_equivalent(length(l1), length(l2))

      # mixing of site and horizon level expressions is the intersection
      l1 <- subset(sp1df, group == 2, prop > mean(prop, na.rm = TRUE))
      expect_equivalent(length(l1), 4)

      # grepSPC works as expected
      expect_equal(length(grepSPC(sp1df, texture, "SCL")), 1)

      # subApply works as expected
      expect_equal(length(subApply(sp1df, function(p)  TRUE)), length(sp1df))
    })

    if (use_class == "data.table") {
      test_that("data.table specific", {
        expect_equal(min(sp1df), 59)
        expect_equal(max(sp1df), 240)
      })
    }
  })

})

