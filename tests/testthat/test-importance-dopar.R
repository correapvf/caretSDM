
test_that("create a var_imp object", {
    
    expect_silent(v.rf <- varImp2(model.rf))
    expect_equal(nrow(v.rf$importance), 7)
    expect_equal(ncol(v.rf$importance), 5)
    expect_false(anyNA(v.rf$importance))
    expect_true(is.null(v.rf$resamples))
    
    expect_silent(v.brt <- varImp2(model.brt))
    expect_silent(vc <- c(v.rf, v.brt))
    
    expect_silent(vs <- varImp2(models, nperm = 3))
    expect_equal(nrow(vs$importance), 14)
    expect_equal(ncol(vs$importance), 5)
    expect_false(anyNA(vs$importance))
    expect_equal(nrow(vs$resamples), 6)
    expect_equal(ncol(vs$resamples), 8)
    expect_false(anyNA(vs$resamples))
    
    expect_silent(s.vs <- summary(vs))
    expect_equal(nrow(s.vs), 14)
    expect_equal(ncol(s.vs), 3)
    expect_false(anyNA(s.vs))
    expect_silent(s.vs <- summary(vs, scale = FALSE))
    expect_equal(nrow(s.vs), 14)
    expect_equal(ncol(s.vs), 4)
    expect_false(anyNA(s.vs))
    expect_s3_class(plot(vs), "ggplot")
    expect_s3_class(plot(vs, FALSE, FALSE), "ggplot")
    
})

