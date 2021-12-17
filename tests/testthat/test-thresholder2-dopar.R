
test_that("create a thresholder2 object", {

    expect_silent(t.rf <- thresholder2(model.rf, thr.interval = 0.005, final = TRUE))
    expect_equal(nrow(t.rf$thrs), 10)
    expect_equal(ncol(t.rf$thrs), 8)
    expect_false(anyNA(t.rf$thrs))
    expect_equal(nrow(t.rf$thresholder), 201)
    expect_equal(ncol(t.rf$thresholder), 8)
    expect_false(anyNA(t.rf$thresholder))

    expect_s3_class(plot(t.rf, select.thr = 3:7), "ggplot")

    t.rf.df <- as.data.frame(t.rf)
    expect_s3_class(t.rf.df, "data.frame")

    expect_output(thr.rf <- summary(t.rf))
    expect_length(thr.rf, 1)
    expect_true(thr.rf >= 0 && thr.rf <= 1)

    expect_silent(t.rf <- thresholder2(model.rf, thr.method = 1:6, final = FALSE))
    expect_equal(nrow(t.rf$thrs), 18)
    expect_equal(ncol(t.rf$thrs), 9)
    expect_false(anyNA(t.rf$thrs))
    expect_equal(nrow(t.rf$thresholder), 303)
    expect_equal(ncol(t.rf$thresholder), 8)
    expect_false(anyNA(t.rf$thresholder))

    expect_s3_class(plot(t.rf, select.thr = "all", statistics = "all", display.final = FALSE), "ggplot")

    # test setThresholder
    expect_output(ts.rf <- summary(t.rf))
    expect_silent(model.rf2 <- setThreshold(model.rf, ts.rf))
    expect_equal(length(model.rf), length(model.rf2) - 1)
    expect_false(anyNA(model.rf2$pred$pred))

    expect_silent(model.rf2 <- setThreshold(model.rf, 0.4))
    expect_equal(length(model.rf), length(model.rf2) - 1)
    expect_false(anyNA(model.rf2$pred$pred))
})


test_that("test check.thr.cutoff", {
    expect_equal(check.thr.cutoff(1:3, letters[1:5]), letters[1:3])
    expect_equal(check.thr.cutoff("a", letters[1:5]), "a")
    expect_equal(check.thr.cutoff("all", letters[1:5]), letters[1:5])
})


test_that("test cost", {
    expect_equal(round(cost(0.8, 0.8, 0.7, c(.9, .6, .8, .6), c(.7, .8, .6, .9)), 4),
                 c(0.7091, 0.4727, 0.5777, 0.5121))
})
