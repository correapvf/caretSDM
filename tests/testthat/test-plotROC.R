
test_that("create a plotROC object", {

    expect_silent(r.rf <- ROCcurve(model.rf, testdata, calc.train = FALSE, use.pROC = TRUE, errorFunction = NULL))
    expect_true(nrow(r.rf$dat.roc) > 0)
    expect_equal(ncol(r.rf$dat.roc), 5)
    expect_false(anyNA(r.rf$dat.roc))
    expect_equal(nrow(r.rf$thrs), 0)
    expect_equal(nrow(r.rf$coords), 0)
    expect_s3_class(plot(r.rf), "ggplot")

    thrs <- list(thresholder2(model.rf), thresholder2(model.brt))
    expect_silent(r <- ROCcurve(models, thrs = thrs, testdata = testdata))
    expect_true(nrow(r$dat.roc) > 0)
    expect_false(anyNA(r$dat.roc))
    expect_true(nrow(r$thrs) > 0)
    expect_false(anyNA(r$thrs))
    expect_true(nrow(r$coords) > 0)
    expect_false(anyNA(r$coords))

    expect_s3_class(plot(r, select.thr = 3:7), "ggplot")
    expect_s3_class(plot(r, type = "test", select.thr = 3:7, plot_ci = FALSE, add.text = FALSE), "ggplot")

})


test_that("test roc2 output", {
    expect_silent(x <- roc2(c('a', 'a', 'p', 'p', 'p', 'a'), c(.2, .3, .8, .9, .7, .1), seq(0, 1, .1), c('p', 'a')))
    expect_equal(x$se, c(1, 1, 1, 1, 1, 1, 1, 2 / 3, 1 / 3, 0, 0))
    expect_equal(x$sp, c(0, 1 / 3, 2 / 3, 1, 1, 1, 1, 1, 1, 1, 1))
})
