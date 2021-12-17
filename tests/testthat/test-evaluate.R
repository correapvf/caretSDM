
test_that("evaluate model: classification", {
    expect_silent(e <- evaluate(models, testdata, errorFunction = NULL))
    expect_true(all(e$eval$error == 0))
    expect_true(nrow(e$resample) == 0)
    expect_s3_class(plot(e), "ggplot")

    expect_silent(e <- evaluate(models, testdata))
    expect_equal(nrow(e$eval), 12)
    expect_equal(ncol(e$eval), 5)
    expect_false(anyNA(e$eval))
    expect_equal(nrow(e$resample), 20)
    expect_equal(ncol(e$resample), 6)
    expect_false(anyNA(e$resample))

    expect_s3_class(plot(e), "ggplot")
    expect_s3_class(plot(e, plot_errorbar = FALSE), "ggplot")
    expect_s3_class(dotplot(e, data = "train"), "ggplot")
    expect_s3_class(dotplot(e, metric = "ROC"), "ggplot")
    expect_s3_class(pairs(e), "ggmatrix")
})

test_that("evaluate model: regression", {
    expect_silent(e.rf <- evaluate(model.rf.reg))
    expect_silent(e.brt <- evaluate(model.brt.reg))
    expect_silent(e <- c(e.rf, e.brt))
    expect_false(anyNA(e$eval))
    expect_false(anyNA(e$resample))
    expect_s3_class(plot(e), "ggplot")
})
