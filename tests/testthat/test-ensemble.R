
test_that("evaluate ensemble model", {
    control2 <- control
    control2$index <- model.rf$control$index
    control2$indexOut <- model.rf$control$indexOut
    model.brt2 <- caret::train(Class ~ ., data = traindata, method = "gbm", metric = "ROC", trControl = control2, verbose = FALSE)
    expect_silent(ens <- createEnsemble(list(model.rf, model.brt2), calc.pred = TRUE))
    # browser()
    expect_silent(e <- evaluate(ens, testdata))
    expect_false(anyNA(e$eval))
    expect_false(anyNA(e$resample))
    expect_s3_class(plot(e), "ggplot")

    expect_silent(r <- ROCcurve(ens, testdata = testdata))
    expect_true(nrow(r$dat.roc) > 0)
    expect_false(anyNA(r$dat.roc))
    expect_true(nrow(r$coords) > 0)
    expect_false(anyNA(r$coords))
    expect_s3_class(plot(r), "ggplot")
})

test_that("create a ensemble: classification, weighted_mean", {
    expect_silent(ens <- createEnsemble(models, ensemble_method = "weighted_mean", metric = "ROC"))
    expect_silent(conf <- confidence_map(ens, r.class, nrep = 3, progress = FALSE))
    expect_s4_class(conf, "RasterLayer")
    expect_false(anyNA(as.vector(conf)))

    expect_silent(preds <- predict(ens, testdata))
    expect_length(preds, nrow(testdata))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: regression, weighted_mean", {
    expect_silent(ens <- createEnsemble(list(model.rf.reg, model.brt.reg),
                  ensemble_method = "weighted_mean"))

    expect_silent(preds <- predict(ens, testdata.reg))
    expect_length(preds, nrow(testdata.reg))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: classification, mean", {
    expect_silent(ens <- createEnsemble(models, ensemble_method = "mean", metric = "Sens"))

    expect_silent(preds <- predict(ens, testdata))
    expect_length(preds, nrow(testdata))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: regression, mean", {
    expect_silent(ens <- createEnsemble(list(model.rf.reg, model.brt.reg),
                                        ensemble_method = "mean"))

    expect_silent(conf <- confidence_map(ens, r.reg, nrep = 3, progress = FALSE))
    expect_s4_class(conf, "RasterLayer")
    expect_false(anyNA(as.vector(conf)))

    expect_silent(preds <- predict(ens, testdata.reg))
    expect_length(preds, nrow(testdata.reg))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: classification, median", {
    expect_silent(ens <- createEnsemble(models, ensemble_method = "median", metric = "ROC"))

    expect_silent(preds <- predict(ens, testdata))
    expect_length(preds, nrow(testdata))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: regression, median", {
    expect_silent(ens <- createEnsemble(list(model.rf.reg, model.brt.reg),
                                        ensemble_method = "median"))

    expect_silent(conf <- confidence_map(ens, r.reg, nrep = 3, progress = FALSE))
    expect_s4_class(conf, "RasterLayer")
    expect_false(anyNA(as.vector(conf)))

    expect_silent(preds <- predict(ens, testdata.reg))
    expect_length(preds, nrow(testdata.reg))
    expect_false(anyNA(preds))
})

test_that("create a ensemble: classification, number_votes", {
    expect_silent(ens <- createEnsemble(models, ensemble_method = "number_votes", metric = "ROC"))
    # browser()
    expect_silent(conf <- confidence_map(ens, r.class, nrep = 3, progress = FALSE))
    expect_s4_class(conf, "RasterLayer")
    expect_false(anyNA(as.vector(conf)))

    expect_silent(preds <- predict(ens, testdata))
    expect_length(preds, nrow(testdata))
    expect_false(anyNA(preds))
})
