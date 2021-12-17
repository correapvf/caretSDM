
test_that("create maxent model", {
    expect_silent(mx <- maxent(Class ~ ., traindata))

    expect_silent(pred <- predict(mx, thrtype = "Sens=Spec", clamp = FALSE))
    expect_length(pred, nrow(traindata))
    expect_false(anyNA(pred))
    expect_silent(pred <- predict(mx, testdata, type = "prob"))
    expect_equal(nrow(pred), nrow(testdata))
    expect_false(anyNA(pred))

    expect_s3_class(plot(mx, testdata), "roc")
    expect_silent(vmx <- var_imp(mx, itype = "contribution"))
    expect_silent(vmx <- var_imp(mx))
    expect_equal(nrow(vmx), 7)
    expect_false(anyNA(pred))
})

test_that("crate maxent model using caret and trainControlSDM", {
    # treat absence data as background
    expect_silent(control <- trainControlSDM(number = 5, y = traindata$Class, presence.only = TRUE))
    expect_length(control, 27)
    expect_length(control$index, 5)

    expect_silent(model.maxent <- train(Class ~ ., data = traindata,
                      method = maxentCaret, metric = "ROC", trControl = control,
                      tuneGrid = expand.grid(reg = c("lq", "l"), beta = c(1, 2)),
                      categorical = c("TwoFactor1", "TwoFactor2")
                      ))

    expect_length(model.maxent$finalModel, 16)
    expect_equal(nrow(model.maxent$results), 4)
    expect_equal(ncol(model.maxent$results), 14)
    expect_false(anyNA(model.maxent$results))
    expect_equal(nrow(model.maxent$resample), 5)
    expect_equal(ncol(model.maxent$resample), 7)
    expect_false(anyNA(model.maxent$resample))
})
