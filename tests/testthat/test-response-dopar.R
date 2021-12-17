
test_that("Create a response.train object", {
    expect_silent(r <- response(models, n = 3))

    expect_equal(nrow(r$num), 30)
    expect_equal(ncol(r$num), 5)
    expect_false(anyNA(r$num))
    expect_equal(nrow(r$num_resample), 150)
    expect_equal(ncol(r$num_resample), 5)
    expect_false(anyNA(r$num_resample))

    expect_equal(nrow(r$fact), 10)
    expect_equal(ncol(r$fact), 5)
    expect_false(anyNA(r$fact))
    expect_equal(nrow(r$fact_resample), 50)
    expect_equal(ncol(r$fact_resample), 5)
    expect_false(anyNA(r$fact_resample))

    expect_s3_class(plot(r), "ggplot")
    expect_s3_class(plot(r, plot_errorbar = FALSE, plot_rugs = FALSE), "ggplot")
})
