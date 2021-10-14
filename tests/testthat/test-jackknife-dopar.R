
test_that("create a jackknife object", {
    expect_silent(j <- jackknife(models, param_override_only = list(mtry=1), 
                                 param_override_without = list(mtry=4),
                                 errorFunction = sd, progress = FALSE))
    
    expect_equal(nrow(j), 90)
    expect_equal(ncol(j), 6)
    expect_false(anyNA(j))
    
    expect_s3_class(plot(j), "ggplot")
    expect_s3_class(plot(j, metric = "Sens", plot_errorbar = FALSE), "ggplot")
})
