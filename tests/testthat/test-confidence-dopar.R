
test_that("create a confidence map", {
    expect_silent(conf <- confidence_map(model.rf.reg, r.reg, nrep = 3, progress = FALSE))
    expect_silent(conf <- confidence_map(model.rf, r.class, nrep = 3, progress = FALSE))
    expect_silent(conf <- confidence_map(models, r.class, nrep = 3, progress = FALSE))
    expect_length(conf, 2)
    expect_named(conf, c("means", "sds"))
    expect_s4_class(conf$means, "RasterStack")
    expect_s4_class(conf$sds, "RasterStack")
})
