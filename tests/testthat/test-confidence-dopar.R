
test_that("create a confidence map", {
    expect_silent(conf <- confidence_map(model.rf.reg, r.reg, nrep = 3, progress = FALSE))
    expect_equal(raster::ncell(conf), 2500)
    expect_s4_class(conf, "RasterLayer")

    expect_silent(conf <- confidence_map(model.rf, r.class, nrep = 3, progress = FALSE))
    expect_equal(raster::ncell(conf), 2500)
    expect_s4_class(conf, "RasterLayer")

    expect_silent(conf <- confidence_map(models, r.class, nrep = 3, progress = FALSE))
    expect_equal(raster::ncell(conf), 2500)
    expect_equal(raster::nlayers(conf), 2)
    expect_s4_class(conf, "RasterStack")

    ens <- createEnsemble(models, ensemble_method = "weighted_mean", metric = "ROC")
    expect_silent(conf <- confidence_map(ens, r.class, nrep = 3, progress = FALSE))
    expect_equal(raster::ncell(conf), 2500)
    expect_s4_class(conf, "RasterLayer")
})
