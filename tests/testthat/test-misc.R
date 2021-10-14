
test_that("Test create.test.index", {
    set.seed(1)
    expect_silent(testindex <- create.test.index(testdata$Class))
    expect_length(testindex, 25)
    expect_true(all(sapply(testindex, length) == 36))
})

test_that("Create a confusionMatrix2 object", {
    expect_silent(cmatrix <- confusionMatrix2(models, testdata))
    expect_length(cmatrix, 2)
    expect_length(cmatrix[[1]], 6)
})

test_that("Test spatial functions", {
    set.seed(1)
    coords <- data.frame(lon = runif(nrow(traindata), -10, 10), lat = runif(nrow(traindata), -10, 10))
    expect_silent(ENM <- blockENM2fold(method = "block", traindata$Class, coords, presence.level = "Class1"))
    
    coords <- sp::SpatialPoints(coords)
    set.seed(1)
    sptrain <- sp::SpatialPoints(data.frame(lon = runif(nrow(testdata), -10, 10), lat = runif(nrow(testdata), -10, 10)))
    set.seed(1)
    expect_warning(sb <- blockCV::spatialBlock(coords, theRange = 200000, biomod2Format = FALSE,
                                    progress = FALSE, verbose = FALSE))

    expect_silent(sbindex <- blockCV2fold(sb))
    expect_length(sbindex, 2)
    expect_length(sbindex$index, 5)
    expect_length(sbindex$indexOut, 5)
    
    expect_warning(sbindex.test <- create.test.index.blockCV(sptrain, sb))
    expect_length(sbindex.test, 5)
})
