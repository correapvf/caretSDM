
test_that("Parallel computation", {
    skip_on_cran()

    # skip if package is not installed (else package is not loaded in all cores, and error is returned)
    skip_if_not_installed("caretSDM")
    
    # skip("test")

    suppressMessages(library(doParallel))
    n_cores <- detectCores() / 2
    cl <- makePSOCKcluster(n_cores)
    registerDoParallel(cl)


    cat("\n\nTesting with", n_cores, "cores\n")
    test_dir("./", filter = "dopar$")
    cat("\nFinished parallel tests!\n\n")


    stopCluster(cl)
    registerDoSEQ()
})
