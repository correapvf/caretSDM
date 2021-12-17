
#' @rdname maxent
#' @export
maxentCaret <- list(type = "Classification",
                    library = c("data.table", "caretSDM"),
                    label = "MaxEnt",
                    loop = NULL
                    )

# paramters
maxentCaret$parameters <- data.frame(
    parameter = c("reg", "beta"),
    class = c("character", "numeric"),
    label = c("Regularization Features", "Beta_Multiplier")
    )


# grid function
maxentCaret$grid <- function(x, y, len = NULL, search = "grid") {
    reg.val <- switch(findInterval(nrow(x), c(10, 15, 80)) + 1,
                  "l", "lq", "lqh", "lqph")

    if (search == "grid") {
        out <- expand.grid(reg = reg.val,
                           beta = c(0.5, 1, 2))
    } else {
        if (is.null(len)) len <- 5
        out <- expand.grid(reg = reg.val,
                          beta = stats::runif(len, min = 0.5, max = 3))
    }
    return(out)
}


maxentCaret$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    maxent.matrix(x, y, reg = param$reg, beta = param$beta, ...)
}


maxentCaret$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata)
}


maxentCaret$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata, type = "prob")
}


# sort first with less regularization features, than with beta closest to 1
maxentCaret$sort <- function(x) x[order(nchar(as.character(x$reg)), abs(x$beta - 1)), ]

maxentCaret$varImp <- function(x, ...) {
    if (hasArg(type)) {
        if (!(type %in% c("contribution", "permutation"))) stop("Type must be 'contribution' or 'permutation'")
        out <- var_imp(x, type)
    } else {
        out <- var_imp(x, type = "contribution")
    }
    return(out)
}

maxentCaret$levels <- function(x) x$classes
