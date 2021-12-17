
#' @rdname varImp2
#' @export
varImp2 <- function(model, ...) UseMethod("varImp2")

#' Calculate variable importance
#'
#' A generic method for calculating variable importance for objects produced by \code{train}.
#'
#' The process of calculating variable importance is different from \code{caret::varImp(model, useModel = TRUE)}.
#' Here we use the same process as described in \code{biomod2::variables_importance},
#' but the function will directly accept a caret model.
#' If available, multiple cores are used to compute correlations.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param nperm Number of permutations for each variable.
#' @param errorFunction A function used to calculate errors. Function must accept \code{na.rm}.
#' Only used if \code{nperm > 1}.
#' @return An S3 object of class 'varImp2', including:
#' \itemize{
#'   \item importance - A data.table with variables importance, importance from 0 to 100,
#'   and errors across permutations.
#'   \item resamples - A data.table with correlations for each permutation.
#'   }
#' @examples
#' \dontrun{
#' v.obj <- varImp2(model)
#' summary(v.obj)
#' summary(v.obj, scale = FALSE)
#' plot(v.obj)
#'
#' # for multiple models
#' v.obj <- varImp2(list(model1, model2, model3), nperm = 25)
#' plot(v.obj, scale = FALSE)
#' }
#' @rdname varImp2
#' @export
varImp2.train <- function(model, nperm = 1, errorFunction = ci_95, ...) {

    check_train(model)
    model.type <- if (model$modelType == "Classification") "prob1" else "raw"

    predref <- predict2(model, type = model.type)

    seed <- utils::tail(model$control$seeds, 1)[[1]]
    coefs <- getcoefs(model)
    `%op%` <- if (model$control$allowParallel && getDoParWorkers() > 1) `%dopar%` else  `%do%`
    trainingData <- as.data.frame(model$trainingData)

    result <- foreach(i = seq(nperm), .combine = "rbind") %:%
        foreach(coef = coefs, .combine = "c") %op% {
            set.seed(seed + i)
            tmpdata <- trainingData[, coefs]
            tmpdata[[coef]] <- sample(tmpdata[[coef]])
            set.seed(NULL)
            predtmp <- predict2(model, tmpdata, type = model.type)
            1 - max(stats::cor(predref, predtmp, method = "pearson", use = "pairwise.complete.obs"), 0)
        }

    if (is.matrix(result)) {

        imp <- apply(result, 2, mean, na.rm = TRUE)
        errors <- apply(result, 2, errorFunction, na.rm = TRUE)

        colnames(result) <- coefs
        res <- data.table(method = factor(model$modelInfo$label), result)
    } else {
        imp <- result
        errors <- 0
        res <- NULL
    }


    scaled <- imp / max(imp) * 100

    out <- list()
    out$importance <- data.table(method = factor(model$modelInfo$label),
                         variable = factor(coefs),
                         importance = imp,
                         scaled = scaled,
                         error = errors)

    out$resamples <- res
    out$nperm <- nperm

    class(out) <- "varImp2"
    return(out)
}

        

#' @rdname combine
#' @export
varImp2.list <- function(model, ...) {

    check_list(model)
    model <- check_names(model)

    x <- lapply(model, varImp2, ...)
    return(c_varImp2(x))
}



#' @param plot_errorbar logical. Should plot error bars? Error bars are only plotted if \code{scaled = FALSE}.
#' @rdname varImp2
#' @export
plot.varImp2 <- function(x, scale = FALSE, plot_errorbar = TRUE, ...) {
    tmp <- summary(x, scale)

    fig <- ggplot(tmp, aes(x = importance, y = variable)) +
        geom_bar(position = position_dodge(), stat = "identity") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
        scale_y_discrete(limits = rev) +
        facet_wrap(~method, scales = "free_x") +
        theme_bw()

    if ("error" %in% colnames(tmp) && plot_errorbar) {
        fig <- fig + geom_errorbar(aes(xmin = importance - error, xmax = importance + error),
                                   width = .25, position = position_dodge(.9))
    }

    print(fig)
    return(invisible(fig))
}



#' @param x,object An object from \code{varImp2}.
#' @param scale logical. Return scaled values from 0 to 100?
#' @param ... ignored
#' @rdname varImp2
#' @export
summary.varImp2 <- function(object, scale = FALSE, ...) {
    out <- copy(object$importance)

    if (scale) {
        out[, c("importance", "error") := NULL]
        setorderv(out, cols = "scaled", order = -1L)
        out[, "variable" := factor(variable, levels = unique(variable))]
        colnames(out)[3] <- "importance"
    } else {
        if (diff(range(out$error)) == 0) out[, "error" := NULL]
        out[, "scaled" := NULL]
        out[, "variable" := factor(variable, levels = unique(variable))]
    }
    setorder(out, method, variable)
    return(out)
}



#' @rdname combine
#' @export
c.varImp2 <- function(...) {
    x <- list(...)
    check_c(x, "varImp2")
    return(c_varImp2(x))
}
c_varImp2 <- function(x) {
    m <- list()
    m$importance <- rbindlist(lapply(x, `[[`, "importance"))
    m$resamples <- rbindlist(lapply(x, `[[`, "resamples"))
    m$nperm <- unlist(lapply(x, `[[`, "nperm"))
    class(m) <- "varImp2"
    return(m)
}



#' @export
print.varImp2 <- function(x, scale = TRUE, ...) {
    cat("Object of type varImp2\n")
    cat("Models avaiable:", levels(x$importance$method), "\n")
    cat("Number of permutations:", x$nperm, "\n\n")
    tmp <- summary(x, scale)
    print(dcast(tmp, method ~ variable, value.var = "importance"), nrows = 20)
}
