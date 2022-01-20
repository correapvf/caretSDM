
#' @rdname evaluate
#' @export
evaluate <- function(x, ...) UseMethod("evaluate")

#' Evaluate a caret model
#'
#' Evaluate a caret model using the metrics of \code{summaryFunction}.
#'
#' @param x A model returned by \code{\link[caret]{train}}.
#' @param testdata A data.frame with test values to be evaluated. If \code{NULL}, training values will be used.
#' @param testy A factor of response variable of \code{testdata}. If \code{NULL}, it will be guessed from
#' \code{testdata} data.frame.
#' @param testindex A list with rows index of testdata for each resample, preferably an output of
#' \code{\link{create.test.index}} or \code{\link{create.test.index.blockCV}}. If \code{NULL}, indexes
#' are create based on the same methods provided by \code{trainControl} used in the model.
#' Only used if \code{error_bar = TRUE}.
#' @param summaryFunction A Summary function (one of \code{\link[caret]{defaultSummary}}) that calculate the metrics.
#' If \code{NULL}, the summaryFunction provided in the model will be used.
#' @param calc.train logical. Evaluate training data? If \code{FALSE}, only test data is evaluated.
#' @param errorFunction A function used to calculate errors across resamples. Default is 95\% confidence interval.
#' If \code{NULL}, errors are not calculated.
#' @return An S3 object of class 'evaluate.train', including:
#' \itemize{
#'   \item eval - A data.table in the long format with data type, metrics, values and error across resamples.
#'   If \code{errorFunction != NULL}, values are means across resamples.
#'   \item resample - A data.table with metrics in each resample.
#'   }
#' @note This function is somewhat similar to \code{\link[caret]{resamples}}, however this function
#' supports evaluation using test data, a custom threshold (use \code{setThreshold}), or a different summaryFunction.
#' @seealso \code{\link{confusionMatrix2}} \code{\link{ROCcurve}}
#' @examples
#' \dontrun{
#' evaluate(model)
#'
#' # evaluate test data only
#' testindex <- create.test.index(testdata$response) # get response of testdata
#' evaluate(model, testdata, testindex = testindex, calc.train = FALSE)
#'
#' # for multiple models
#' models <- list(model1, model2, model3)
#' e <- evaluate(models, summaryFunction = twoClassSummary)
#' plot(e)
#' dot_plot(e, data.type = "test", metric = "ROC")
#' pairs_plot(e)
#' pairs_plot(e, fixed_axis = FALSE)
#' }
#' @rdname evaluate
#' @export
evaluate.train <- function(x, testdata = NULL, testy = NULL, testindex = NULL,
                           summaryFunction = NULL, calc.train = TRUE, errorFunction = ci_95, ...) {


    type.class <- if (x$modelType == "Classification") TRUE else FALSE

    if ((x$control$savePredictions == "none" || x$control$savePredictions == FALSE)) {
        if (is.null(testdata))
            stop("'savePredictions' should be TRUE, 'all', or 'final'
                 to calculate training data.")

        if (calc.train) {
            warning("Skipping training data. Set 'savePredictions'to TRUE, 'all', or 'final'
             to calculate training data.")
            calc.train <- FALSE
        }
    }

    if (calc.train) check_train(x)

    if (is.null(summaryFunction)) {
        summaryFunction <- x$control$summaryFunction
    }

    # check index
    if (!is.null(testdata)) {

        # get predictions
        if (type.class) {
            testy <- get.response(x, testdata, testy)
        } else {
            # get the outcome variable for testdata data
            # as outcome is not a factor, we can't use 'get.response'
            if (is.null(testy)) {
                if (inherits(x, "train.formula")) {
                    pred.name <- all.vars(x$terms)[attr(x$terms, "response")]
                } else {
                    stop("Either provide 'testy' or train using a formula")
                }
                testy <- testdata[[pred.name]]
            }
        }
    }


    if (is.null(errorFunction)) {
        out <- eval_ci_false(x, testdata, testy, summaryFunction, type.class, calc.train)
    } else {
        out <- eval_ci_true(x, testdata, testy, testindex, summaryFunction,
                            errorFunction, type.class, calc.train)
    }

    class(out) <- "evaluate.train"
    return(out)
}



#' @rdname combine
#' @export
evaluate.list <- function(x, ...) {

    check_list(x)
    x <- check_names(x)

    x <- lapply(x, evaluate.train, ...)
    return(c_evaluate.train(x))
}



#' @param x An object returned by \code{evaluate}.
#' @param plot_errorbar logical. Should plot error bars?
#' @param ... ignored
#' @rdname evaluate
#' @export
plot.evaluate.train <- function(x, plot_errorbar = TRUE, ...) {
    x <- x$eval
    dat <- levels(x$data)
    colors <- c("deepskyblue", "brown1")[c("test", "train") %in% dat]

    fig <- ggplot(x, aes(x = metric, y = value, fill = data)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            facet_wrap(~method) + scale_fill_manual(values = colors) + theme_bw()

    if (length(dat) == 1) fig <- fig + guides(fill = "none")

    if (diff(range(x$error)) > 0 && plot_errorbar) {
        fig <- fig + geom_errorbar(aes(ymin = value - error, ymax = value + error),
                                   position = position_dodge(0.9), width = 0.25)
    }

    print(fig)
    return(invisible(fig))
}



#' @rdname combine
#' @export
c.evaluate.train <- function(...) {
    x <- list(...)
    check_c(x, "evaluate.train")

    cnames <- lapply(x, function(x) levels(x$eval$metric))
    if (!all(sapply(cnames, FUN = identical, y = cnames[[1]])))
        stop("You should use the same 'summaryFunction' for all models to concatanate.")

    return(c_evaluate.train(x))
}
c_evaluate.train <- function(x) {
    out <- list(eval = rbindlist(lapply(x, `[[`, "eval")),
                resample = rbindlist(lapply(x, `[[`, "resample"))
                )

    class(out) <- "evaluate.train"
    return(out)
}



#' @export
print.evaluate.train <- function(x, ...) {
    cat("Object of type evaluate.train\n")
    cat("Models avaiable:", levels(x$eval$method), "\n\n")
    print(x$eval, nrows = 20)
}



#' @export
dot_plot <- function(x, ...) UseMethod("dot_plot")

#' @param data Which data type to plot? Should be either 'train' or 'test'.
#' When \code{NULL}, it defaults to test data, if present.
#' @param metric A character, indicating which metric to plot. If \code{NULL},
#' only the first metric is plotted. If 'all', all metrics are plotted.
#' @rdname evaluate
#' @export
dot_plot.evaluate.train <- function(x, data = NULL, metric = "all", ...) {
    metrics <- metric; data.type <- data; rm(metric, data)
    if (is.null(data.type)) data.type <- levels(x$resample$data)[1]
    if (is.null(metrics)) metrics <- x$eval$metric[1]

    if (metrics == "all") {
        tmp <- x$eval[data == data.type]
    } else {
        tmp <- x$eval[data == data.type & metric == metrics]
    }

    fig <- ggplot(tmp, aes(x = value, y = method)) + geom_point() +
        geom_errorbar(aes(xmin = value - error, xmax = value + error), width = 0.1) +
        facet_wrap(~metric, scales = "free_x") +
        xlab(paste(data.type, "data")) +
        theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

        print(fig)
        return(invisible(fig))
}


#' @export
pairs_plot <- function(x, ...) UseMethod("pairs_plot")

#' @param fixed_axis logical. Should axis in all plots be fixed to the same limits?
#' @rdname evaluate
#' @export
pairs_plot.evaluate.train <- function(x, data = NULL, metric = NULL, fixed_axis = TRUE, ...) {
    metrics <- metric; data.type <- data; rm(metric, data)
    if (is.null(metrics)) metrics <- as.character(x$eval$metric[1])
    if (metrics == "all") {
        metrics <- x$eval$metric[1]
        warning("'all' is not avaiable in this plot, defauting to the first metric")
    }

    if (is.null(data.type)) data.type <- levels(x$resample$data)[1]

    tittle_plot <- paste0(metrics, " - ", data.type, " data")

    if (length(x$method) == 1) {
        tmp <- x$resample[[metrics]][x$resample$data == data.type]

        fig <- ggplot() + geom_point(aes(y = tmp, x = seq_along(tmp))) +
            theme_bw() + xlab("Index") + ylab(tittle_plot) + ggtitle(x$resample$method[1]) +
            theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
    } else {
        tmp <- dcast(x$resample, Resample~method, subset = .(data == data.type), value.var = metrics)
        tmp$Resample <- NULL

        if (fixed_axis) {
            fig <- GGally::ggpairs(tmp, lower = list(continuous = limitScatter),
                                   diag = list(continuous = limitDensity))
        } else {
            fig <- GGally::ggpairs(tmp)
        }

        fig <- fig + ggtitle(tittle_plot) + theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
    }

    print(fig)
    return(invisible(fig))
}





############ helper functions

eval_ci_false <- function(model, testdata, testy, summaryFunction, type.class, calc.train) {
    if (calc.train) {
        if (type.class) {
            pred <- data.frame(obs = model$trainingData$.outcome,
                               predict2(model, type = "both"))
        } else {
            pred <- data.frame(obs = model$trainingData$.outcome,
                               pred = predict(model))
        }
        out <- summaryFunction(pred, model$levels, model$method)
        out <- data.table(method = factor(model$modelInfo$label),
                          data = factor("train"),
                          metric = factor(names(out)),
                          value = out,
                          error = 0)
    } else {
        out <- data.table()
    }

    if (!is.null(testdata)) {
        if (type.class) {
            pred <- data.frame(obs = testy,
                               predict2(model, testdata, type = "both"))
        } else {
            pred <- data.frame(obs = testy,
                               pred = predict(model, testdata))
        }
        out2 <- summaryFunction(pred, model$levels, model$method)
        out2 <- data.table(method = factor(model$modelInfo$label),
                           data = factor("test"),
                           metric = factor(names(out2)),
                          value = out2,
                          error = 0)
        out <- rbind(out, out2)
    }

    return(list(eval = out, resample = data.table()))
}

eval_ci_true <- function(model, testdata, testy, testindex, summaryFunction, errorFunction, type.class, calc.train) {

    SDcols <- if (type.class) c("pred", "obs", model$levels) else c("pred", "obs")

    # calculate for train data
    if (calc.train) {
        tmp2 <- merge(model$pred, model$bestTune)
        setDT(tmp2)
        out <- calc_metric(tmp2, summaryFunction, model, "train", errorFunction, SDcols)
        x <- out[[2]]; out <- out[[1]]
    } else {
        out <- data.table()
        x <- data.table()
    }


    # calculate for test data
    if (!is.null(testdata)) {

        if (is.null(testindex))
            testindex <- createIndex(testy, model$control$method, model$control$number,
                                     model$control$repeats, model$control$p)
        # get predictions
        if (type.class) {
            pred <- data.frame(obs = testy,
                               predict2(model, testdata, type = "both"))
        } else {
            pred <- data.frame(obs = testy,
                               pred = predict(model, testdata))
        }

        # resample predictions
        tmp2 <- lapply(testindex, function(x, y) y[x, ], y = pred)
        tmp2 <- rbindlist(tmp2, idcol = "Resample")

        # calculate metrics
        out2 <- calc_metric(tmp2, summaryFunction, model, "test", errorFunction, SDcols)
        x2 <- out2[[2]]; out2 <- out2[[1]]
        out2$error[is.na(out2$error)] <- 0
        out <- rbind(out, out2)
        x <- rbind(x, x2)
    }
    return(list(eval = out, resample = x))
}

calc_metric <- function(tmp2, summaryFunction, model, dataset, errorFunction, SDcols) {
    # calculate metrics
    x <- tmp2[, as.list(summaryFunction(as.data.frame(.SD), model$levels, model$method)),
              by = "Resample", .SDcols = SDcols]

    # average over resamples
    outmean <- apply(x[, -"Resample"], 2, mean)
    outerror <- apply(x[, -"Resample"], 2, errorFunction)

    # format output
    out <- data.table(method = factor(model$modelInfo$label),
                      data = factor(dataset),
                      metric = factor(names(outmean)),
                      value = outmean,
                      error = outerror)

    x <- data.table(method = factor(model$modelInfo$label),
                    data = factor(dataset), x)
    return(list(out, x))
}


limitScatter <- function(data, mapping, ...) {
    lims <- range(data, na.rm = TRUE)
    ggplot(data = data, mapping = mapping, ...) +
        geom_point(...) +
        scale_y_continuous(limits = lims) +
        scale_x_continuous(limits = lims)
}

limitDensity <- function(data, mapping, ...) {
    lims <- range(data, na.rm = TRUE)
    ggplot(data = data, mapping = mapping, ...) +
        geom_density(...) +
        scale_x_continuous(limits = lims)
}
