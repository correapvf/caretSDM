
globalVariables("m")

#' Set a threshold in a caret model
#'
#' This function will set a probability threshold cutoff to be used in predictions
#' in a two class Classification problem. This threshold is automatically read by \code{\link{predict2}} and other
#' functions in this package when calculating new predictions.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param thr A value between 0 and 1 to be used as a threshold or the
#' output of \code{summary.thresholder2()}.
#' @return The caret model provided with a new item \code{thr} and the
#' \code{pred} data.frame updated, if it exists. If "thr" is a 'data.frame', the finalModel
#' is also updated using new parameters provided.
#' @note Objects \code{results} and \code{resample} remain unchanged. Use \code{\link{evaluate}} to
#' obtain corrected statistics based on the new threshold.
#' @seealso \code{\link{predict2}} \code{\link[caret]{update.train}}
#' @export
setThreshold <- function(model, thr) {

    if (!inherits(model, "train"))
        stop("Model should be an object of class 'train'")

    lvl <- model$levels
    if (!(model$modelType == "Classification" && length(lvl) == 2))
        stop("Model should be a two class Classification problem.")

    if (inherits(thr, "thresholder2")) thr <- summary(thr)
    if (is.data.frame(thr)) {
        newTune <- thr
        newTune$thr <- NULL
        thr <- thr$thr
        model <- stats::update(model, param = newTune)
    } else {
        if (!is.numeric(thr) && length(thr) > 1)
            stop("'thr' should be a single number between [0,1].")
    }

    if (thr < 0 && thr > 1)
        stop("'thr' should be a number between [0,1].")


    model$thr <- thr

    if (!is.null(model$pred)) {
        model$pred$pred <- factor(model$pred[, lvl[1]] > thr,
                                  levels = c(TRUE, FALSE), labels = lvl)
    }

    return(model)
}


#' @rdname predict2
#' @export
predict2 <- function(object, ...) UseMethod("predict2")


#' Extract predictions train objects using a threshold
#'
#' This function is a wrapper around \code{\link[caret]{predict.train}}, but it accepts a
#' threshold cutoff to calculate predictions from class probabilities.
#'
#' Most arguments work the same as \code{\link[caret]{predict.train}}.\cr\cr
#' This function is only useful for two class Classification problems, when using
#' \code{type = "raw"} and a different threshold from the default (which must set using \code{\link{setThreshold}}).
#' Otherwise, the output is the same as \code{predict}.\cr\cr
#' Additionally, you can set \code{type = "both"}, which will return a data.frame with all class probabilities and
#' a prediction, \code{type = "prob1"} to return a single vector with the first class probabilities (assumed
#' to be presence). \code{type = "both1"} will return class prediction and the first class probabilities.\cr\cr
#' The method for RasterStack is a simpler version of \code{raster::predict}, but it may be faster sometimes.
#' @param object,model A model returned by \code{\link[caret]{train}} or a list of models.
#' object can also be a RasterStack from the \code{raster} package, used to create predictions.
#' @param newdata An optional set of data to predict on.
#' @param type One of "raw", "prob", "both", "both1" or "prob1" (see details).
#' @param doclamp logical. Clamp \code{newdata} or \code{RasterStack} based on training data?
#' @param ... Further arguments passed to \code{predict} or method 'train'.
#' @return A vector or data.frame with predictions or probabilities, based on the argument \code{type}. Check details.
#' If \code{model} is a list of models, than a list of predictions is returned.
#' @seealso \code{\link{setThreshold}} c\code{\link{lamp_data}}
#' @rdname predict2
#' @export
predict2.train <- function(object, newdata = NULL, type = "raw", doclamp = FALSE, ...) {

    if (doclamp && !is.null(newdata)) newdata <- clamp_data(object, newdata)

    switch(type,
        raw = {

            if (is.null(object$thr)) {
                out <- predict(object, newdata, "raw", ...)
            } else {
                pred <- predict(object, newdata, "prob", ...)
                out <- factor(pred[, object$levels[1]] > object$thr,
                            levels = c(TRUE, FALSE), labels = object$levels)
            }
        },
        both = {
            pred <- predict(object, newdata, "prob", ...)

            if (is.null(object$thr)) {
                tmp <- predict(object, newdata, "raw", ...)
            } else {
                tmp <- factor(pred[, object$levels[1]] > object$thr,
                            levels = c(TRUE, FALSE), labels = object$levels)
            }
            out <- data.frame(pred = tmp, pred)
        },
        prob1 = {
            out <- predict(object, newdata, "prob", ...)[, object$levels[1]]
        },
        both1 = {
            pred <- predict(object, newdata, "prob", ...)

            if (is.null(object$thr)) {
                tmp <- predict(object, newdata, "raw", ...)
            } else {
                tmp <- factor(pred[, object$levels[1]] > object$thr,
                            levels = c(TRUE, FALSE), labels = object$levels)
            }
            out <- data.frame(pred = tmp, pred[, object$levels[1], drop = FALSE])
        },
        stop("type must be one of 'raw', 'prob', 'both' or 'prob1'.")
    )

    return(out)
}



#' @rdname predict2
#' @export
predict2.list <- function(object, ...) {
    check_list(object)
    object <- check_names(object)

    out <- lapply(object, predict2, ...)
    names(out) <- sapply(object, function(x) x$modelInfo$label)

    return(out)
}



#' @rdname predict2
#' @export
predict2.RasterStack <- function(object, model, doclamp = FALSE, ...) {
    # convert raster to data.frame
    r <- raster2data(object)
    r_index <- as.numeric(row.names(r))

    is.train <- inherits(model, "train")
    if (is.train) {
        allowParallel <- model$control$allowParallel
        if (doclamp) r <- clamp_data(model, r)
    } else {
        allowParallel <- model[[1]]$control$allowParallel
        if (doclamp) {
            message("Only training data of the first model will be used to clamp rasterStack")
            r <- clamp_data(model[[1]], r)
        }
    }

    if (allowParallel && getDoParWorkers() > 1) {

        # prepare parallel
        n <- seq_len(nrow(r))
        index.split <- split(n, ceiling(n / (nrow(r) / getDoParWorkers())))
        if (is.train) model <- list(model)
        comb <- if (is.data.frame(predict2(model[[1]], newdata = r[1, ], ...))) "rbind" else "c"

        preds <- foreach(m = model) %:%
            foreach(i = index.split, .combine = comb) %dopar% {
                predict2(m, newdata = r[i, ], ...)
            }

        if (is.train) preds <- preds[[1]]

    } else {

        preds <- predict2(model, newdata = r, ...)

    }


    if (is.train) {

        func_back <- if (is.data.frame(preds)) back_to_raster2 else back_to_raster
        out <- func_back(preds, object, r_index)

    } else {

        func_back <- if (is.data.frame(preds[[1]])) back_to_raster2 else back_to_raster
        out <- lapply(preds, func_back, rasterStack = object, r_index = r_index)
        out <- raster::stack(out)

    }
    return(out)
}


back_to_raster2 <- function(cv, rasterStack, r_index) {
    out <- list()
    cnames <- colnames(cv)
    for (i in seq_len(ncol(cv))) {
        out[[cnames[i]]] <- back_to_raster(cv[, i], rasterStack, r_index)
    }
    return(raster::stack(out))
}



#' Clamp values in a dataset
#'
#' This function clamp values in the \code{dataset} object based on
#' minimum and maximum values on the train data.
#' @param object A data.frame with the training data or a model returned
#' by \code{\link[caret]{train}}, from which the training data is obtained.
#' @param dataset Either a data.frame or a rasterStack object with values to be clamped.
#' Columns or layers names should match the columns of the training data.
#' @return The \code{dataset} with values clamped.
#' @export
clamp_data <- function(object, dataset) {

    if (inherits(object, "train"))
        traindata <- object$trainingData
    else {
        traindata <- object
    }


    if (!all(names(dataset) %in% colnames(traindata)))
        stop("'dataset' layer/columns names should be equal to 'traindata' columns names.")

    traindata <- as.data.frame(traindata)
    traindata <- traindata[, names(dataset)]

    for (i in seq_len(ncol(traindata))) {
        rang <- range(traindata[, i])

        tmp <- dataset[[i]] < rang[1]
        dataset[[i]][tmp] <- rang[1]

        tmp <- dataset[[i]] > rang[2]
        dataset[[i]][tmp] <- rang[2]
    }

    return(dataset)
}
