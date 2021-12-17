
globalVariables(c("ind", "o"))

#' Create a ensemble from models
#'
#' Combine the predictions of multiple models
#'
#' You can create a ensemble model based on predictions of multiple models.
#' The ensemble prediction is calculated based on the ensemble_method:
#' \itemize{
#' \item \code{mean} - models mean prediction.
#' \item \code{median} - models median prediction.
#' \item \code{weighted_mean } - models weighted mean prediction. Weighs are based on the \code{metric},
#' so models with higher metric have more weigh in the mean.
#' \item \code{number_votes} - The number of predictions of the first class (considered positive or presence) is
#' divided by the number of models.  When the prediction is close to 1, it means that all models
#' agree to predict the first class. Only models of type "Classification" are supported.
#' }
#' For classification, the probability of predictions is used to create the ensemble.
#' @param model.list A list of models returned by \code{\link[caret]{train}}.
#' @param ensemble_method One of 'mean', 'median', 'weighted_mean' and 'number_votes'. Check details.
#' @param metric A metric to use to calculate weights. Only used if \code{ensemble_method = "weighted.mean"}.
#' Must be one of the metrics provided by 'summaryFunction' used to train the models.
#' If \code{NULL}, the metric of the first model is used.
#' @param calc.pred logical. Calculate predictions for training data? If \code{TRUE}, savePredictions is set
#' to "final"; else, savePredictions is set to "FALSE".
#' @return An S3 object of classes "ensemble.train" that also inherits "train".
#' This object can be used in other functions, like \code{evaluate} or \code{confidence_map}.
#' @seealso \code{\link{confidence_map}}.
#' @export
createEnsemble <- function(model.list, ensemble_method = "weighted_mean", metric = NULL, calc.pred = FALSE) {

    check_list(model.list)
    check_list_ensemble(model.list)

    out <- list()
    out$model.list <- model.list

    # add items in the list so it can be evaluated later
    out$method <- "ensemble"
    out$modelInfo <- list(label = "Ensemble",
                      parameters = data.frame(parameter = "params", class = "numeric", label = "dummy"))
    out$modelType <- model.list[[1]]$modelType
    out$levels <- model.list[[1]]$levels
    out$coefnames <- model.list[[1]]$coefnames
    out$xlevels <- model.list[[1]]$xlevels
    out$trainingData <- model.list[[1]]$trainingData
    out$control <- model.list[[1]]$control

    # select algorithm
    out$ensemble_method <- ensemble_method
    out$algo <- switch(ensemble_method,
                       mean = function(x, w) mean(x),
                       median = function(x, w) stats::median(x),
                       weighted_mean = function(x, w) stats::weighted.mean(x, w),
                       number_votes = function(x, w) sum(x == w) / length(x)
                    )
    if (is.null(out$algo))
        stop("ensemble_method should be one of 'mean', 'median',
                                'weighted_mean' or 'number_votes'")

    # check model.type
    if (ensemble_method == "number_votes") {

        if (out$modelType != "Classification")
            stop("All models in 'model.list' must be of type Classification for this algorithm.")
        out$type <- "raw"
    } else {
        out$type <- if (out$modelType == "Classification") "prob1" else "raw"
    }


    # get weighs
    if (ensemble_method == "weighted_mean") {

        if (is.null(metric)) metric <- model.list[[1]]$metric
        tmp <- lapply(model.list, caret::getTrainPerf)

        if (is.null(tmp[[1]][[paste0("Train", metric)]]))
            stop("'metric' must be a valid metric from summaryFunction used to train the model.")
        out$w <- sapply(tmp, `[[`, paste0("Train", metric))

    } else if (ensemble_method == "number_votes") {

        presence <- sapply(model.list, function(x) x$levels[1])
        if (!all(presence == presence[1]))
            stop("All models in 'model.list' should have the same first level as response.")

        out$w <- presence[1]

    } else {
        out$w <- NULL
    }


    # calculate predictions if possible
    if (calc.pred) {
        # check 1
        check_control0 <- sapply(model.list, function(x) length(x$control$indexOut))
        check_control0 <- all(check_control0 == check_control0[1])

        if (!check_control0) {
            message("Distinct number of replication detected. Skipping predictions calculation!")
            calc.pred <- FALSE
        }

        # check 2
        check_control1 <- sapply(model.list, function(x)
            x$control$savePredictions == "none" || x$control$savePredictions == FALSE)

        if (any(check_control1)) {
            message("'savePredictions' is not set to TRUE or 'final' in all models.
                    Skipping predictions calculation!")
            calc.pred <- FALSE
        }

        # check 3
        check_control2 <- sapply(model.list, function(x) {
            any(mapply(function(x, y) any(y %in% x), x = x$control$index, y = out$control$indexOut))
            })

        if (any(check_control2)) {
            message("indexOut of one model has records from index of onother model at any replicate.
                You should train all models of the ensemble using the same index and indexOut in trainControl.
                Skipping predictions calculation!")
            calc.pred <- FALSE
        }

    }
    


    if (calc.pred) {

        check_control3 <- sapply(model.list, function(x, y) identical(y, x$control$indexOut), y = out$control$indexOut)

        # if one model has distinct traindata, we need to recalculate testdata for pred
        if (!all(check_control3)) {
            message("Models with distinct traindata detected. Using traindata of the first model to evaluate.")
            m <- which(!check_control3)

            for (i in m) {

                 args <- create_args(model.list[[i]])
                 args$trControl$savePredictions <- "final"

                # get with seed to use
                results <- model.list[[i]]$results
                results$order <- seq(nrow(results))
                results <- merge(results, model.list[[i]]$bestTune)
                M <- results$order

                `%op%` <- if (out$control$allowParallel && getDoParWorkers() > 1) `%dopar%` else  `%do%`

                results <- foreach(ind = model.list[[i]]$control$index,
                                   s = model.list[[i]]$control$seeds,
                                   o = out$control$indexOut,
                                   .packages = "caret") %op% {

                    args$trControl$seeds <- s[M]
                    args$data <- model.list[[i]]$trainingData[ind, ]
                    args$weights <- model.list[[i]]$trainingData$.weights[ind]

                    tmp.model <- invisible(do.call("train", args))

                    predict2(tmp.model, out$trainingData[o, ], type = "both")

                }

                results <- as.data.frame(rbindlist(results))
                results2 <- data.frame(
                    obs = out$trainingData$.outcome,
                    rowIndex = unlist(out$control$indexOut),
                    Resample = rep(names(out$control$index), sapply(out$control$indexOut, length)),
                    model.list[[i]]$bestTune
                )

                model.list[[i]]$pred <- cbind(results, results2)

            }
        }


        preds <- lapply(model.list, function(x) merge(x$pred, x$bestTune))
        out_tmp <- preds[[1]][, c('obs', 'rowIndex', 'Resample')]
        preds <- lapply(preds, as.data.table)
        col_join <- c('Resample', 'rowIndex')
        preds <- lapply(preds, function(x) x[out_tmp[, col_join], on = col_join])

        if (out$modelType == "Classification") {
            preds <- lapply(preds, `[[`, out$levels[1])
        } else {
            preds <- lapply(preds, `[[`, "pred")
        }
        preds <- apply(data.frame(preds), 1, out$algo, w = out$w)

        out$pred <- data.frame(
                pred = factor(preds > 0.5, levels = c(TRUE, FALSE), labels = out$levels),
                obs = out_tmp$obs,
                lvl1 = preds,
                lvl2 = 1 - preds,
                rowIndex = out_tmp$rowIndex,
                params = 0,
                Resample = out_tmp$Resample
            )
        colnames(out$pred)[3:4] <- out$levels
        out$bestTune <- data.frame(params = 0)
        out$control$savePredictions <- "final"

    } else {
        
        out$control$savePredictions <- FALSE
    }


    # get terms if available
    if (inherits(model.list[[1]], "train.formula")) {
        out$terms <- model.list[[1]]$terms
        class(out) <- c("ensemble.train", "train", "train.formula")
    } else {
        class(out) <- c("ensemble.train", "train")
    }

    return(out)
}


#' @param object A object returned by \code{createEnsemble}.
#' @param newdata A data.frame containing data to predict.
#' @param type One of "raw" or "prob".
#' @param ... Further arguments passed to \code{predict}.
#' @rdname createEnsemble
#' @export
predict.ensemble.train <- function(object, newdata = NULL, type = "raw", ...) {

    if (is.null(newdata)) newdata <- object$trainingData

    preds <- lapply(object$model.list, predict2, newdata = newdata, type = object$type, ...)
    preds <- apply(data.frame(preds), 1, object$algo, w = object$w)

    if (object$modelType == "Classification") {
        if (type == "raw") {
            preds <- factor(preds > 0.5, levels = c(TRUE, FALSE), labels = object$levels)
        } else {
            preds <- data.frame(l1 = preds, l2 = 1 - preds)
            colnames(preds) <- object$levels
        }
    }
    return(preds)
}
