
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
#' If \code{NULL}, the first metric is used.
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
    out$modelInfo <- list(label = "Ensemble")
    out$modelType <- model.list[[1]]$modelType
    out$levels <- model.list[[1]]$levels
    out$coefnames <- model.list[[1]]$coefnames
    out$xlevels <- model.list[[1]]$xlevels
    
    nrows <- sapply(model.list, function(x) nrow(x$trainingData))
    if (all(nrows == nrows[1])) {
        i <- 1
    } else {
        i <- which.min(nrows)
        warning("Models with distinct trainingData.
                Only the first trainingData with less rows will be used in other functions.")
    }
    out$trainingData <- model.list[[i]]$trainingData
    out$control <- model.list[[i]]$control
    
    # select algorithm
    out$ensemble_method <- ensemble_method
    out$algo <- switch(ensemble_method,
                       mean = function(x, w) mean(x),
                       median = function(x, w) stats::median(x),
                       weighted_mean = function(x, w) stats::weighted.mean(x, w),
                       number_votes = function(x, w) sum(x == w)/length(x)
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
        
        if (is.null(tmp[[1]][[paste0("Train",metric)]]))
            stop("'metric' must be a valid metric from summaryFunction used to train the model.")
        out$w <- sapply(tmp, `[[`, paste0("Train",metric))
        
    } else if (ensemble_method == "number_votes") {
        
        presence <- sapply(model.list, function(x) x$levels[1])
        if (!all(presence == presence[1]))
            stop("All models in 'model.list' should have the same first level as response.")
        
        out$w <- presence[1]
        
    } else {
        out$w <- NULL
    }

    
    # calculate predictions if requested
    if (calc.pred) {
        # check if models diverge in control
        check_control <- sapply(model.list, function(x,y) identical(y, x$control$indexOut), y = out$control$indexOut)
        if (!all(check_control)) stop("It is only possible to calculate train predictions if the 
                                        same 'control' and 'data' were used in all models.")
        
        # check for savePredictions in all models
        check_pred <- sapply(model.list, function(x) x$control$savePredictions == "none" || x$control$savePredictions == FALSE)
        if (any(check_pred)) stop("'savePredictions' should be TRUE, 'all', or 'final' in all models.")
        
        preds <- lapply(model.list, function(x) merge(x$pred, x$bestTune))
        if (out$modelType == "Classification") {
            preds <- lapply(preds, `[[`, out$levels[1])
        } else {
            preds <- lapply(preds, `[[`, "pred")
        }
        preds <- apply(data.frame(preds), 1, out$algo, w = out$w)

        out$pred <- data.frame(
                pred = factor(preds > 0.5, levels=c(TRUE,FALSE), labels=out$levels),
                obs = model.list[[1]]$pred$obs,
                lvl1 = preds,
                lvl2 = 1-preds,
                rowIndex = model.list[[1]]$pred$rowIndex,
                params = 0,
                Resample = model.list[[1]]$pred$Resample
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
            preds <- factor(preds > 0.5, levels=c(TRUE,FALSE), labels=object$levels)
        } else {
            preds <- data.frame(l1 = preds, l2 = 1 - preds)
            colnames(preds) <- object$levels
        }
    }
    return(preds)
}
