
globalVariables(c("ensamble_method", "cvi", "predi", "wi", "i", "j"))

#' @rdname confidence_map
#' @export
confidence_map <- function(model, ...) UseMethod("confidence_map")

#' Create confidence maps
#'
#' Create confidence maps using bootstrap replicates
#'
#' For each interaction (defined by nrep), the trained data set for each model is resampled (bootstrap)
#' and each model retrained using this new data.\cr
#' Predictions are made for each model for each interaction, separately.
#'
#' For 'train' objects, the mean and standard deviation from all replicates are returned.
#'
#' For 'ensemble.train' objects, the coefficient of variation is calculated for each model, which in turn is assembled
#' based on algorithmic used to create the ensemble:
#' \tabular{ll}{
#' \code{mean, weighted_mean   ->} \tab \figure{eq_mean.png}\cr
#' \code{median, number_votes  ->} \tab \figure{eq_median.png}
#' }
#' Where \code{cvi =} mean coefficient of variation for model \code{i},\cr
#' \code{pi =} predictions for model \code{i} using all training data,\cr
#' \code{wi =} weight of model \code{i},\cr
#' and \code{p =} predictions of the ensemble using all training data.\cr
#' For \code{weighted_mean}, \code{wi = metric/sum(all metrics)};
#' and for \code{mean}, \code{wi = 1/(number of models)}.
#' @param model A model returned by \code{\link[caret]{train}} or \code{\link{createEnsemble}}.
#' @param rasterStack A RasterStack from the \code{raster} package, used to create predictions.
#' @param nrep Number of bootstrap replicates.
#' @param progress logical. Show progress text? If parallel is activated, it automatically defaults to \code{FALSE}.
#' @param ... ignored
#' @return For 'train' or 'list' methods, a 'RasterLayer' or a 'RasterStack' containing the coefficient of variation
#' (CV) of the bootstrap output for each model. \cr\cr
#' For 'ensemble.train' method, if \code{return.all} is \code{TRUE}, return a list with \code{$preds}
#' and \code{$cvs} containing each a 'RasterStack' of the predictions and CVs of all models,
#' including the ensemble. Else, return a single 'RasterLayer' with CV of the ensemble model.
#' @rdname confidence_map
#' @export
confidence_map.train <- function(model, rasterStack, nrep = 200, progress = TRUE, ...) {
    out <- confidence_map.list(list(model), rasterStack, nrep, progress)
    return(raster::raster(out))
}



#' @rdname confidence_map
#' @export
confidence_map.list <- function(model, rasterStack, nrep = 200, progress = TRUE, ...) {

    check_list(model)
    check_list_ensemble(model)
    model <- check_names(model)

    model.type <- if (model[[1]]$modelType == "Classification") "prob1" else "raw"


    # convert raster to data.frame
    r <- raster2data(rasterStack)
    r_index <- as.numeric(row.names(r))

    results <- confidence_helper(model, r, nrep, model.type, progress)
    names.models <- sapply(model, function(x) x$modelInfo$label)

    # calculate mean
    means <- lapply(results, `[[`, "mean")
    names(means) <- names.models
    means <- lapply(means, back_to_raster, rasterStack = rasterStack, r_index = r_index)

    # calculate sd
    sds <- lapply(results, `[[`, "sd")
    names(sds) <- names.models
    sds <- lapply(sds, back_to_raster, rasterStack = rasterStack, r_index = r_index)

    return(raster::stack(raster::stack(sds) / raster::stack(means)))

}



#' @param return.all logical. If \code{TRUE}, return predictions and cvs for all models. Else, return
#' only the CV for the ensemble model.
#' @rdname confidence_map
#' @export
confidence_map.ensemble.train <- function(model, rasterStack, nrep = 200, progress = TRUE, return.all = FALSE, ...) {

    # convert raster to data.frame
    r <- raster2data(rasterStack)
    r_index <- as.numeric(row.names(r))

    model.type <- if (model$modelType == "Classification") "prob1" else "raw"

    # calculate cv for each model
    results <- confidence_helper(model$model.list, r, nrep, model.type, progress)
    cvs <- lapply(results, function(x) x$sd / x$mean)

    # calculate predictions for each model
    preds <- predict2(model$model.list, r, type = model.type)

    # ensemble model of predictions
    pred <- apply(data.frame(preds), 1, model$algo, w = model$w)


    if (model$ensemble_method %in% c("mean", "weighted_mean")) {

        weigts <- switch(model$ensemble_method,
                         mean = 1 / length(model$model.list),
                         weighted_mean =  model$w / sum(model$w)
        )

        cv <- foreach(cvi = cvs, predi = preds, wi = weigts, .combine = `+`) %do% {
            (cvi * predi * wi)^2
        }
        cv <- sqrt(cv / pred^2)

    } else {

        cv <- foreach(cvi = cvs, predi = preds, .combine = cbind) %do% {
            (cvi * predi) / pred
        }
        cv <- apply(cv, 1, stats::median)
    }

    if (return.all) {
        # convert predictions and cvs back to raster
        names(cvs) <- names(preds)
        preds$ensemble <- pred
        cvs$ensemble <- cv

        preds <- lapply(preds, back_to_raster, rasterStack = rasterStack, r_index = r_index)
        cvs <- lapply(cvs, back_to_raster, rasterStack = rasterStack, r_index = r_index)

        return(list(preds = raster::stack(preds), cvs = raster::stack(cvs)))
    } else {
        return(back_to_raster(cv, rasterStack, r_index))
    }
}



###############
# Helper functions

confidence_helper <- function(model.list, newdata, number, model.type, progress) {

    # prepare progress bar
    do.par <- model.list[[1]]$control$allowParallel && getDoParWorkers() > 1
    `%op%` <- if (do.par) `%dopar%` else  `%do%`
    opts <- NULL
    doProgress <- progress && !do.par
    
    if (progress) {
        cat("Create confidence map\n")
        pb <- txtProgressBar(max = length(model.list) * number, style = 3)
        
        if (do.par) {
            opts <- list(progress = function(n) setTxtProgressBar(pb, n))
        } else {
            counter <- 1
        }
    }



    # create index
    index.list <- lapply(model.list, function(x) caret::createResample(x$trainingData$.outcome, number))


    # create args
    args.list <- lapply(model.list, create_args)

    # get coefficients without the .weights
    coefs <- getcoefs(model.list[[1]], ".weights")

    wa <- WACombine$new(length(model.list))

    results <- foreach(i = seq_along(model.list)) %:%
        foreach(j = seq_len(number), .combine = wa$combine, .init = NULL,
                .inorder = FALSE, .multicombine = TRUE,
                .packages = "caret", .options.snow = opts) %op% {
            
            model <- model.list[[i]]
            index <- index.list[[i]][[j]]
            args <- args.list[[i]]

            # assign data to args
            args$data <- as.data.frame(model$trainingData)[index, coefs]
            args$weights <- model$trainingData$.weights[index]

            # importance for only the coef
            tmp.model <- invisible(do.call("train", args))

            # create predictions
            pred <- predict2(tmp.model, newdata, type = model.type)

            if (doProgress) {
                setTxtProgressBar(pb, counter)
                counter <- counter + 1
            }

            list(i, pred)
        }

    if (progress) close(pb)

    return(wa$finalize())
}


WACombine <- R6::R6Class("WelfordAlgorithm_combine_foreach",
    private = list(
        out = NULL
    ),
    
    public = list(
        initialize = function(n) {
            private$out <- lapply(seq_len(n), function(x) WelfordAlgorithm$new())
        },
        combine = function(...) {
            l <- list(...)
            l <- l[!sapply(l, is.null)]
            for (x in l) {
                n <- x[[1]]
                values <- x[[2]]
                private$out[[n]]$update(values)
            }
            return(NULL)
        },
        finalize = function() {
            return(lapply(private$out, function(x) x$finalize()))
        }
    ),
    
    active = list()
)

WelfordAlgorithm <- R6::R6Class("WelfordAlgorithm",
    private = list(
        count = NULL,
        mean = NULL,
        M2 = NULL),

    public = list(
        initialize = function() {
            private$count <- 0
            private$mean <- 0
            private$M2 <- 0
        },
        update = function(newValue) {
            private$count <- private$count + 1
            delta <- newValue - private$mean
            private$mean <- private$mean + (delta / private$count)
            delta2 <- newValue - private$mean
            private$M2 <- private$M2 + (delta * delta2)
        },
        finalize = function() {
            if (private$count < 2) {
                warning("Only one value avaiable. Returning NaN.")
                return(NaN)
            } else {
                return(list(mean = private$mean, sd = sqrt(private$M2 / (private$count - 1))))
            }
        }
    ),

    active = list()
)


back_to_raster <- function(cv, rasterStack, r_index) {
    cv_final <- rep(NA, raster::ncell(rasterStack))
    cv_final[r_index] <- cv
    cv_final <- raster::raster(matrix(cv_final, nrow(rasterStack), ncol(rasterStack), byrow = TRUE),
                               template = rasterStack)
}
