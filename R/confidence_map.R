
globalVariables(c("ensamble_method", "cvi", "predi", "wi", "index", "i", "model"))

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
#' For 'train' objects, the mean and standard deviation of all replicates are retuned.
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
#' @return For 'train' or 'list' methods, a list with \code{$means} and \code{$sds} containing
#' 'RasterLayers' (for 'train') or 'RasterStacks' (for 'list', each layer representing one model).
#'  For 'ensemble.train' method, a single 'RasterLayer' is returned.
#' @rdname confidence_map
#' @export
confidence_map.train <- function(model, rasterStack, nrep = 200, progress = TRUE, ...) {
    out <- confidence_map.list(list(model), rasterStack, nrep, progress)
    return(list(means = raster::raster(out$means), sds = raster::raster(out$sds)))
}



#' @rdname confidence_map
#' @export
confidence_map.list <- function(model, rasterStack, nrep = 200, progress = TRUE, ...) {
    
    check_list(model)
    check_list_ensemble(model)
    model <- check_names(model)
    
    model.type <- if (model[[1]]$modelType == "Classification") "prob1" else "raw"
    
    
    # convert raster to data.frame
    r <- raster::as.data.frame(rasterStack, na.rm = FALSE)
    names(r) <- sub("_VALUE$", "", names(r))
    r_index <- as.numeric(row.names(r))
    
    sums <- confidence_helper(model, r, nrep, model.type, progress)
    names.models = sapply(model, function(x) x$modelInfo$label)
    
    # calculate mean
    means <- lapply(sums, calc_mean, nrep)
    names(means) <- names.models
    means <- lapply(means, back_to_raster, rasterStack=rasterStack, r_index=r_index)
    
    # calculate sd
    sds <- lapply(sums, calc_sd, nrep)
    names(sds) <- names.models
    sds <- lapply(sds, back_to_raster, rasterStack=rasterStack, r_index=r_index)
    
    return(list(means = raster::stack(means), sds = raster::stack(sds)))
    
}



#' @rdname confidence_map
#' @export
confidence_map.ensemble.train <- function(model, rasterStack, nrep = 200, progress = TRUE, ...) {
    
    # convert raster to data.frame
    r <- raster::as.data.frame(rasterStack, na.rm = FALSE)
    names(r) <- sub("_VALUE$", "", names(r))
    r_index <- as.numeric(row.names(r))
    
    model.type <- if (model$modelType == "Classification") "prob1" else "raw"
    
    # calculate cv for each model
    sums <- confidence_helper(model$model.list, r, nrep, model.type, progress)
    cvs <- lapply(sums, calc_cv, nrep)
    
    # calculate predictions for each model
    preds <- predict2(model$model.list, r, type = model.type)
    
    # ensemble model of predictions
    pred <- apply(data.frame(preds), 1, model$algo, w = model$w)
    
    
    if (model$ensemble_method %in% c("mean","weighted_mean")) {
        
        weigts <- switch(model$ensemble_method,
                         mean = 1/length(model$model.list),
                         weighted_mean =  model$w/sum(model$w)
        )
        
        cv <- foreach(cvi = cvs, predi = preds, wi = weigts, .combine = `+`) %do% {
            (cvi*predi*wi)^2
        }
        cv <- sqrt(cv/pred^2)
        
    } else {
        
        cv <- foreach(cvi = cvs, predi = preds, .combine = cbind) %do% {
            (cvi*predi)/pred
        }
        cv <- apply(cv, 1, stats::median)
    }
    
    return(back_to_raster(cv, rasterStack, r_index))
}



###############
# Helper functions

confidence_helper <- function(model.list, newdata, number, model.type, progress) {
    
    # prepare foreach
    if(model.list[[1]]$control$allowParallel && getDoParWorkers() > 1) {
        `%op%` <- `%dopar%`
        progress <- FALSE
    } else {
        `%op%` <- `%do%`
    }
    
    if (progress) {
        pb <- txtProgressBar(max = length(model.list)*number, style = 3)
        pbi <- 1
    }
    
    
    # create index
    index.list <- lapply(model.list, function(x) caret::createResample(x$trainingData$.outcome, number))
    
    
    # create args
    args.list <- lapply(model.list, create_args)
    
    # get coefficients without the .weights
    coefs <- getcoefs(model.list[[1]], ".weights")
    
    
    results <- foreach(model=model.list, args=args.list, index=index.list) %:%
        foreach(i=index, .combine = sum_pred, .inorder = FALSE, .multicombine = FALSE) %op% {
            
            # assign data to args
            args$data <- model$trainingData[i,coefs]
            args$weights <- model$trainingData$.weights[i]
            
            # importance for only the coef              
            tmp.model <- invisible(do.call("train", args))
            
            # create predictions
            pred <- predict2(tmp.model, newdata, type = model.type)
            
            if (progress) {setTxtProgressBar(pb, pbi); pbi <- pbi + 1}
            list(pred_sum = pred, pred_sum_sqr = pred^2)
        }
    
    return(results)
}


sum_pred <- function(...) {
    x <- list(...)
    list(pred_sum = rowSums(sapply(x, `[[`, "pred_sum")), 
         pred_sum_sqr = rowSums(sapply(x, `[[`, "pred_sum_sqr")))
}


calc_mean <- function(x, n) {
    x$pred_sum / n
}

calc_sd <- function(x, n) {
    sqrt((n*x$pred_sum_sqr - x$pred_sum^2)/(n*(n-1)))
}

calc_cv <- function(x, n) {
    calc_sd(x, n) / calc_mean(x, n)
}


back_to_raster <- function(cv, rasterStack, r_index) {
    cv_final <- rep(NA, raster::ncell(rasterStack))
    cv_final[r_index] <- cv
    cv_final <- raster::raster(matrix(cv_final, nrow(rasterStack), ncol(rasterStack), byrow = TRUE), 
                               template=rasterStack)
}
