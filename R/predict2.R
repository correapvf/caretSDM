
#' Set a threshold in a caret model
#' 
#' This function will set a probability threshold cutoff to be used in predictions
#' in a two class Classification problem. This threshold is automatically read by \code{\link{predict2}} and other 
#' functions in this package when calculating new predictions.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param thr A value between 0 and 1 to be used as a threshold or a data.frame with one row and a column named 'thr'
#' containing the threshold to use and other columns indicating parameters to retune the model. Can also be the
#' output of \code{summary.thresholder2()}.
#' @return The caret model provided with a new item \code{thr} and the 
#' \code{pred} data.frame updated, if it exists. If "thr" is a 'data.frame', the finalModel
#' is also updated using new parameters provided.
#' @note Objects \code{results} and \code{resample} remain unchanged. Use \code{\link{evaluate}} to
#' obtain corrected statistics based on the new threshold.
#' @seealso \code{\link{predict2}} \code{\link[caret]{update.train}}
#' @export
setThreshold <- function(model, thr) {
    
    if(!inherits(model, "train")) 
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
        model$pred$pred <- factor(model$pred[,lvl[1]] > thr, 
                                  levels=c(TRUE,FALSE), labels=lvl)
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
#' Additionally, you can set \code{type = "both"}, which will return a data.frame with both class probabilities and a prediction.
#' or \code{type = "prob1"} to return a single vector with the first class probabilities (assumed to be presence).\cr\cr
#' The method for RasterStack is a simpler version of \code{raster::predict}, but it may be faster sometimes.
#' @param object,model A model returned by \code{\link[caret]{train}} or a list of models.
#' object can also be a RasterStack from the \code{raster} package, used to create predictions.
#' @param newdata An optional set of data to predict on.
#' @param type One of "raw", "prob", "both" or "prob_presence" (see details).
#' @param ... Further arguments passed to \code{predict} or method 'train'.
#' @return A vector or data.frame with predictions or probabilities, based on the argument \code{type}. Check details.
#' If \code{model} is a list of models, than a list of predictions is returned.
#' @seealso \code{\link{setThreshold}}
#' @rdname predict2
#' @export
predict2.train <- function(object, newdata = NULL, type = "raw", ...) {
    
    if (type=="raw") {
        
        if (is.null(object$thr)) {
            out <- predict(object, newdata, "raw", ...)
        } else {
            pred <- predict(object, newdata, "prob", ...)
            out <- factor(pred[,object$levels[1]] > thr, 
                          levels=c(TRUE,FALSE), labels=object$levels)
        }
        
    } else if (type=="both") {
        pred <- predict(object, newdata, "prob", ...)
        
        if (is.null(object$thr)) {
            tmp <- predict(object, newdata, "raw", ...)
        } else {
            tmp <- factor(pred[,object$levels[1]] > thr, 
                          levels=c(TRUE,FALSE), labels=object$levels)
        }
        out <- data.frame(pred = tmp, pred)
        
    } else if (type=="prob1") {
        
        out <- predict(object, newdata, "prob", ...)[,object$levels[1]]
        
    } else {
        stop("type must be one of 'raw', 'prob', 'both' or 'prob1'.")
    }
    
    return(out)
}



#' @rdname predict2
#' @export
predict2.list <- function(object, ...) {
    check_list(object)
    object <- check_names(object)
    
    out <- lapply(object, predict2, ...)
    names(out) = sapply(object, function(x) x$modelInfo$label)
    
    return(out)
}



#' @rdname predict2
#' @export
predict2.RasterStack <- function(object, model, ...) {
    # convert raster to data.frame
    r <- as.data.frame(object)
    r <- stats::na.omit(r)
    r_index <- as.numeric(row.names(r))
    
    preds <- predict2(model, newdata = r, ...)
    
    if (inherits(model, "train")) {
        out <- back_to_raster(preds, object, r_index)
    } else {
        out <- lapply(preds, back_to_raster, rasterStack=object, r_index=r_index)
        out <- raster::stack(out)
    }
    return(out)
}

