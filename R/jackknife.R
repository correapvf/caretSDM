
#' @rdname jackknife
#' @export
jackknife <- function(model, ...) UseMethod("jackknife")


#' Jackknife test to measure variable importance
#' 
#' Create metrics for models with and without each variable.
#' 
#' Multiple models are created using the same method and parameters of the \code{model}.
#' Each variable is excluded in turn, and a model created with the remaining variables.
#' Then a model is created using each variable in isolation. Metrics (provided by \code{summaryFunction})
#' are calculated for each model.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param summaryFunction A Summary function (one of \code{\link[caret]{defaultSummary}}) that calculate the metrics. 
#' If \code{NULL}, the summaryFunction provided in the model will be used.
#' @param errorFunction A function used to calculate errors across resamples. Default is 95\% confidence interval.
#' @param param_override_only,param_override_without A list or data.frame with parameters to override the
#' best tune before they are passed to "train" to create a model with only and without one variable.
#' Useful for models where tuning parameters are dependent of the number of variables (like 'mtry' for randomForest).
#' @param progress logical. Show progress bar? If parallel is activated, it automatically defaults to \code{FALSE}.
#' @return A data.table that also inherts "jackknife.train" with metrics for each variable.
#' @examples
#' \dontrun{
#' jackknife.train(model)
#' 
#' # using caret twoClassSummary and standard deviation as errors
#' jackknife.train(model, summaryFunction = twoClassSummary, errorFunction = sd)
#' 
#' # For models trained with 'rf', override 'mtry' as 1 when training with a single variable
#' j <- jackknife.train(model, param_override_only = list(mtry=1))
#' plot(j)
#' }
#' @rdname jackknife
#' @export
jackknife.train <- function(model, summaryFunction = NULL, errorFunction = ci_95,
                            param_override_only = NULL, param_override_without = NULL, progress = TRUE, ...) {
    
    check_train(model)
    
    out <- evaluate(model, summaryFunction = summaryFunction, errorFunction = errorFunction)
    out <- out$eval
    out[,c("data","variable") := .("With all variables","")]
    
    coefs <- getcoefs(model)
    type.class <- if (model$modelType == "Classification") TRUE else FALSE
    
    
    # modify trainControl
    control <- model$control
    control$savePredictions <- "final"
    
    if (is.null(summaryFunction)) {
        summaryFunction <- control$summaryFunction
    }
    
    # make a list of arguments
    args_only <- c(list(.outcome ~ .,
                        method = model$method,
                        preProcess = model$call$preProcess,
                        weights = model$trainingData$.weights,
                        metric = model$metric,
                        trControl = control),
                   model$dots)
    
    args_without <- args_only
    
    if (is.null(param_override_only)) {
        args_only$tuneGrid <- model$bestTune
    } else {
        args_only$tuneGrid <- param_overrive(model$bestTune, param_override_only)
    }
    
    if (is.null(param_override_without)) {
        args_without$tuneGrid <- model$bestTune
    } else {
        args_without$tuneGrid <- param_overrive(model$bestTune, param_override_without)
    }
    
    
    if(model$control$allowParallel && getDoParWorkers() > 1) {
        `%op%` <- `%dopar%`
        progress <- FALSE
    } else {
        `%op%` <- `%do%`
    }
    
    if (progress) {
        pb <- txtProgressBar(max = length(coefs)*2, style = 3)
        pbi <- 1
    }
    
    
    results <- foreach(coef=coefs) %op% {
        
        # importance for only the coef              
        coef_only <- c(coef, ".outcome")
        args_only$data <- model$trainingData[,coef_only]
        
        tmp.model <- invisible(do.call("train", args_only))
        
        tmp.out <- eval_ci_true(tmp.model, NULL, NULL, NULL, summaryFunction, 
                                errorFunction, type.class, TRUE)
        tmp.out <- tmp.out$eval[, data := "With only variable"]
        if (progress) {setTxtProgressBar(pb, pbi); pbi <- pbi + 1}
        
        # importance without variable
        coef_without <- c(coefs[!(coefs %in% coef)], ".outcome")
        args_without$data <- model$trainingData[,coef_without]
        tmp.model <- invisible(do.call("train", args_without))
        tmp.out2 <- eval_ci_true(tmp.model, NULL, NULL, NULL, summaryFunction, 
                                 errorFunction, type.class, TRUE)
        tmp.out2 <- tmp.out2$eval[, data := "Without variable"]
        if (progress) {setTxtProgressBar(pb, pbi); pbi <- pbi + 1}
        
        tmp.out <- rbind(tmp.out, tmp.out2)
        tmp.out[, variable := coef]
        tmp.out
        
    }
    
    # format output
    results <- rbindlist(results)
    out <- rbind(out, results)
    
    out[, data := factor(data, levels = c("Without variable", "With only variable", "With all variables"))]
    out[, variable := factor(variable, levels = c(coefs, ""))]
    
    class(out) <- append("jackknife.train", class(out))
    return(out)
}



#' @rdname combine
#' @export
jackknife.list <- function(model, ...) {
    
    check_list(model)
    model <- check_names(model)
    
    x <- lapply(model, jackknife.train, ...)
    return(c_jackknife.train(x))
}



#' @param x  An object from \code{jackknife}.
#' @param metric A character, indicating which metric to plot. If \code{NULL},
#' only the first metric is plotted.
#' @param plot_errorbar logical. Should plot error bars?
#' @param ... ignored
#' @rdname jackknife
#' @export
plot.jackknife.train <- function(x, metric = NULL, plot_errorbar = TRUE, ...) {
    met <- if (is.null(metric)) x$metric[1] else metric
    x <- x[metric == met]
    
    fig <- ggplot(x, aes(x=value, y=variable, fill=data)) +
        geom_bar(position=position_dodge(), stat="identity") +
        facet_wrap(~method) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
        scale_y_discrete(limits=rev) +
        scale_fill_brewer(palette="Set2") + theme_bw() +
        ggtitle(met) + theme(plot.title = element_text(hjust = 0.5, size=9, face = "bold"))
    
    if (plot_errorbar) {
        fig <- fig + geom_errorbar(aes(xmin=value-error, xmax=value+error),
                                   width=.25, position=position_dodge(.9))
    }
    
    print(fig)
    return(invisible(fig))
}


#' @rdname combine
#' @export
c.jackknife.train <- function(...) {
    obj <- list(...)
    
    if (!all(sapply(obj, inherits, what = "jackknife.train")))
        stop("All objects must be an output of 'jackknife.train'.")
    
    methods <- unlist(lapply(obj, function(x) unique(x$method)))
    if (any(duplicated(methods)))
        stop("Models should have different methods.")
    
    return(c_jackknife.train(obj))
}
c_jackknife.train <- function(x) {
    x <- rbindlist(x)
    class(x) <- append("jackknife.train", class(x))
    return(x)
}



#' @export
print.jackknife.train <- function(x, ...) {
    cat("Object of type jackknife.train\n")
    cat("Model(s) avaiable:", levels(x$method),"\n\n")
    NextMethod(nrows=20)
}



# substitute a parameter of the bestTune
param_overrive <- function(bestTune, param) {
    cnames <- colnames(bestTune)
    index <- which(cnames %in% names(param))
    for (i in index) {
        bestTune[[i]] <- param[[ cnames[i] ]]
    }
    return(bestTune)
}
