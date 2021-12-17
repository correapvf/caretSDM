
utils::globalVariables(c("factors", "predictors", "s"))

#' @rdname response
#' @export
response <- function(model, ...) UseMethod("response")

#' Calculate response plots for train models
#'
#' Evaluation Strip method proposed by Elith et al.(2005),
#' applied to models created using \code{caret::train}.
#' Here we use the same process as described in \code{biomod2::response.plot2}.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param fixedvarFunction A function used to fix as constant the other variables when the predicting responses.
#' @param errorFunction A function used to calculate error across resamples. Default is 95% confidence interval.
#' If \code{NULL}, responses are calculated from the final model only.
#' @param n Number of responses to get for each variable, as in seq(min(variable), max(variable), length.out=n).
#' @param progress logical. Show progress bar?
#' @return An S3 object of class 'response.train', including:
#' \itemize{
#'   \item{num and fact}{Data.tables with responses for each variable of type numeric and factors.
#'   If \code{errorFunction != NULL}, responses are means from resamples and errors are also provided.}
#'   \item{num_resample and fact_resample}{Data.tables with responses for each resample,
#'   only provided if \code{errorFunction != NULL}.}
#'   \item{quantiles}{A matrix with quantiles for each variable used to plot rugs.}
#' }
#' @rdname response
#' @export
response.train <- function(model, fixedvarFunction = mean, errorFunction = ci_95, n = 100, progress = FALSE, ...) {

    fixedDat <- response_table(model, fixedvarFunction, n)

    out <- response_main(model, errorFunction, progress, fixedDat, ...)
    return(out)

}



#' @rdname combine
#' @export
response.list <- function(model, ...) {

    check_list(model)
    model.list <- check_names(model)

    # calculated fixedData for predictions based on the training data of the first model
    fixedDat <- response_table(model[[1]], ...)

    x <- lapply(model, response_main, fixedDat = fixedDat, ...)
    return(c_response.train(x))
}



#' @param x An object returned by \code{response}.
#' @param plot_errorbar logical. Should plot errors? Only valid if \code{resample = TRUE}.
#' @param plot_rugs logical. Should plot rugs representing quantiles?
#' @param plot_thr logical. Should plot a line representing the probability threshold of the model?
#' A threshold must be set using \code{setThreshold} and its only plotted if there is only one model
#' (to avoid to much noise in the plot).
#' @param ... ignored
#' @rdname response
#' @export
plot.response.train <- function(x, plot_errorbar = TRUE, plot_rugs = TRUE, plot_thr = TRUE, ...) {
    fig <- list()
    plot.error <- diff(range(x$num$error)) > 0 && plot_errorbar


    for (coef in x$coefs) {
        if (coef %in% names(x$xlevels)) {
            # plot bar plot to factor
            tmp <- x$fact[variable == coef]
            figtmp <- ggplot(tmp, aes(x = factors, y = response, fill = method)) +
                geom_bar(position = position_dodge(), stat = "identity") +
                scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
                scale_fill_brewer(palette = "Set1")

            if (plot.error) {
                figtmp <- figtmp + geom_errorbar(aes(ymin = response - error, ymax = response + error),
                                                 position = position_dodge(.9), width = 0.25)
            }

        } else {
            # plot line to numeric
            tmp <- x$num[variable == coef]
            figtmp <- ggplot(tmp, aes(x = predictors, y = response, color = method)) +
                geom_line() + scale_color_brewer(palette = "Set1") +
                scale_x_continuous(expand = expansion(mult = c(0.025, 0.025)))

            if (plot.error) {
                figtmp <- figtmp + geom_ribbon(aes(ymin = response - error, ymax = response + error, fill = method),
                                               alpha = 0.2, color = NA) +
                    scale_fill_brewer(palette = "Set1")
            }

            if (plot_rugs) {
                quantiles <- data.frame(q = x$quantiles[, coef])
                figtmp <- figtmp + geom_rug(data = quantiles, aes(x = q), sides = "b", inherit.aes = FALSE)
            }

        }

        if (!is.null(x$thr) && plot_thr) {
            figtmp <- figtmp + geom_hline(yintercept = x$thr, linetype = "dashed")
        }

        fig[[coef]] <- figtmp + ggtitle(coef) + theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),
                  axis.title = element_blank())
    }

    methods <- unique(levels(x$num$method), levels(x$fact$method))
    if (length(methods) > 1) {
        figs <- ggpubr::ggarrange(plotlist = fig, common.legend = TRUE, legend = "bottom")
    } else {
        figs <- ggpubr::ggarrange(plotlist = fig, legend = "none")
    }

    print(figs)
    return(invisible(figs))
}


#' @rdname combine
#' @export
c.response.train <- function(...) {
    x <- list(...)

    check_c(x, "response.train")

    cnames <- lapply(x,  `[[`, "coefs")
    if (!all(sapply(cnames, FUN = identical, y = cnames[[1]])))
        stop("All models must use same coeficients for train data.")

    return(c_response.train(x))
}
c_response.train <- function(x) {
    m <- list()

    m$num <- rbindlist(lapply(x, `[[`, "num"))
    m$fact <- rbindlist(lapply(x, `[[`, "fact"))

    m$num_resample <- rbindlist(lapply(x, `[[`, "num_resample"))
    m$fact_resample <- rbindlist(lapply(x, `[[`, "fact_resample"))

    m$coefs <- x[[1]][["coefs"]]
    m$xlevels <- x[[1]][["xlevels"]]
    m$thr <- NULL
    m$modelType <- x[[1]][["modelType"]]
    m$quantiles <- x[[1]][["quantiles"]]

    class(m) <- "response.train"
    return(m)
}


#' @export
print.response.train <- function(x, ...) {
    cat("Object of type response.train\n")
    methods <- unique(levels(x$num), levels(x$fact))
    cat("Models avaiable:", methods, "\n")
    cat("Coeficients avaiable:", x$coefs, "\n\n")
    if (nrow(x$num) > 0) print(x$num, nrows = 20)
    if (nrow(x$fact) > 0) print(x$fact, nrows = 20)
}



###################
# helper function

response.helper <- function(model, fixedDat, model.type) {

    varnumeric <- fixedDat[[1]]
    varfactor <- fixedDat[[2]]
    fixedDatan <- fixedDat[[3]]
    fixedDataf <- fixedDat[[4]]
    response_numeric <- fixedDat[[7]]
    variablen <- fixedDat[[8]]
    variablef <- fixedDat[[9]]
    tmp2_u <- fixedDat[[10]]

    out <- list()

    # predict and save output
    # for numeric
    if (length(varnumeric) > 0) {

        pred <- predict2(model, fixedDatan, type = model.type)

        out$num <- data.table(variable = factor(variablen, levels = varnumeric),
                              predictors = response_numeric,
                              response = pred)

    } else {
        out$num <- data.table(variable = factor(),
                              predictors = numeric(),
                              response = numeric())
    }

    # for factors
    if (length(varfactor) > 0) {

        pred <- predict2(model, fixedDataf, type = model.type)

        out$fact <- data.table(variable = factor(variablef, levels = varfactor),
                               factors = tmp2_u,
                               response = pred)
    } else {
        out$fact <- data.table(variable = factor(),
                               factors = character(),
                               response = numeric())
    }

    return(out)
}



response_table <- function(model, fixedvarFunction = mean, n = 100, ...) {

    # create tables with fixed values for predictions

    check_train(model)
    coefs <- getcoefs(model)
    model$trainingData <- as.data.frame(model$trainingData)

    # calculate overall response
    varnumeric <- coefs[!(coefs %in% names(model$xlevels))]
    varfactor <- names(model$xlevels)
    newdata <- model$trainingData[, coefs]

    # calculate data.frames with variable fixed
    tmp1 <- lapply(newdata[, varnumeric, drop = FALSE], fixedvarFunction, na.rm = TRUE)
    tmp2 <- lapply(newdata[, varfactor, drop = FALSE], function(x) names(which.max(table(na.omit(x)))))
    fixedData <- data.frame(Filter(function(x) length(x) > 0, list(tmp1, tmp2)))

    # for numeric data
    if (length(varnumeric) > 0) {
        fixedDatan <- fixedData[rep(1, (n * length(varnumeric))), ]
        variablen <- rep(varnumeric, each = n)

        response_numeric <- vector("numeric", n * length(varnumeric))
        for (var in varnumeric) {
            tmp <- range(model$trainingData[, var])
            response_numeric[which(variablen == var)] <- seq(tmp[1], tmp[2], length.out = n)
        }

        # calculate quantiles for rugs
        quantiles <- apply(model$trainingData[, varnumeric], 2, stats::quantile, probs = seq(0.01, 0.99, 0.01))

        # replace values in fixedData for a sequence for each variable
        for (var in varnumeric) {
            index <- which(variablen == var)
            fixedDatan[index, var] <- response_numeric[index]
        }

        if (length(varfactor) > 0)
            fixedDatan[, varfactor] <- mapply(x = fixedDatan[, varfactor],
                                              levels = model$xlevels, factor, SIMPLIFY = FALSE)
    } else {
        fixedDatan <- NULL
        response_numeric <- NULL
        quantiles <- NULL
        variablen <- NULL
    }

    # for factor data
    if (length(varfactor) > 0) {
        tmp2_u <- lapply(newdata[, varfactor, drop = FALSE], unique)
        tmp2_l <- sapply(tmp2_u, length)
        fixedDataf <- fixedData[rep(1, sum(tmp2_l)), ]
        variablef <- rep(varfactor, tmp2_l)
        tmp2_u <- unlist(tmp2_u)

        for (var in varfactor) {
            index <- variablef == var
            fixedDataf[index, var] <- as.character(tmp2_u[index])
        }

        fixedDataf[, varfactor] <- mapply(x = fixedDataf[, varfactor], levels = model$xlevels, factor, SIMPLIFY = FALSE)
    } else {
        fixedDataf <- NULL
        variablef <- NULL
        tmp2_u <- NULL
    }


    return(list(varnumeric, varfactor, fixedDatan, fixedDataf, quantiles, coefs,
                response_numeric, variablen, variablef, tmp2_u))
}



response_main <- function(model, errorFunction = ci_95, progress = FALSE, fixedDat, ...) {

    quantiles <- fixedDat[[5]]
    coefs <- fixedDat[[6]]

    coefs_obs <- c(coefs, ".outcome")
    model.type <- if (model$modelType == "Classification") "prob1" else "raw"
    model$trainingData <- as.data.frame(model$trainingData)

    # calculate response
    if (!is.null(errorFunction)) {

        # get with seed to use
        results <- model$results
        results$order <- seq(nrow(results))
        results <- merge(results, model$bestTune)
        M <- results$order

        # modify trainControl to remove resampling
        args <- create_args(model)

        # prepare progress bar
        do.par <- model$control$allowParallel && getDoParWorkers() > 1
        `%op%` <- if (do.par) `%dopar%` else  `%do%`
        opts <- NULL
        doProgress <- progress && !do.par
        
        if (progress) {
            cat(model$modelInfo$label, "\n")
            pb <- txtProgressBar(max = length(model$control$index), style = 3)
            
            if (do.par) {
                opts <- list(progress = function(n) setTxtProgressBar(pb, n))
            } else {
                counter <- 1
            }
        }


        results <- foreach(i = model$control$index, s = model$control$seeds,
                           .packages = "caret", .options.snow = opts) %op% {

            args$trControl$seeds <- s[M]
            args$data <- model$trainingData[i, coefs_obs]
            args$weights <- model$trainingData$.weights[i]

            tmp.model <- invisible(do.call("train", args))

            if (doProgress) {
                setTxtProgressBar(pb, counter)
                counter <- counter + 1
            }

            # create response
            response.helper(tmp.model, fixedDat, model.type)
        }

        if (progress) {
            close(pb)
            cat("\n")
        }

        names(results) <- names(model$control$index)
        num <- rbindlist(lapply(results, `[[`, "num"), idcol = "Resample")
        fact <- rbindlist(lapply(results, `[[`, "fact"), idcol = "Resample")
        num[, c("Resample", "method") := .(factor(Resample), factor(model$modelInfo$label))]
        fact[, c("Resample", "method") := .(factor(Resample), factor(model$modelInfo$label))]

        # calculate mean and error
        out <- list()
        out$num <- num[, .(response = mean(response), error = errorFunction(response)),
                       by = .(method, variable, predictors)]

        out$fact <- fact[, .(response = mean(response), error = errorFunction(response)),
                         by = .(method, variable, factors)]

        out$num_resample <- num
        out$fact_resample <- fact

    } else {

        # only use the final model
        out <- response.helper(model, fixedDat, model.type)

        out$num <- out$num[, .(method = factor(model$modelInfo$label),
                               variable, predictors, response, error = 0)]
        out$fact <- out$fact[, .(method = factor(model$modelInfo$label),
                                 variable, factors, response, error = 0)]

        out$num_resample <- data.table()
        out$fact_resample <- data.table()
    }

    # add other variables to the object
    out$coefs <- coefs
    out$xlevels <- model$xlevels
    out$thr <- model$thr
    out$modelType <- model$modelType
    out$quantiles <- quantiles
    class(out) <- "response.train"
    return(out)
}
