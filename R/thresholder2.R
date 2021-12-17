
globalVariables(c("prob_threshold", "Kappa", "Detection", "Dist", "Detection Prevalence", "parameters", "thr.method"))

#' Choose a Probability Threshold
#'
#' This function apply \code{thresholder} and return the optimal thresholds based on multiple methods (check details).
#'
#' Possible \code{thr.method} are:
#' \itemize{
#'  \item Default - The default for most models, this set the threshold to 0.5
#'  \item Min_Presence - Required threshold to have Sensitivity = 1
#'  \item 10%_Presence - Required threshold to have Sensitivity = 0.9
#'  \item Sens=Spec - Optimize threshold where sensitivity equals specificity
#'  \item MaxSens+Spec - Optimize threshold which maximaze sensitivity plus specificity
#'  \item MaxKappa - Optimize threshold which maximaze kappa
#'  \item PredPrev=Obs - Optimize threshold where Observed prevalence equals specificity
#'  \item Dist - Optimize threshold which minimize Dist (check \code{\link[caret]{thresholder}})
#'  \item Cost - Optimize threshold which maximize Cost (check \code{PresenceAbsence::optimal.thresholds})
#'  \item ReqSens - Required threshold to have Sensitivity = \code{req.sens}
#'  \item ReqSpec - Required threshold to have Specificity = \code{req.spec}
#' }
#' You can check the help documentation of \code{PresenceAbsence::optimal.thresholds} or \code{caret::thresholder2}
#' for more details. Most names of threshold methods are the same for \code{optimal.thresholds}.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param thr.method A vector with threshold methods to use when calculating the optimal threshold.
#' Can be a character or numeric vector. Check details for possible values. Default "all" is to use all methods.
#' @param thr.interval A value to create probability thresholds cutoffs intervals to be evaluated.
#' Should be between 0 and 0.1.
#' @param final logical. Should only the final tuning parameters chosen by train be used?
#' @param add.statistics A character vector indicating additional statistics to calculate.
#' See \code{\link[caret]{thresholder}} for a list of possible values.
#' Note that 'Sensitivity', 'Specificity', 'Kappa', 'Dist' and 'Detection Prevalence' are always calculated.
#' @param obs.prev Observed prevalence, in case your data is taken from a larger dataset.
#' Defaults to observed prevalence from training data.
#' @param FPC,FNC False Positive and False Negative Costs. Used only to calculate "Cost" threshold.
#' @param req.sens,req.spec Required Sensitivity and Specificity to calculate "ReqSens" and "ReqSpec" thresholds.
#' @return An S3 object of class 'thresholder2', including:
#' \itemize{
#'   \item thrs - A data.table with thresholds methods and their statistics.
#'   \item thresholder - The output of \code{\link[caret]{thresholder}}.
#'   }
#' @note By default, Youden's J statistic is also returned, which is the same as TSS (True Skill Statistic).
#' @seealso \code{\link[caret]{thresholder}}
#' @examples
#' \dontrun{
#' # Select a threshold looking at all tuning parameters
#' t.obj <- thresholder2(model, thr.method= 1:6, final = FALSE)
#' plot(t.obj, select.thr="all", statistics="all", display.final = FALSE)
#' summary(t.obj)
#'
#' # Select a threshold looking only at the final model
#' t.obj <- thresholder2(model, thr.interval = 0.005, final = TRUE)
#' plot(t.obj, select.thr=3:7)
#' thr <- summary(t.obj)
#' thr # this will hold the best threshold based on which.statistics
#'
#' as.data.frame(t.rf) # data.frame with thresholds and statistics for each method
#' }
#' @export
thresholder2 <- function(model, thr.method = "all", thr.interval = 0.01,
                         final = TRUE, add.statistics = "J",
                         obs.prev = NULL, FPC = 1, FNC = 1,
                         req.sens = 0.85, req.spec = 0.85) {

    # check models
    if (!inherits(model, "train"))
        stop("Model should be an object of class 'train'.")

    if (!(model$modelType == "Classification" && length(model$levels) == 2))
        stop("Model should be a two class Classification problem.")

    thr.names <- c("Min_Presence", "10%_Presence", "Sens=Spec", "MaxSens+Spec",
                   "MaxKappa", "PredPrev=Obs", "Dist", "Cost", "ReqSens", "ReqSpec")

    thr.method <- check.thr.cutoff(thr.method, thr.names)


    # add new statistics if provided
    stats <- c('Sensitivity', 'Specificity', 'Kappa', 'Dist', 'Detection Prevalence')
    if (is.null(add.statistics)) {
        add.statistics <- stats
    } else {
        add.statistics <- add.statistics[!(add.statistics %in% stats)]
        add.statistics <- c(stats, add.statistics)
    }

    # create thr.interval
    if (thr.interval <= 0 && thr.interval > 0.1)
        stop("'thr.interval' should be between 0 and 0.1")
    thr.interval <- seq(0, 1, thr.interval)

    parameters <- model$modelInfo$parameters$parameter

    # calculate thrs
    if (model$control$allowParallel && getDoParWorkers() > 1 && length(thr.interval) > 101) {
        thr.split <- split(thr.interval, ceiling(seq_along(thr.interval) / (length(thr.interval) / getDoParWorkers())))
        x <- foreach(thr.interval = thr.split, .combine = "rbind") %dopar% {
            caret::thresholder(model, thr.interval, final, add.statistics)
        }

    } else {
        x <- caret::thresholder(model, thr.interval, final, add.statistics)
    }

    # calculate observed prevalence
    if (is.null(obs.prev)) {
        check_train(model)
        obs.prev <- sum(model$trainingData$.outcome == model$levels[1]) / nrow(model$trainingData)
    }

    # get optimal thrs
    setDT(x)
    out <- list()

    if ("Default" %in% thr.method) {
        out[[1]] <- x[, .SD[which(prob_threshold == 0.5)], by = parameters]
        out[[1]]$thr.method <- "Default"
    }

    if ("Min_Presence" %in% thr.method) {
        out[[2]] <- x[, .SD[which.max(prob_threshold[Sensitivity >= 1])], by = parameters]
        out[[2]]$thr.method <- "Min_Presence"
    }

    if ("10%_Presence" %in% thr.method) {
        out[[3]] <- x[, .SD[which.max(prob_threshold[Sensitivity >= 0.9])], by = parameters]
        out[[3]]$thr.method <- "10%_Presence"
    }

    if ("Sens=Spec" %in% thr.method) {
        out[[4]] <- x[, .SD[which.min(abs(Sensitivity - Specificity))], by = parameters]
        out[[4]]$thr.method <- "Sens=Spec"
    }

    if ("MaxSens+Spec" %in% thr.method) {
        out[[5]] <- x[, .SD[which.max(Sensitivity + Specificity)], by = parameters]
        out[[5]]$thr.method <- "MaxSens+Spec"
    }

    if ("MaxKappa" %in% thr.method) {
        out[[6]] <- x[, .SD[which.max(Kappa)], by = parameters]
        out[[6]]$thr.method <- "MaxKappa"
    }

    if ("PredPrev=Obs" %in% thr.method) {
        out[[7]] <- x[, .SD[which.min(abs(obs.prev - `Detection Prevalence`))], by = parameters]
        out[[7]]$thr.method <- "PredPrev=Obs"
    }

    if ("Dist" %in% thr.method) {
        out[[8]] <- x[, .SD[which.min(Dist)], by = parameters]
        out[[8]]$thr.method <- "Dist"
    }

    if ("Cost" %in% thr.method) {
        out[[9]] <- x[, .SD[which.max(cost(FPC, FNC, obs.prev, Sensitivity, Specificity))], by = parameters]
        out[[9]]$thr.method <- "Cost"
    }

    if ("ReqSens" %in% thr.method) {
        out[[10]] <- x[, .SD[which.max(prob_threshold[Sensitivity >= req.sens])], by = parameters]
        out[[10]]$thr.method <- "ReqSens"
    }

    if ("ReqSpec" %in% thr.method) {
        out[[11]] <- x[, .SD[which.min(prob_threshold[Specificity >= req.spec])], by = parameters]
        out[[11]]$thr.method <- "ReqSpec"
    }

    out <- rbindlist(out)
    setcolorder(out, c(parameters, 'thr.method', 'prob_threshold', add.statistics))

    if (final) {
        out[, (parameters) := NULL]
    }

    m <- list()
    m$thrs <- out
    m$thresholder <- x
    m$final <- final
    m$method <- model$method
    m$bestTune <- model$bestTune
    m$statistics <- add.statistics

    class(m) <- "thresholder2"
    return(m)
}



#' @param select.thr A vector with threshold methods to be displayed. Can be index of
#' thr.method used in thresholder2.
#' @param statistics A character vector indicating statistics to be displayed. Can be index of
#' statistics used in thresholder2. Alternatively, can be "all" to display all available statistics
#' or Sensitivity+Specificity" to display a Sensitivity and Specificity in a single plot.
#' @param display.final logical. Should display only thresholds for the final model tuned by \code{train}?
#' This is only used if you set \code{final = FALSE} in \code{thresholder2}.
#' @param add.text logical. Plot threshold methods names? They are displayed only if \code{display.final = TRUE}.
#' @param plot logical. Return the plot or not?
#' @return For the method \code{plot}, if \code{plot = TRUE}, a ggplot is returned.
#' If \code{plot = FALSE}, a list of data.tables
#' in the long format (used by ggplot) is returned.
#' @rdname thresholder2
#' @export
plot.thresholder2 <- function(x, select.thr = "all",
                              statistics = "Sensitivity+Specificity", display.final = TRUE,
                              add.text = TRUE, plot = TRUE, ...) {

    if (!display.final && x$final) stop("To plot all possible parameters, you should set
                                'final=FALSE' in the thresholder2.")

    if (statistics[1] == "Sensitivity+Specificity") {
        statistics <- c('Sensitivity', 'Specificity')
        y <- plot.thr.helper(x, select.thr, statistics, display.final)
        thrs_text <- y$thrs[variable == "Sensitivity"]

        if (display.final) {
            fig <- ggplot(mapping = aes(x = prob_threshold, y = value, color = variable))
        } else {
            fig <- ggplot(mapping = aes(x = prob_threshold, y = value, color = parameters, linetype = variable))
        }


    } else {
        # generic case for each statistics

        # check which statistics to display
        statistics <- check.thr.cutoff(statistics, x$statistics, "statistics")

        # do the plot
        y <- plot.thr.helper(x, select.thr, statistics, display.final)


        if (display.final) {
            fig <- ggplot(mapping = aes(x = prob_threshold, y = value))
        } else {
            fig <- ggplot(mapping = aes(x = prob_threshold, y = value, color = parameters))
        }

        fig <- fig + facet_wrap(~variable, scales = "free_y")
    }

    if (plot) {
        fig <- fig +
            geom_line(data = y$thresholder) +
            geom_point(data = y$thrs, aes(shape = thr.method)) +
            theme_bw()

        if (add.text) {
            fig <- fig + geom_text(data = y$thrs, aes(label = thr.method),
                                   hjust = "left", nudge_x = 0.01, size = 2.5, show.legend = FALSE)
        }

        print(fig)
        return(invisible(fig))
    } else {
        return(y)
    }
}



#' @param x,object An object returned by thresholder2
#' @param which.statistics A single character with a statistic to be used to select the best threshold.
#' Only used if \code{which.method} is NULL.
#' @param which.method A single character with a threshold method to obtain the threshold.
#' @param maximize A logical: should the statistic be maximized or minimized?
#' @param ... ignored
#' @rdname thresholder2
#' @export
summary.thresholder2 <- function(object, which.statistics = "J", which.method = NULL,
                                 maximize=ifelse(which.statistics == "Dist", FALSE, TRUE), ...) {
    if (is.null(which.method)) {
        if (maximize) {
            thr <- as.data.frame(object$thrs[which.max(object$thrs[[which.statistics]])])
        } else {
            thr <- as.data.frame(object$thrs[which.min(object$thrs[[which.statistics]]), ])
        }
        if (nrow(thr) == 0) stop("which.statistics must be one of: ", paste(object$statistics, collapse = " "))
    } else {
        thr <- as.data.frame(object$thrs[thr.method == which.method])
        if (nrow(thr) == 0) stop("which.method must be one of: ", paste(object$thrs$thr.method, collapse = " "))
    }

    cat("The best threshold method was", thr[, "thr.method"])
    cat(" with a threshold of", thr[, "prob_threshold"])
    cat(" and a", which.statistics, "of", round(thr[, which.statistics], 3), "\n")

    if (!object$final) {
        tmp <- thr[, colnames(object$bestTune), drop = FALSE]
        cat("The best model selected was:\n")
        print(tmp)
        if (identical(as.data.table(tmp), as.data.table(object$bestTune))) {
            return(invisible(thr[, "prob_threshold"]))
        } else {
            cat("The model with the best statistics is not the finalModel selected by 'train'")
            tmp$thr <- thr[, "prob_threshold"]
            return(invisible(tmp))
        }
    } else {
        return(invisible(thr[, "prob_threshold"]))
    }
}



#' @export
print.thresholder2 <- function(x, ...) {
    cat("Object of type thresholder for", x$method, "\n")
    cat("Thresholds avaiable:", unique(x$thrs$thr.method), "\n\n")
    print(x$thrs, nrows = 20)
}



#' @rdname thresholder2
#' @export
as.data.frame.thresholder2 <- function(x, ...) return(as.data.frame(x$thrs))



############ helper functions

# modify the output of thresholder2 to a long format to ggplot2
plot.thr.helper <- function(x, select.thr, statistics, display.final) {

    # select thr
    if (x$final) {
        thrs <- x$thrs
    } else {
        thrs <- merge(x$thrs, x$bestTune, by = colnames(x$bestTune))
    }

    select.thr <- check.thr.cutoff(select.thr, unique(x$thrs$thr.method))
    select.thr <- data.table(thr.method = select.thr)
    thrs <- merge(thrs, select.thr, by = 'thr.method')

    if (display.final) {

        thrs <- melt(thrs, id.vars = c('thr.method', 'prob_threshold'), measure.vars = statistics)

        thresholder <- merge(x$thresholder, x$bestTune)
        thresholder <- melt(thresholder, id.vars = 'prob_threshold', measure.vars = statistics)

    } else {

        parameters <- colnames(x$bestTune)

        thrs <- melt(thrs, id.vars = c(parameters, 'thr.method', 'prob_threshold'), measure.vars = statistics)
        thresholder <- melt(x$thresholder, id.vars = c(parameters, 'prob_threshold'), measure.vars = statistics)

        thrs[, parameters := interaction(.SD, sep = "_"), .SDcols = parameters]
        thresholder[, parameters := interaction(.SD, sep = "_"), .SDcols = parameters]

    }

    out <- list(thrs = thrs, thresholder = thresholder)
    return(out)

}


# calculate cost function
cost <- function(FPC, FNC, obs.prev, Sensitivity, Specificity) {
    sl <- (FPC / FNC) * (1 - obs.prev) / obs.prev
    x <- (1 - Specificity)
    y <- Sensitivity
    rad <- (x^2 + y^2)^.5
    theta.new <- atan2(y, x) - atan(sl)
    y.new <- rad * sin(theta.new)
    return(y.new)
}


# check if thresholds provided in "thr.method" are valid based on "thr.names"
# if "all", all "thr.names" are returned, or "thr.method" can also be number,
# representing index of thr.names
check.thr.cutoff <- function(thr.method, thr.names, arg.name = "thr.method") {

    # check if thr.method is correct
    if (is.character(thr.method)) {
        if (thr.method[1] == "all") {
            thr.method <- thr.names
        } else {
            if (!all(thr.method %in% thr.names)) stop(arg.name, " must be one of ", paste(thr.names, , collapse = " "))
        }

    } else if (is.numeric(thr.method)) {
        if (!all(thr.method %in% seq_along(thr.names))) stop(arg.name, " must be numbers from 1 to ", length(thr.names))
        thr.method <- thr.names[thr.method]
    } else {
        stop(arg.name, " must be either a character or numeric vector")
    }
    return(thr.method)
}
