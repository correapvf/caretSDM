
#' Calculates performance metrics for two classes
#'
#' Functions to calculate performance metrics for two class Classification problems,
#' to be used in \code{trainControl} or \code{evaluate}.
#'
#' \code{twoClassTSS} calculates ROC, sensitivity, specificity and True-Skill Statistics (TSS).
#' \code{twoClassSDM} is a wrapper for multiple metrics, and calculates kappa, AUC and TSS.
#' \code{twoClassSDM2} calculates all metrics from \code{twoClassSDM}, plus log-likelihood.
#' @param data,lev,model Check \code{\link[caret]{twoClassSummary}}.
#' @examples
#' \dontrun{
#' control <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSDM)
#' control <- trainControl(method = "cv", summaryFunction = twoClassTSS)
#' }
#' @rdname twoClassSDM
#' @export
twoClassSDM <- function(data, lev = NULL, model = NULL) {
    # modifield from caret to include TSS metric

    if (length(lev) > 2) {
        stop(paste("Your outcome has", length(lev), "levels. The twoClassSummary() function isn't appropriate."))
    }
    if (!all(levels(data[, "pred"]) == lev)) {
        stop("levels of observed and predicted data do not match")
    }

    dataComplete <- data[stats::complete.cases(data), ]
    tmp <- unlist(e1071::classAgreement(table(dataComplete[, "obs"],
                                              dataComplete[, "pred"]))
    )[c("diag", "kappa")]

    rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">",
                               quiet = TRUE), silent = TRUE)
    rocAUC <- if (inherits(rocObject, "try-error")) NA else rocObject$auc

    Se <- caret::sensitivity(data[, "pred"], data[, "obs"], lev[1])
    Sp <- caret::specificity(data[, "pred"], data[, "obs"], lev[2])

    is_class1 <- ifelse(data$obs == lev[1], 1, 0)
    prob_class1 <- data[, lev[1]]

    out <- c(tmp, rocAUC, Se, Sp, Se + Sp - 1)
    names(out) <- c("Accuracy", "Kappa", "ROC", "Sens", "Spec", "TSS")
    return(out)
}


#' @rdname twoClassSDM
#' @export
twoClassSDM2 <- function(data, lev = NULL, model = NULL) {
    # modifield from caret to include TSS metric

    if (length(lev) > 2) {
        stop(paste("Your outcome has", length(lev), "levels. The twoClassSummary() function isn't appropriate."))
    }
    if (!all(levels(data[, "pred"]) == lev)) {
        stop("levels of observed and predicted data do not match")
    }

    dataComplete <- data[stats::complete.cases(data), ]
    probs <- as.matrix(dataComplete[, lev, drop = FALSE])
    logLoss <- ModelMetrics::mlogLoss(dataComplete$obs, probs)

    tmp <- unlist(e1071::classAgreement(table(dataComplete[, "obs"],
                                              dataComplete[, "pred"]))
    )[c("diag", "kappa")]

    rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">",
                               quiet = TRUE), silent = TRUE)
    rocAUC <- if (inherits(rocObject, "try-error")) NA else rocObject$auc

    Se <- caret::sensitivity(data[, "pred"], data[, "obs"], lev[1])
    Sp <- caret::specificity(data[, "pred"], data[, "obs"], lev[2])

    is_class1 <- ifelse(data$obs == lev[1], 1, 0)
    prob_class1 <- data[, lev[1]]

    out <- c(tmp, rocAUC, Se, Sp, Se + Sp - 1, logLoss)
    names(out) <- c("Accuracy", "Kappa", "ROC", "Sens", "Spec", "TSS", "logLoss")
    return(out)
}

#' @rdname twoClassSDM
#' @export
twoClassTSS <- function(data, lev = NULL, model = NULL) {
    # modifield from caret to include TSS metric

    if (length(lev) > 2) {
        stop(paste("Your outcome has", length(lev), "levels. The twoClassTSS() function isn't appropriate."))
    }

    if (!all(levels(data[, "pred"]) == lev)) {
        stop("levels of observed and predicted data do not match")
    }

    Se <- caret::sensitivity(data[, "pred"], data[, "obs"], lev[1])
    Sp <- caret::specificity(data[, "pred"], data[, "obs"], lev[2])

    out <- c(Se, Sp, Se + Sp - 1)
    names(out) <- c("Sens", "Spec", "TSS")
    return(out)
}



#' @rdname confusionMatrix2
#' @export
confusionMatrix2 <- function(model, ...) UseMethod("confusionMatrix2")


#' Confusion Matrix for a caret model
#'
#' This is a wrapper function for \code{\link[caret]{confusionMatrix}},
#' but it accepts directly a caret model and a data.frame with test data.
#' @param model A model returned by \code{\link[caret]{train}}.
#' @param testdata A data.frame with test values to be evaluated.
#' If \code{NULL}, training values will be used.
#' @param testy A factor of response variable of \code{testdata}.
#' If \code{NULL}, it will be guessed from \code{testdata} data.frame.
#' @param ... Further arguments to \code{\link[caret]{confusionMatrix}}.
#' @note Using \code{confusionMatrix2(model)} (without test data) will produce simple training metrics, unlike
#' \code{confusionMatrix(model)}, which is based on resampling procedure.
#' @seealso \code{\link{evaluate}} \code{\link{ROCcurve}}
#' @rdname confusionMatrix2
#' @export
confusionMatrix2.train <- function(model, testdata = NULL, testy = NULL, ...) {
    if (is.null(testdata)) {
        check_train(model)
        cmatrix <- caret::confusionMatrix(predict2(model), model$trainingData$.outcome, ...)
    } else {
        cmatrix <- caret::confusionMatrix(predict2(model, testdata),
                                          get.response(model, testdata, testy), ...)
    }
    return(cmatrix)
}


#' @rdname combine
#' @export
confusionMatrix2.list <- function(model, ...) {
    check_list(model)
    model.list <- check_names(model)

    return(lapply(model, confusionMatrix2.train, ...))
}


#' Create resampling index for test data
#'
#' This function provides resamples for test data to be used in \code{\link{evaluate}} or \code{\link{ROCcurve}}
#' Arguments are similar to \code{\link[caret]{trainControl}}.
#'
#' When \code{control} is provided, arguments "method", "number",
#' "repeats" and "p" are obtained from the \code{trainControl}
#' object and used to create the index for test data. When \code{control = NULL}, the arguments provided
#' in this function are used instead.
#' @param testy A factor of response variable of the test data.
#' @param control A \code{trainControl} object or a \code{train} model from which a trainControl object is used.
#' @param method,number,repeats,p Check \code{\link[caret]{trainControl}}for details.
#' Only used if \code{control = NULL}.
#' @return A list of test data index to be used in each resampling.
#' @seealso \code{\link{create.test.index.blockCV}}
#' @export
create.test.index <- function(testy, control = NULL, method = "boot",
                              number = ifelse(grepl("cv", method), 10, 25), repeats = 10, p = 0.75) {

    if (is.null(control)) {
        index <- createIndex(testy, method, number, repeats, p)
    } else {
        if (inherits(control, "train")) control <- control$control
        index <- createIndex(testy, control$method, control$number, control$repeats, control$p)
    }

    return(index)
}
