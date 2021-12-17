
#' Control parameters for train
#'
#' Similar to the \code{caret::trainControl}, but with additional options
#'
#' This function has two more keys arguments compared to \code{caret::trainControl}:
#' 'spatial.folds' and 'presence.only'.
#' When spatial.folds is specified, the sampling (as defined by \code{method}) is applied in all folds, except one.
#' This fold is used as held-out is the model and the process is repeated for each fold. \cr
#' When presence.only is set to \code{TRUE}, than the sampling is done only in the presence data,
#' and all absence records are used in each replicate.
#' This should be set to \code{TRUE} when using presence/background data.
#'
#' @param method one of 'cv', 'repeatedcv', 'boot', 'lgocv', 'none', 'adaptive_cv', 'adaptive_boot' and 'adaptive_lgocv'
#' @param number,repeats,p Same as in \code{caret::trainControl}. Check its help file for more details.
#' @param y Response variable. Only used if \code{presence.only} is \code{TRUE}.
#' First level is assumed to be "presence" data.
#' @param spatial.folds A vector or factor of fold designation. Must be same order as y, if supplield.
#' @param presence.only If \code{TRUE}, only presence values are sampled for each method.
#' @param classProbs,summaryFunction Same as in \code{caret::trainControl}, with different defaults.
#' @param ... Other parameters passed to \code{trainControl}.
#' @return A trainControl object to be used in the `train` function.
#' @importFrom caret twoClassSummary trainControl
#' @export
trainControlSDM <- function(method = "boot", number = 10, repeats = NA, p = 0.75, y = NULL, spatial.folds = NULL,
                               presence.only = FALSE, classProbs = TRUE, summaryFunction = twoClassSDM, ...) {
    if (is.null(y) && presence.only)
        stop("You should provide 'y' if 'presence.only = TRUE'")

    if (is.null(spatial.folds)) {

        if (presence.only) {
            # presence only, no spatial folds
            pres <- y == levels(y)[1]
            y.pres <- y[pres]
            int.aus <- which(!pres)

            index <- createIndex(y.pres, method, number, repeats, p)
            indexOut <- lapply(index, function(x, y) y[!(y %in% x)], y = seq_along(y.pres))

            index <- lapply(index, append, values = int.aus)
            indexOut <- lapply(indexOut, append, values = int.aus)

            control <- trainControl(method = method, number = number, repeats = repeats, p = p,
                                    index = index, indexOut = indexOut, classProbs = classProbs,
                                    summaryFunction = summaryFunction, ...)

        } else {

            # presence/absence, no spatial folds
            control <- trainControl(method = method, number = number, repeats = repeats, p = p,
                                    classProbs = classProbs, summaryFunction = summaryFunction, ...)

        }

    } else {

        indexFold <- list()
        indexoutFold <- list()

        if (presence.only) {
            # presence only, with spatial folds
            for (i in unique(spatial.folds)) {
                y.select <- spatial.folds == i
                original.ind <- seq_along(y.select)

                y.fold <- y[!y.select]
                original.ind <- original.ind[!y.select]

                pres <- y.fold == levels(y)[1]
                y.pres <- y.fold[pres]
                int.aus <- which(!pres)

                index <- createIndex(y.pres, method, number, repeats, p)
                index <- lapply(index, append, values = int.aus)
                index <- lapply(index, function(x, y) y[x], y = original.ind)
                names(index) <- paste0(names(index), "_", i)

                indexout <- rep(list(tmp = which(y.select)), length(index))
                names(indexout) <- names(index)

                indexFold <- append(indexFold, index)
                indexoutFold <- append(indexoutFold, indexout)
            }

        } else {

            if (method == "none") { # simple spatial cv
                y  <- seq_along(spatial.folds)
            }

            # presence/absence, with spatial folds
            for (i in unique(spatial.folds)) {
                y.select <- spatial.folds == i
                original.ind <- seq_along(y.select)

                y.fold <- y[!y.select]
                original.ind <- original.ind[!y.select]

                index <- createIndex(y.fold, method, number, repeats, p)
                index <- lapply(index, function(x, y) y[x], y = original.ind)
                names(index) <- paste0(names(index), "_", i)

                indexout <- rep(list(tmp = which(y.select)), length(index))
                names(indexout) <- names(index)

                indexFold <- append(indexFold, index)
                indexoutFold <- append(indexoutFold, indexout)
            }
        }


        method <-  if (method == "none") "spatialcv" else paste0(method, "_spatialcv")

        control <- trainControl(method = method, number = number, repeats = repeats, p = p,
                                index = indexFold, indexOut = indexoutFold, classProbs = classProbs,
                                summaryFunction = summaryFunction, ...)

    }
    return(control)

}
