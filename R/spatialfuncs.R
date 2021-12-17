
#' Create Spatial blocks using ENMeval
#'
#' @param method One of 'block', 'check1', 'check2'. Check ENMeval documentation
#' @param y A response factor
#' @param coords Two-column data.frame or matrix of longitude and latitude, same order as y
#' @param presence.level character. Value in y which represents presence. By default, the first level
#' of y is considered presence.
#' @param env,aggregation.factor used only for methods 'check1' and 'check2'
#' @return A vector with folds to be used in \code{\link{trainControlSDM}}.
#' @family spatial
#' @export
blockENM2fold <- function(method = "block", y, coords, presence.level = NULL, env = NULL, aggregation.factor = NULL) {
    requireNamespace("ENMeval", quietly = TRUE)

    if (is.null(presence.level)) presence.level <- levels(y)[1]
    pres <- y == presence.level
    int_order <- order(c(which(pres), which(!pres)))

    occ <- coords[pres, ]
    bg.coords <- coords[!pres, ]

    folds <- switch(method,
                    block = ENMeval::get.block(occ, bg.coords),
                    check1 = ENMeval::get.checkerboard1(occ, env, bg.coords, aggregation.factor),
                    check2 = ENMeval::get.checkerboard2(occ, env, bg.coords, aggregation.factor)
    )

    return(c(folds$occ.grp, folds$bg.grp)[int_order])
}



#' Convert folds from blockCV to trainControl in caret
#'
#' @note \code{biomod2Format} argument of the \pkg{blockCV} functions must be set to \code{FALSE}.
#' @param object object from spatialBlock, buffering or envBlock functions of the \pkg{blockCV} package.
#' @return a list that can be passed to index and indexOut of \code{\link[caret]{trainControl}}.
#' @examples
#' \dontrun{
#' sb <- spatialBlock(speciesData = sptrain, biomod2Format = FALSE) # biomod2Format must be FALSE
#' indices <- blockCV2fold(sb)
#' control <- trainControl(index = indices$index, indexOut = indices$indexOut)
#'
#' # This will result in the same output
#' control <- trainControlSDM(method = "none", spatial.folds = sb$foldID)
#' }
#' @family spatial
#' @export
blockCV2fold <- function(object) {
    return(list(index = lapply(object$folds, `[[`, 1), indexOut = lapply(object$folds, `[[`, 2)))
}



#' Create resampling index for test data using blockCV
#'
#' This function provides resamples for test data to be used in \code{\link{evaluate}} or \code{\link{ROCcurve}}
#' The resamples are based in the blocks created by \code{\link[blockCV]{spatialBlock}}.
#'
#' The folds of each block are assigned to the test data based on their location.
#' If a point does not fall over any block, then the fold of the block closest to that point is
#' assigned, with a warning.\cr\cr
#' For each resample, all folds except one are used, and this is done for each fold.\cr\cr
#' The order of points in the spatialPoints object must be the same from the data.frame used for the test data.
#' @param speciesData A spatialPoints or spatialPointsDataFrame with the locations for each test data.
#' @param sb A \code{spatialBlock} object.
#' @return A list of test data index to be used in each resampling.
#' @family spatial
#' @export
create.test.index.blockCV <- function(speciesData, sb) {
    tmp <- sp::over(speciesData, sb$blocks)
    folds <- tmp$folds

    check <- is.na(folds)
    if (any(check)) {
        warning(sum(check), " points were outside polygons. Their folds will
                be assigned based on the closest block.")

        for (i in which(check)) {
            gDists <- rgeos::gDistance(speciesData[i, ], sb$blocks, byid = TRUE)
            folds[i] <- sb$blocks$folds[which.min(gDists)]
        }
    }

    index <- list()
    each.fold <- unique(folds)
    each.fold <- each.fold[order(each.fold)]
    for (f in each.fold) {
        index[[paste0("Fold", f)]] <- which(folds != f)
    }

    return(index)
}
