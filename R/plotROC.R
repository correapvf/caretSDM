
globalVariables(c("Specificity", "Sensitivity", "se", "sp", "sp.err", "se.err", "importance",
                  "se.lower", "sp.lower", "se.upper", "sp.upper","x","y", "thr.method","obs"))

#' @rdname ROCcurve
#' @export
ROCcurve <- function(model, ...) UseMethod("ROCcurve")

#' Plot a ROC curve
#' 
#' Plot a ROC curve from a caret model.
#' @param model A model or a list of models returned by \code{\link[caret]{train}}.
#' @param testdata A data.frame with test values to be use in ROC curve. If \code{NULL}, training values will be used.
#' @param testy A factor of response variable of \code{testdata}. If \code{NULL}, it will be guessed from \code{testdata} data.frame.
#' @param testindex A list with rows index of testdata for each resample, preferably an output of
#' \code{\link{create.test.index}} or \code{\link{create.test.index.blockCV}}. If \code{NULL}, indexes are create based on 
#' the same methods provided by \code{trainControl} used in the model. Ignored if \code{use.pROC = TRUE}.
#' @param thrs A named vector with thresholds values between [0,1] to be plotted in the ROC curve
#' or the output of \code{\link{thresholder2}}. If \code{model} is a list, this should be also a list of the same ordet
#' with thresholds to be applied for each model.
#' @param use.pROC logical. If \code{TRUE}, use \code{\link[pROC]{roc}} to compute the ROC curve and confidence intervals.
#' If \code{FALSE}, use "model" and "testindex" resamples instead.
#' @param calc.train logical. If \code{FALSE}, ROC curve for train data is not computed.
#' @param errorFunction A function used to calculate confidence interval across resamples. 
#' Function is ignored if \code{use.pROC = TRUE}. Else, the  ROC curve is calculated from the
#' mean between resamples. If \code{NULL}, confidence interval is not calculated and all train and 
#' test data are used to calculate the curve. Default is 95\% confidence interval.
#' @param ... Arguments passed to \code{predict}. Don't use object, newdata, type.
#' @return An S3 object of class 'ROCcurve', including:
#' \item{dat.roc }{A data.table with values of Sensitivity and Specificity to plot the ROC curve.}
#' \item{thrs}{A data.table with Sensitivity and Specificity for each threshold provided by \code{thrs}.}
#' \item{coords}{A data.table with the coordinates to plot the confidence interval.}
#' @seealso \code{\link{evaluate}} \code{\link{confusionMatrix2}}
#' @examples
#' \dontrun{
#' # using pROC package
#' ROCcurve(model, use.pROC = TRUE)
#' 
#' # using caret resamples, with testdata and thresholds
#' testindex <- create.test.index(testy)
#' thrs <- thresholder2(model)
#' r.obj <- ROCcurve(model, testdata, testy, testindex, thrs, use.pROC = FALSE)
#' 
#' plot(r.obj, type = "facet", select.thr = 3:7, plot_ci = TRUE, add.text = FALSE)
#' 
#' # for multiple models
#' models <- list(model1, model2, model3)
#' thrs <- lapply(models, thresholder2)
#' r.obj <- ROCcurve(models, thrs, testdata, testindex=testindex)
#' plot(r.obj, type = "test", select.thr = 3:7, plot_ci = FALSE, add.text = TRUE)
#' }
#' @rdname ROCcurve
#' @export
ROCcurve.train <- function(model, testdata = NULL, testy = NULL, testindex = NULL, thrs = NULL,
                    use.pROC = FALSE, calc.train = TRUE, errorFunction = ci_95, ...) {
    
    # check models
    plot.thrs <- !is.null(thrs)
    ci <- !is.null(errorFunction)
    
    if (!(model$modelType == "Classification" && length(model$levels) == 2))
        stop("Model should be a two class Classification problem.")
    
    check <- (model$control$savePredictions == "none" || model$control$savePredictions == FALSE) && ci
    if (check && !use.pROC) {
        if (is.null(testdata))
            stop("'savePredictions' should be TRUE, 'all', or 'final'
                 to calculate training data.")
        
        if (calc.train) {
            warning("Skipping training data. Set 'savePredictions'to TRUE, 'all', or 'final'
             to calculate training data.")
            calc.train <- FALSE
        }
        
    }
    
    if (calc.train) check_train(model)

    # check and prepare thrs data.table
    if (plot.thrs) {
        
        if (class(thrs) == "thresholder2"){
            if (thrs$method != model$method)
                stop("The model used in 'thresholder2' is different from the provided model.")
            
            if (thrs$final) {
                thrs <- thrs$thrs
            } else {
                thrs <- merge(thrs$thrs, thrs$bestTune, by=colnames(thrs$bestTune))
            }
            thrs <- thrs[, c('thr.method','prob_threshold')]
            
        } else if (is.numeric(thrs)) {
            if (is.null(names(thrs))) names(thrs) <- paste0("Thr", seq_along(thrs))
            thrs <- data.frame(thr.method = names(thrs), prob_threshold = thrs)
        } else {
            stop("'thrs' must be a vector with thresholds values or a output of thresholder2.")
        }
        
        if (any(thrs$prob_threshold < 0 | thrs$prob_threshold > 1))
            stop("thresholds must be between [0,1].")
        setDT(thrs)
        thrs_to_calc <- unique(thrs$prob_threshold)
    } else {
        thrs_to_calc <- NULL
    }
    
    
    # check index
    if(!is.null(testdata)) {
        
        testy <- get.response(model, testdata, testy)
        
        if (is.null(testindex))
            testindex <- createIndex(testy, model$control$method, model$control$number, 
                                     model$control$repeats, model$control$p)
    } else {
        # if no testdata and no traindata
        if (!calc.train)
            stop("Either use calc.train to TRUE or provide testdata.")
    }
    
    lvl <- model$levels

    
    # calculate ROC
    if (use.pROC) {
        do.par <- model$control$allowParallel && getDoParWorkers() > 1
        
        if (calc.train) {
            dat.roc <- calc.pROC(model$trainingData$.outcome, 
                                 predict(model, type="prob", ...)[,lvl[1]],
                                 lvl, "train", do.par, thrs_to_calc, ci)
        } else {
            dat.roc <- data.table()
        }
        
        if (!is.null(testdata)) {
            dat.roc.test <- calc.pROC(testy, 
                                      predict(model, testdata, type="prob", ...)[,lvl[1]],
                                      lvl, "test", do.par, thrs_to_calc, ci)
            dat.roc <- rbind(dat.roc, dat.roc.test)
        }
        
        setDT(dat.roc)
    } else {
        # use caret resamples
        if (calc.train) {
            if (ci) {
                tmp <- merge(model$pred, model$bestTune)
                setDT(tmp)
                dat.roc <- calc.caret_ci_true(tmp, lvl, "train", errorFunction, thrs_to_calc)
            } else {
                tmp <- data.table(obs = model$trainingData$.outcome, 
                                  predict(model, type="prob", ...))
                dat.roc <- calc.caret_ci_false(tmp, lvl, "train", errorFunction, thrs_to_calc)
            }
        } else {
            dat.roc <- data.table()
        }
        
        if (!is.null(testdata)) {
            tmp <- data.frame(obs = testy, 
                              predict(model, testdata, type="prob", ...))
            if (ci) {
                # resample predictions
                tmp2 <- lapply(testindex, function(x,y) y[x,], y=tmp)
                tmp2 <- rbindlist(tmp2, idcol="Resample")
                
                dat.roc.test <- calc.caret_ci_true(tmp2, lvl, "test", errorFunction, thrs_to_calc)
                dat.roc <- rbind(dat.roc, dat.roc.test)
            } else {
                dat.roc.test <- calc.caret_ci_false(tmp, lvl, "test", errorFunction, thrs_to_calc)
                dat.roc <- rbind(dat.roc, dat.roc.test)
            }
        }
    }
    
    
    
    dat.roc[, Specificity := 1-Specificity]
    
    # calculate data.frame for the thresholds
    if (plot.thrs) {
        thrs2 <- dat.roc[thrs, .(method = factor(model$modelInfo$label), 
                                 data = factor(data), 
                                 thr.method = factor(thr.method),
                                 thr, Sensitivity, Specificity), 
                         on=c(thr="prob_threshold"), nomatch = 0L]
    } else {
        thrs2 <- data.table()
    }
    
    # clear data
    dat.roc <- unique(dat.roc, by = c('Sensitivity', 'Specificity', 'data'))
    setorder(dat.roc, -data, -thr)
    
    if (ci) {
        dat.roc[, c('se.lower', 'sp.lower') := .(pmax(se.lower,0), pmax(sp.lower,0))]
        dat.roc[, c('se.upper', 'sp.upper') := .(pmin(se.upper,1), pmin(sp.upper,1))]
        
        # calculate coordinates for the ci interval
        data_coords <- rbind(
            dat.roc[,.(x=c(Specificity, rev(Specificity)), 
                       y=c(se.lower, rev(se.upper)),
                       poly=1), by="data"],
            dat.roc[,.(x=c(1-sp.lower, rev(1-sp.upper)),
                       y=c(Sensitivity, rev(Sensitivity)), 
                       poly=2), by="data"]
        )
        
        coords <- list()
        for (d in c("train","test")) {
            tmp <- data_coords[data==d]
            if (nrow(tmp) > 0) {
                dc <- split(tmp, by=c('poly','data'), keep.by=FALSE)
                ps <- lapply(dc, sp::Polygon)
                p1 <- lapply(seq_along(ps), function(i) sp::Polygons(list(ps[[i]]), ID = names(dc)[i]  ))
                p2 <- sp::SpatialPolygons(p1)
                p3 <- rgeos::gUnaryUnion(p2, checkValidity = 0L)
                p4 <- data.frame(p3@polygons[[1]]@Polygons[[1]]@coords)
                p4$data <- d
                coords[[d]] <- p4
            }
        }
        coords <- rbindlist(coords)
        coords <- coords[,.(method = factor(model$modelInfo$label), 
                            data = factor(data), 
                            Specificity=x, Sensitivity=y)]
    } else {
        coords <- data.table()
    }
    
    # create list to store results
    m <- list()
    m$dat.roc <- dat.roc[,.(method = factor(model$modelInfo$label),
                            data = factor(data), 
                            thr, Sensitivity, Specificity)]
    m$thrs <- thrs2
    m$coords <- coords
    class(m) <- "ROCcurve"
    
    return(m)
    
}



#' @rdname combine
#' @export
ROCcurve.list <- function(model, ...) {
    
    check_list(model)
    model.list <- check_names(model)
    args <- list(...)
    
    if (is.null(args$thrs)) {
        x <- lapply(model, ROCcurve, ...)
    } else {
        thrs.list <- args$thrs
        args$thrs <- NULL
        
        if (!inherits(thrs.list, "list"))
            stop("'thrs' should be a list when 'model' is a list")
        
        if (length(model) != length(thrs.list))
            stop("'model' and 'thrs' should have the same length.")
        
        x <- mapply(ROCcurve.train, model = model, thrs = thrs.list,
                    MoreArgs = args, SIMPLIFY = FALSE)
    }
    
    return(c_ROCcurve(x))
}



#' @param x An objected returned by ROCcurve.
#' @param type one of "facet", "train" or "test".
#' @param select.thr "all", "none" or a vector with indices of thresholds to plot.
#' @param plot_ci logical. Plot confidence intervals?
#' @param add.text logical. Plot threshold methods names?
#' @rdname ROCcurve
#' @export
plot.ROCcurve <- function(x, type = "facet", select.thr = "all",
                          plot_ci = TRUE, add.text = TRUE, ...) {
    dat <- levels(x$dat.roc$data)
    colors <- c("blue", "red")[c("test","train") %in% dat]
    plot.thrs <- nrow(x$thrs) > 0 && select.thr[1] != "none"
    
    if (plot.thrs) {
        select.thr <- check.thr.cutoff(select.thr, levels(x$thrs$thr.method))
        select.thr <- data.table(thr.method = select.thr)
        thrs2 <- merge(x$thrs[data==dat[1]], select.thr, by='thr.method')
        
        if (nrow(thrs2) == 0) {
            warning("'select.thr' are not avaiable. Thresholds will not be plotted.")
            plot.thrs <- FALSE
        }
    }
    
    ci <- nrow(x$coords) > 0 && plot_ci
    
    if (type == "facet") {
        
        fig <- ggplot(mapping = aes(x=Specificity, y=Sensitivity)) + 
            geom_line(data=x$dat.roc, aes(color=data)) + 
            facet_wrap(~method) +
            scale_fill_manual(values=colors) + scale_colour_manual(values=colors)
        
        if (ci) {
            fig <- fig + geom_polygon(data=x$coords, aes(fill=data), alpha= 0.2)
        }
        
        if (plot.thrs) {
            thrs2 <- merge(x$thrs[data==dat[1]], select.thr, by='thr.method')
            fig <- fig + geom_point(data=thrs2, aes(shape=thr.method))
            
            if (add.text) {
                fig <- fig + geom_text(data=thrs2, aes(label = thr.method), 
                                       hjust = "left", size = 2.5, nudge_x=0.01, show.legend = FALSE)
            }
        }
        
        
    } else if (type %in% c("train","test")) {
        if (!(type %in% dat))
            stop("'type' is ",type, " but no ",type, " data is avaiable.")
        
        dat.roc <- x$dat.roc[data==type]
        
        fig <- ggplot(mapping = aes(x=Specificity, y=Sensitivity)) + 
            geom_line(data=dat.roc, aes(color=method)) + 
            scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
        
        if (ci) {
            coords <- x$coords[data==type]
            fig <- fig + geom_polygon(data=coords, aes(fill=method), alpha= 0.2)
        }
        
        if (plot.thrs) {
            thrs2 <- merge(x$thrs[data==type], select.thr, by='thr.method')
            fig <- fig + geom_point(data=thrs2, aes(shape=thr.method, color=method))
            
            if (add.text) {
                thrs2_text <- thrs2[method==thrs2$method[1]]
                fig <- fig + geom_text(data=thrs2, aes(label = thr.method), 
                                       hjust = "left", size = 2.5, nudge_x=0.01, show.legend = FALSE)
            }
        }
        
    } else {
        stop("type must be one of 'facet', 'train' or 'test.'")
    }
    
    fig <- fig + geom_abline(slope=1, intercept = 0, linetype = "dashed") +
        coord_equal() + theme_bw() + xlab("1 - Specificity")
    
    if (length(dat) == 1) fig <- fig + guides(fill = "none", color = "none") 
    
    print(fig)
    return(invisible(fig))
}



#' @rdname combine
#' @export
c.ROCcurve <- function(...) {
    x <- list(...)
    check_c(x, "ROCcurve")
    return(c_ROCcurve(x))
}
c_ROCcurve <- function(x) {
    m <- list()
    m$dat.roc <- rbindlist(lapply(x, `[[`, "dat.roc"))
    m$thrs <- rbindlist(lapply(x, `[[`, "thrs"))
    m$coords <- rbindlist(lapply(x, `[[`, "coords"))
    class(m) <- "ROCcurve"
    
    return(m)
}



#' @export
print.ROCcurve <- function(x, ...) {
    cat("Object of type ROCcurve\n")
    cat("Models avaiable:", levels(x$dat.roc$method),"\n\n")
    print(x$dat.roc, nrows=20)
    cat("\n")
    print(x$coords, nrows=20)
    cat("\n")
    if (nrow(x$thrs) > 0) {
        print(x$thrs, nrows=20)
    } else {
        cat("No thresholds avaiable\n")
    }
    
}



############ helper functions

# helper function of plotROC.caret
# get auc curve and confidence interval using roc2 function and train resamples
calc.caret_ci_true <- function(tmp, lvl, dat, errorFunction, thrs_to_calc) {
    
    pred.thr <- sort(unique(round(tmp[[lvl[1]]], 3)))
    pred.thr <- frollmean(pred.thr, 2, algo="exact")[-1]
    pred.thr <- c(pred.thr, thrs_to_calc)
    
    x = tmp[, roc2(obs, get(lvl[1]), pred.thr, lvl), by=.(Resample)]
    
    x = x[, .(data=dat, Sensitivity=mean(se), Specificity=mean(sp), 
                    se.err=errorFunction(se), sp.err=errorFunction(sp)), by=.(thr)]
    
    x2 <- data.table(data=dat, thr=c(-Inf,Inf), Sensitivity=c(1,0), Specificity=c(0,1), se.err=0, sp.err=0)
    x = rbind(x, x2)
    
    x[, c('se.lower', 'se.upper', 'sp.lower', 'sp.upper') := 
        .(Sensitivity-se.err, Sensitivity+se.err, Specificity-sp.err, Specificity+sp.err)]
    x[, c('se.err', 'sp.err')  := NULL]
    
    return(x)
}

calc.caret_ci_false <- function(tmp, lvl, dat, errorFunction, thrs_to_calc) {
    
    pred.thr <- sort(unique(round(tmp[[lvl[1]]], 3)))
    pred.thr <- frollmean(pred.thr, 2, algo="exact")[-1]
    pred.thr <- c(pred.thr, thrs_to_calc)
    
    x = tmp[, roc2(obs, get(lvl[1]), pred.thr, lvl)]
    
    x = x[, .(data=dat, Sensitivity=mean(se), Specificity=mean(sp)), by=.(thr)]
    
    x2 <- data.table(data=dat, thr=c(-Inf,Inf), Sensitivity=c(1,0), Specificity=c(0,1))
    x = rbind(x, x2)

    return(x)
}


# helper function of plotROC.pROC
# get auc curve and confidence interval based on pROC package
calc.pROC <- function(obs, pred, lvl, dat, do.par, thrs_to_calc, ci) {
    rocObject <- pROC::roc(obs, 
                           pred, 
                           levels = rev(lvl),
                           direction = "<", quiet = TRUE)
    
    if (is.null(thrs_to_calc)) {
        thresholds <- rocObject$thresholds
        specificities <- rocObject$specificities
        sensitivities <- rocObject$sensitivities
    } else {
        coordsObj <- pROC::coords(rocObject, x = thrs_to_calc)
        thresholds <- c(rocObject$thresholds, coordsObj[, 1])
        specificities <- c(rocObject$specificities, coordsObj[, 2])
        sensitivities <- c(rocObject$sensitivities, coordsObj[, 3])
    }
    
    if (ci) {
        ciobj.se <- pROC::ci.se(rocObject, specificities = specificities,
                                progress = "none", parallel = do.par)
        
        ciobj.sp <- pROC::ci.sp(rocObject, sensitivities = sensitivities,
                                progress = "none", parallel = do.par)
        
        dat.roc <- data.frame(thr = thresholds,
                              data = dat,
                              Sensitivity = sensitivities, 
                              Specificity = specificities,
                              se.lower = ciobj.se[, 1],
                              se.upper = ciobj.se[, 3],
                              sp.lower = ciobj.sp[, 1],
                              sp.upper = ciobj.sp[, 3])
    } else {
        
        dat.roc <- data.frame(thr = thresholds,
                              data = dat,
                              Sensitivity = sensitivities, 
                              Specificity = specificities)
    }
    
    return(dat.roc)
}


# helper function for roc2
# calculate sensitivities and specificities
se.sp <- function(obs, pred) {
    x <- table(pred, obs)
    return(list(se=x[1,1]/sum(x[,1]), sp=x[2,2]/sum(x[,2])))
}


# helper function for calc.caret
# get se and sp for each threshold
roc2 <- function(obs, pred, pred.thr, lvl) {
    
    out <- data.table(thr=rep(pred.thr, each=length(obs)),
                          obs=factor(rep(obs, length(pred.thr)), levels=lvl),
                          pred=rep(pred, length(pred.thr)) )

    out[, pred := factor(pred > thr, levels=c(TRUE, FALSE), labels=lvl)]
    out <- out[, se.sp(obs, pred), by = "thr"]
    
    return(out)
}

