% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importance.R
\name{varImp2}
\alias{varImp2}
\alias{varImp2.train}
\alias{plot.varImp2}
\alias{summary.varImp2}
\title{Calculate variable importance}
\usage{
varImp2(model, ...)

\method{varImp2}{train}(model, nperm = 1, errorFunction = ci_95, ...)

\method{plot}{varImp2}(x, scale = FALSE, plot_errorbar = TRUE, ...)

\method{summary}{varImp2}(object, scale = FALSE, ...)
}
\arguments{
\item{model}{A model returned by \code{\link[caret]{train}}.}

\item{...}{ignored}

\item{nperm}{Number of permutations for each variable.}

\item{errorFunction}{A function used to calculate errors. Function must accept \code{na.rm}.
Only used if \code{nperm > 1}.}

\item{x, object}{An object from \code{varImp2}.}

\item{scale}{logical. Return scaled values from 0 to 100?}

\item{plot_errorbar}{logical. Should plot error bars? Error bars are only plotted if \code{scaled = FALSE}.}
}
\value{
An S3 object of class 'varImp2', including:
\itemize{
  \item importance - A data.table with variables importance, importance from 0 to 100,
  and errors across permutations.
  \item resamples - A data.table with correlations for each permutation.
  }
}
\description{
A generic method for calculating variable importance for objects produced by \code{train}.
}
\details{
The process of calculating variable importance is different from \code{caret::varImp(model, useModel = TRUE)}.
Here we use the same process as described in \code{biomod2::variables_importance},
but the function will directly accept a caret model.
If available, multiple cores are used to compute correlations.
}
\examples{
\dontrun{
v.obj <- varImp2(model)
summary(v.obj)
summary(v.obj, scale = FALSE)
plot(v.obj)

# for multiple models
v.obj <- varImp2(list(model1, model2, model3), nperm = 25)
plot(v.obj, scale = FALSE)
}
}
