% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxent.R, R/maxentCaret.R
\docType{data}
\name{maxent}
\alias{maxent}
\alias{maxent.default}
\alias{maxent.formula}
\alias{maxent.matrix}
\alias{maxentCaret}
\title{Maximum Entropy model}
\format{
An object of class \code{list} of length 12.
}
\usage{
maxent(x, ...)

\method{maxent}{default}(
  x,
  y,
  reg = "lqph",
  beta = 1,
  niter = 500,
  outputType = "Cloglog",
  thrtype = "MaxSens+Spec",
  clamp = TRUE,
  filesPath = paste0(tempdir(), "/maxent"),
  maxentPath = NULL,
  flags = NULL,
  ...
)

\method{maxent}{formula}(form, data, ..., subset, na.action)

\method{maxent}{matrix}(x, y, ..., categorical = NULL)

maxentCaret
}
\arguments{
\item{x}{A data frame of predictors}

\item{...}{Arguments passed to default method.}

\item{y}{A response factor, same length as \code{nrow(x)}. \strong{First level is assumed to be "presence" data}.}

\item{reg}{Regularization features to use. l - linear, q - quadratic, p - product, t - threshold, h - hinge}

\item{beta}{Beta multiplier}

\item{niter}{Maximum number of iterations}

\item{outputType}{One of 'Cloglog', 'Logistic', 'Cumulative', 'Raw'}

\item{thrtype}{Which threshold type to use. One of 'Min_Presence','10\%_Presence',
'Sens=Spec','MaxSens+Spec','Balance','Entropy'}

\item{clamp}{logical. Apply clamping in prediction?}

\item{filesPath}{Path used to store MaxEnt output files}

\item{maxentPath}{Path of maxent.jar. If NULL, it will use maxent.jar from the dismo package}

\item{flags}{Other flags to maxent. Should be in the format
\code{c('addsamplestobackground=false', 'beta_hinge=1.5')}.}

\item{form}{A formula of the form y ~ x1 + x2 + ...}

\item{data}{Data frame from which variables specified in formula}

\item{subset}{An index vector specifying the cases to be used in the training sample.}

\item{na.action}{A function to specify the action to be taken if NAs are found.
The default action is for the procedure to fail.}

\item{categorical}{A character vector with column names to be treated as categorical variable.}
}
\value{
An S3 object of class 'maxent' build using maxent.jar, including:
\itemize{
  \item path - The path to the files generated by maxent.jar.
  \item params - A list with parameters used to build the model.
  \item results - A data.frame with multiple results from the model.
  \item predicted - A vector with predictions made by the model on the trained data.
}
}
\description{
Create a "MaxEnt" (Maximum Entropy) species distribution model.
}
\details{
This function will use the java implementation of MaxEnt by Phillips,
Dudik and Schapire, instead of the package maxnet.
Check the MaxEnt documentation for more information.
Only suitable for presence/absence or presence/background data.
This is similar to \code{\link[dismo]{maxent}}, with less options (for instance, this will not accept rasters),
but it is faster, specially the predict function.
This function do not support maxent.jar replicates, as those should be handled by caret.
}
\note{
When using a data.frame, variables defined as factors are automatically assigned as
categorical variables. When using a matrix or a formula with caret, you must the argument
\code{categorical} if any of the variables are factors/categorical.\cr
Due to the way \code{caret::train} handles the data when using a formula, it is not possible to
automatically get which variables are categorical.
}
\examples{
\dontrun{
maxent(x, y)
maxent(x, y, reg = "lq", maxentPath="~/maxent.jar", filesPath="/run/shm/maxent")

# using caret
# use 'categorical' if there are categorical  variables in traindata
model.maxent = train(species ~ ., data=traindata,
                     method=maxentCaret, metric="ROC", trControl = control,
                     tuneGrid = expand.grid(reg = c("lq","l"), beta = seq(0.5, 2, 0.2)),
                     categorical = c("factor1", "factor2"))
}
}
\seealso{
\code{\link{methods.maxent}}
}
\keyword{datasets}
