% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.R
\name{response}
\alias{response}
\alias{response.train}
\alias{plot.response.train}
\title{Calculate response plots for train models}
\usage{
response(model, ...)

\method{response}{train}(
  model,
  fixedvarFunction = mean,
  errorFunction = ci_95,
  n = 100,
  progress = FALSE,
  ...
)

\method{plot}{response.train}(
  x,
  plot_errorbar = TRUE,
  plot_rugs = TRUE,
  free_y = TRUE,
  plot_thr = TRUE,
  ...
)
}
\arguments{
\item{model}{A model returned by \code{\link[caret]{train}}.}

\item{...}{ignored}

\item{fixedvarFunction}{A function used to fix as constant the other variables when the predicting responses.}

\item{errorFunction}{A function used to calculate error across resamples. Default is 95\% confidence interval.
If \code{NULL}, responses are calculated from the final model only.}

\item{n}{Number of responses to get for each variable, as in seq(min(variable), max(variable), length.out=n).}

\item{progress}{logical. Show progress bar?}

\item{x}{An object returned by \code{response}.}

\item{plot_errorbar}{logical. Should plot errors? Only valid if \code{resample = TRUE}.}

\item{plot_rugs}{logical. Should plot rugs representing quantiles?}

\item{free_y}{logical. Free or fixed y axis in plots?}

\item{plot_thr}{logical. Should plot a line representing the probability threshold of the model?
A threshold must be set using \code{setThreshold} and its only plotted if there is only one model
(to avoid to much noise in the plot).}
}
\value{
An S3 object of class 'response.train', including:
\itemize{
  \item{num and fact}{Data.tables with responses for each variable of type numeric and factors.
  If \code{errorFunction != NULL}, responses are means from resamples and errors are also provided.}
  \item{num_resample and fact_resample}{Data.tables with responses for each resample,
  only provided if \code{errorFunction != NULL}.}
  \item{quantiles}{A matrix with quantiles for each variable used to plot rugs.}
}
}
\description{
Evaluation Strip method proposed by Elith et al.(2005),
applied to models created using \code{caret::train}.
Here we use the same process as described in \code{biomod2::response.plot2}.
}
