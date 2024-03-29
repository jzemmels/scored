% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featureCalculationHelpers.R
\name{feature_aLaCarte}
\alias{feature_aLaCarte}
\title{Calculate similarity features based on the output of the comparison procedure}
\usage{
feature_aLaCarte(
  comparisonData,
  features = "all",
  id_cols = NULL,
  quiet = FALSE,
  ...
)
}
\arguments{
\item{comparisonData}{tibble such as one returned by the
\code{comparison_cellBased()} or \code{comparison_fullScan()} functions that contains
results from the cell-based or full scan comparison procedure}

\item{features}{character vector containing the features to be calculated.}

\item{id_cols}{variable(s) to group by prior to calculating the summary
statistics}

\item{quiet}{Boolean to suppress function messages}

\item{...}{additional parameters for the feature calculation functions. See
notes for possible additional parameters.}
}
\description{
Calculate similarity features based on the output of the comparison procedure
}
\note{
Each additional parameter should be passed as a single argument.
Possible parameters are "eps" and "minPts" used in the density-based feature
calculation and "threshold" used in the visual diagnostic feature
calculation. See the documentation for feature_densityBased_all or
feature_visualDiagnostic_all to learn more about these parameters.
}
