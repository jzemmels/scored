% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparisonHelper.R
\name{comparison_cellBased}
\alias{comparison_cellBased}
\alias{comparison_fullScan}
\alias{comparison_aces}
\title{Estimate the registration between two cartridge case scans using a cell-based
or full scan comparison procedure}
\usage{
comparison_cellBased(
  reference,
  target,
  direction = "one",
  thetas = seq(-30, 30, by = 3),
  numCells = c(8, 8),
  maxMissingProp = 0.99,
  sideLengthMultiplier = 3,
  returnX3Ps = TRUE
)

comparison_fullScan(
  reference,
  target,
  thetas = seq(-30, 30, by = 3),
  returnX3Ps = TRUE
)

comparison_aces(..., x3p_labels = NULL, standardize_resolutions = FALSE)
}
\arguments{
\item{reference}{an x3p object}

\item{target}{another x3p object}

\item{thetas}{a numeric vector of arbitrary length containing rotation values
to be considered in the registration procedure}

\item{numCells}{a numeric vector of length 2 containing the number of cells to
divide the c(rows,cols) of a cartridge case surface matrix into.}

\item{maxMissingProp}{maximum proportion of missing values allowed for each
cell/region.}

\item{sideLengthMultiplier}{ratio between the target region and reference cell
side lengths. For example, sideLengthMultiplier = 3 implies each region will
be 9 times larger than its paired reference cell.}

\item{returnX3Ps}{A Boolean to return the cellHeightValues and
alignedTargetCells for each cell index. Note that setting this argument to
TRUE is required to calculate the visual diagnostic features}

\item{...}{either a sequence of x3p objects separated by commas like
'x3p1,x3p2,x3p3,...', a single list of x3p objects separated by commas like
'list(x3p1,x3p2,x3p3,...)', or a vector of x3p file paths like
'c("path/to/file1.x3p", "path/to/file2.x3p", "path/to/file3.x3p")'}

\item{x3p_labels}{(optional) character vector of x3p labels}
}
\description{
The \code{comparison_cellBased} function uses a cell-based comparison procedure
implemented in the \code{cmcR::comparison_allTogether()}. The \code{comparison_fullScan}
estimates the registration by comparing the full scans.
}
\note{
The distinction between the "reference" and "target" scans is arbitrary,
yet necessary for keeping track of the comparison direction in which a
particular observation was calculated.
}
\examples{
data("K013sA1","K013sA2")

compData_cellBased <- comparison_cellBased(reference = K013sA1,
                                           target = K013sA2,
                                           thetas = c(-3,0,3))

compData_cellBased

compData_fullScan <- comparison_fullScan(reference = K013sA1,
                                         target = K013sA2,
                                         thetas = c(-3,0,3))

compData_fullScan

}
\seealso{
\code{cmcR::comparison_allTogether()}
}
