#'Calculate features based on visual diagnostic plots
#'
#'The `feature_visualDiagnostic_all()` calculates the six visual
#'diagnostic-based features in one call. Calculate each feature individually
#'with `feature_visualDiagnostic_neighborhoodSizeSummary()` and
#'`feature_visualDiagnostic_scanDifferenceCor()`.
#'
#'@param comparisonData tibble such as one returned by the
#'  `comparison_cellBased()` or `comparison_fullScan()` functions that contains
#'  results from the cell-based or full scan comparison procedure
#'@param thresholdMultiplier the default filtering threshold is equal to the
#'  standard deviation of the joint x3p1 and x3p2 surface matrix values. This
#'  argument can be used to scale this threshold
#'@param id_cols column names in the comparisonData tibble that uniquely
#'  identify each observation. These are returned along with the computed
#'  features
#'@param cellHeightValues list/tibble column of x3p objects containing a
#'  reference scan's cells (as returned by `comparison_cellBased()` or
#'  `comparison_fullScan()`)
#'@param alignedTargetCell list/tibble column of x3p objects containing a target
#'  scan's aligned cells (as returned by `comparison_cellBased()` or
#'  `comparison_fullScan()`)
#'@param summaryFun function that will be used to summarize the neighborhood
#'  sizes
#'@param imputeVal value to return if the feature calculation results in a
#'  non-numeric (i.e., NA, NULL) value
#'
#'@note The `feature_visualDiagnostic_all` function can be used on comparison
#'  data from a full-scan or cell-based comparison. For a full-scan comparison,
#'  note that the standard deviation features will always be NA.
#'
#' @examples
#' data("K013sA1","K013sA2")
#'
#' compData_cellBased <- comparison_cellBased(reference = K013sA1,
#'                                            target = K013sA2,
#'                                            thetas = c(-3,0,3))
#'
#' compData_cellBased %>%
#'   dplyr::group_by(cellIndex,direction) %>%
#'   feature_visualDiagnostic_all()
#'
#' compData_fullScan <- comparison_fullScan(reference = K013sA1,
#'                                          target = K013sA2,
#'                                          thetas = c(-3,0,3))
#'
#' compData_fullScan %>%
#'   dplyr::group_by(direction) %>%
#'   feature_visualDiagnostic_all() %>%
#'   dplyr::select(-c(neighborhoodSizeAve_sd,
#'             neighborhoodSizeSD_sd,
#'             differenceCor_sd))
#'
#'@rdname visualDiagnosticFeatures
#'@export
feature_visualDiagnostic_all <- function(comparisonData,
                                         thresholdMultiplier = 1,
                                         id_cols = NULL){

  diagnosticFeatures <- comparisonData %>%
    dplyr::mutate(neighborhoodSizeAve = feature_visualDiagnostic_neighborhoodSizeSummary(cellHeightValues = cellHeightValues,
                                                                                         alignedTargetCell = alignedTargetCell,
                                                                                         summaryFun = mean,
                                                                                         thresholdMultiplier = thresholdMultiplier),
                  neighborhoodSizeSD = feature_visualDiagnostic_neighborhoodSizeSummary(cellHeightValues = cellHeightValues,
                                                                                        alignedTargetCell = alignedTargetCell,
                                                                                        summaryFun = sd,
                                                                                        thresholdMultiplier = thresholdMultiplier),
                  differenceCor = feature_visualDiagnostic_scanDifferenceCor(cellHeightValues = cellHeightValues,
                                                                             alignedTargetCell = alignedTargetCell,
                                                                             thresholdMultiplier = thresholdMultiplier),
                  filteredRatio = feature_visualDiagnostic_filteredRatio(cellHeightValues = cellHeightValues,
                                                                         alignedTargetCell = alignedTargetCell,
                                                                         thresholdMultiplier = thresholdMultiplier))

  if(!is.null(id_cols)){
    diagnosticFeatures <- dplyr::group_by(diagnosticFeatures,id_cols)
  }

  diagnosticFeatures %>%
    dplyr::summarise(neighborhoodSizeAve_ave = mean(neighborhoodSizeAve,na.rm = TRUE),
                     neighborhoodSizeAve_sd = sd(neighborhoodSizeAve,na.rm = TRUE),
                     neighborhoodSizeSD_ave = mean(neighborhoodSizeSD,na.rm = TRUE),
                     neighborhoodSizeSD_sd = sd(neighborhoodSizeSD,na.rm = TRUE),
                     differenceCor_ave = mean(differenceCor,na.rm = TRUE),
                     differenceCor_sd = sd(differenceCor,na.rm = TRUE),
                     filteredRatio_ave = mean(filteredRatio,na.rm = TRUE),
                     filteredRatio_sd = sd(filteredRatio,na.rm = TRUE))

}

#' @rdname visualDiagnosticFeatures
#' @export
feature_visualDiagnostic_filteredRatio <- function(cellHeightValues,
                                                   alignedTargetCell,
                                                   thresholdMultiplier = 1){

  purrr::map2_dbl(cellHeightValues,
                  alignedTargetCell,
                  function(x3p1,x3p2){

                    cutoffThresh <- impressions::x3p_sd(x3p1,x3p2)

                    averageBinarized <- x3p1 %>%
                      impressions::x3p_to_dataFrame(preserveResolution = FALSE) %>%
                      dplyr::mutate(value = (abs(c({x3p1$surface.matrix - x3p2$surface.matrix})) > cutoffThresh)) %>%
                      summarise(filteredRatio = sum(!value,na.rm = TRUE)/sum(value,na.rm = TRUE)) %>%
                      pull(filteredRatio)


                  })

}

#' @rdname visualDiagnosticFeatures
#' @export
feature_visualDiagnostic_neighborhoodSizeSummary <- function(cellHeightValues,
                                                             alignedTargetCell,
                                                             summaryFun = mean,
                                                             thresholdMultiplier = 1,
                                                             imputeVal = NA){

  purrr::map2_dbl(cellHeightValues,
                  alignedTargetCell,
                  function(x3p1,x3p2){

                    cutoffThresh <- impressions::x3p_sd(x3p1,x3p2)

                    averageBinarized <- x3p1 %>%
                      impressions::x3p_to_dataFrame(preserveResolution = FALSE) %>%
                      dplyr::mutate(value = (abs(c({x3p1$surface.matrix - x3p2$surface.matrix})) > cutoffThresh))

                    suppressWarnings({

                      averageMat <- averageBinarized %>%
                        dplyr::mutate(x = x+1,
                                      y = y+1) %>%
                        as.data.frame() %>%
                        dplyr::select(x,y,value) %>%
                        imager::as.cimg() %>%
                        as.matrix()

                    })

                    averageMatCopy <- averageMat
                    averageMat[is.na(averageMat)] <- 0

                    # we pad the matrix so that the contours one the edge blobs are properly
                    # identified.
                    averageMat  <- averageMat %>%
                      imager::as.cimg() %>%
                      imager::pad(nPix = 10,axes = "xy",val = 0)

                    labels <- imager::label(averageMat)

                    labels[is.na(averageMatCopy)] <- NA
                    labels[averageMatCopy == 0] <- -1

                    blobSizes <- labels %>%
                      as.data.frame() %>%
                      dplyr::filter(value > -1 & !is.na(value)) %>%
                      dplyr::group_by(value) %>%
                      dplyr::tally() %>%
                      dplyr::pull(n)

                    ret <- summaryFun(blobSizes,na.rm = TRUE)

                    if(is.null(ret) | is.na(ret)){
                      return(imputeVal)
                    }

                    return(ret)

                  })

}

#' @rdname visualDiagnosticFeatures
#' @export
feature_visualDiagnostic_scanDifferenceCor <- function(cellHeightValues,
                                                       alignedTargetCell,
                                                       thresholdMultiplier = 1,
                                                       imputeVal = NA){

  purrr::map2_dbl(cellHeightValues,alignedTargetCell,
                  function(x3p1,x3p2){

                    cutoffThresh <- impressions::x3p_sd(x3p1,x3p2)*thresholdMultiplier

                    x3p1Differences <- impressions::x3p_filter(x3p = x3p1,
                                                               cond = function(x,y,thresh) abs(x - y) > thresh,
                                                               y = c(x3p2$surface.matrix),
                                                               thresh = cutoffThresh)

                    x3p2Differences <- impressions::x3p_filter(x3p = x3p2,
                                                               cond = function(x,y,thresh) abs(x - y) > thresh,
                                                               y = c(x3p1$surface.matrix),
                                                               thresh = cutoffThresh)

                    ret <- cor(c(x3p1Differences$surface.matrix),
                               c(x3p2Differences$surface.matrix),
                               use = "pairwise.complete.obs")

                    if(is.null(ret) | is.na(ret)){
                      return(imputeVal)
                    }

                    return(ret)

                  })

}

