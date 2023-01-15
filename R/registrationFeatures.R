#'Calculate features based on the cell-based or full scan registration
#'procedures
#'
#'The `feature_registration_all` function calculates the seven
#'registration-based features in one call. The `feature_registration_summary`
#'function is an exported helper.
#'
#'@param comparisonData tibble such as one returned by the
#'  `comparison_cellBased()` or `comparison_fullScan()` functions that contains
#'  results from the cell-based or full scan comparison procedure
#'@param cellIndex tibble column containing cell indices
#'@param direction tibble column indicating whether the associated row came from
#'  the "reference_vs_target" or "target_vs_reference" comparison
#'@param fft_ccf tibble column containing cross-correlation function values
#'@param summaryVar tibble column that is to be summarized
#'@param summaryFun function that will be used to summarize the values in the
#'  `summaryVar` column
#'@param imputeVal value to return if the feature calculation results in a
#'  non-numeric (i.e., NA, NULL) value
#'@param id_cols variable(s) to group by prior to calculating the summary
#'  statistics
#'
#'@note The `feature_registration_all` function can be used on comparison data
#'  from a full-scan or cell-based comparison. For a full-scan comparison, we
#'  recommend using only the average CCF and average pairwise-complete
#'  correlation.
#'
#'@note To calculate  the visual diagnostic features, you must set the
#'  returnX3Ps argument in the `comparison_cellBased()` or
#'  `comparison_fullScan()` functions to TRUE.
#'
#' @examples
#' data("K013sA1","K013sA2")
#'
#' compData_cellBased <- comparison_cellBased(reference = K013sA1,
#'                                            target = K013sA2,
#'                                            thetas = c(-3,0,3))
#'
#' feature_registration_all(compData_cellBased)
#'
#' compData_fullScan <- comparison_fullScan(reference = K013sA1,
#'                                          target = K013sA2,
#'                                          thetas = c(-3,0,3))
#'
#' feature_registration_all(compData_fullScan) %>%
#'   dplyr::select(ccfMean,pairwiseCompCorMean)
#'
#'@rdname registrationFeatures
#'@export
feature_registration_all <- function(comparisonData,id_cols = NULL){

  if(!is.null(id_cols)){

    comparisonData <- comparisonData %>%
      dplyr::group_by(!!as.name(id_cols))

  }

  comparisonData  %>%
    dplyr::summarise(ccfMean = feature_registration_summary(cellIndex = cellIndex,
                                                            direction = direction,
                                                            fft_ccf = fft_ccf,
                                                            summaryVar = fft_ccf,
                                                            summaryFun = mean,
                                                            imputeVal = NA),
                     ccfSD = feature_registration_summary(cellIndex = cellIndex,
                                                          direction = direction,
                                                          fft_ccf = fft_ccf,
                                                          summaryVar = fft_ccf,
                                                          summaryFun = sd,
                                                          imputeVal = NA),
                     pairwiseCompCorMean = feature_registration_summary(cellIndex = cellIndex,
                                                                        direction = direction,
                                                                        fft_ccf = fft_ccf,
                                                                        summaryVar = pairwiseCompCor,
                                                                        summaryFun = mean,
                                                                        imputeVal = NA),
                     pairwiseCompCorSD = feature_registration_summary(cellIndex = cellIndex,
                                                                      direction = direction,
                                                                      fft_ccf = fft_ccf,
                                                                      summaryVar = pairwiseCompCor,
                                                                      summaryFun = sd,
                                                                      imputeVal = NA),
                     xTransSD = feature_registration_summary(cellIndex = cellIndex,
                                                             direction = direction,
                                                             fft_ccf = fft_ccf,
                                                             summaryVar = x,
                                                             summaryFun = sd,
                                                             imputeVal = NA),
                     yTransSD = feature_registration_summary(cellIndex = cellIndex,
                                                             direction = direction,
                                                             fft_ccf = fft_ccf,
                                                             summaryVar = y,
                                                             summaryFun = sd,
                                                             imputeVal = NA),
                     thetaRotSD = feature_registration_summary(cellIndex = cellIndex,
                                                               direction = direction,
                                                               fft_ccf = fft_ccf,
                                                               summaryVar = theta,
                                                               summaryFun = sd,
                                                               imputeVal = NA))

}

#' @rdname registrationFeatures
#' @export
feature_registration_summary <- function(cellIndex,direction,fft_ccf,summaryVar,
                                         summaryFun = mean,imputeVal = NA){

  ret <- data.frame(cellIndex = cellIndex,
                    direction = direction,
                    fft_ccf = fft_ccf,
                    summaryVar = summaryVar) %>%
    dplyr::group_by(cellIndex,direction) %>%
    dplyr::filter(fft_ccf == max(fft_ccf)) %>%
    dplyr::pull(summaryVar) %>%
    summaryFun()

  if(!is.numeric(ret) | is.na(ret)){
    return(imputeVal)
  }
  return(ret)

}

