#' Calculate a summary statistic of a variable at the estimated registration
#' @name feature_registration_summary
#'
#' @param cellIndex tibble column containing cell indices
#' @param direction tibble column indicating whether the associated row came
#'   from the "reference vs. target" or "target vs. reference" comparison
#' @param fft_ccf tibble column containing cross-correlation function values
#' @param summaryVar tibble column that is to be summarized
#' @param summaryFun function that will be used to summarize the values in the
#'   `summaryVar` column
#' @param imputeVal value to return in case the summary function results in a
#'   non-numeric (e.g., NA, NULL) value
#'
#' @return A numeric value resulting from passing to `summaryFun` values in the
#'   `summaryVar` column (at which the CCF is maximized per cell index &
#'   direction)
#' @export

feature_registration_summary <- function(cellIndex,direction,fft_ccf,summaryVar,
                                         summaryFun = mean,imputeVal = -1){

  ret <- data.frame(cellIndex = cellIndex,
                    direction = direction,
                    fft_ccf = fft_ccf,
                    summaryVar = summaryVar) %>%
    group_by(cellIndex,direction) %>%
    filter(fft_ccf == max(fft_ccf)) %>%
    pull(summaryVar) %>%
    summaryFun()

  if(!is.numeric(ret) | is.na(ret)){
    return(imputeVal)
  }
  return(ret)

}

#' Calculate all seven registration-based features
#' @name feature_registration_all
#'
#' @param comparisonData a data frame resulting from a comparison of two
#'   cartridge cases (for example, output of the `comparison_cellBased` or
#'   `comparison_fullScan` functions)
#' @param id_cols variable(s) to group by prior to calculating the summary
#'   statistics
#'
#' @return A data frame containing the seven registration-based features: 1.
#'   Mean CCF value 2. Standard deviation (S.D.) of the CCF values 3. Mean
#'   pairwise-complete correlation 4. S.D. of the pairwise-complete correlation
#'   values 5. S.D. of the horizontal translation values 6. S.D. of the vertical
#'   translation values 7. S.D. of the rotation values
#'
#' @note This function can be used on comparison data from a full-scan or
#'   cell-based comparison. For a full-scan comparison, we recommend using only
#'   the average CCF and average pairwise-complete correlation
#'
#' @export

feature_registration_all <- function(comparisonData,id_cols = NULL){

  if(!is.null(id_cols)){

    comparisonData <- comparisonData %>%
      group_by(!!as.name(id_cols))

  }

  comparisonData  %>%
    summarise(ccfMean = feature_registration_summary(cellIndex = cellIndex,
                                                     direction = direction,
                                                     fft_ccf = fft_ccf,
                                                     summaryVar = fft_ccf,
                                                     summaryFun = mean,
                                                     imputeVal = -1),
              ccfSD = feature_registration_summary(cellIndex = cellIndex,
                                                   direction = direction,
                                                   fft_ccf = fft_ccf,
                                                   summaryVar = fft_ccf,
                                                   summaryFun = sd,
                                                   imputeVal = 1000),
              pairwiseCompCorMean = feature_registration_summary(cellIndex = cellIndex,
                                                                 direction = direction,
                                                                 fft_ccf = fft_ccf,
                                                                 summaryVar = pairwiseCompCor,
                                                                 summaryFun = mean,
                                                                 imputeVal = -1),
              pairwiseCompCorSD = feature_registration_summary(cellIndex = cellIndex,
                                                               direction = direction,
                                                               fft_ccf = fft_ccf,
                                                               summaryVar = pairwiseCompCor,
                                                               summaryFun = sd,
                                                               imputeVal = 1000),
              xTransSD = feature_registration_summary(cellIndex = cellIndex,
                                                      direction = direction,
                                                      fft_ccf = fft_ccf,
                                                      summaryVar = x,
                                                      summaryFun = sd,
                                                      imputeVal = 1000),
              yTransSD = feature_registration_summary(cellIndex = cellIndex,
                                                      direction = direction,
                                                      fft_ccf = fft_ccf,
                                                      summaryVar = y,
                                                      summaryFun = sd,
                                                      imputeVal = 1000),
              thetaRotSD = feature_registration_summary(cellIndex = cellIndex,
                                                        direction = direction,
                                                        fft_ccf = fft_ccf,
                                                        summaryVar = theta,
                                                        summaryFun = sd,
                                                        imputeVal = 1000))

}
