#' Calculate similarity features based on the output of the comparison procedure
#'
#' @name feature_aLaCarte
#'
#'@param comparisonData tibble such as one returned by the
#'  `comparison_cellBased()` or `comparison_fullScan()` functions that contains
#'  results from the cell-based or full scan comparison procedure
#'@param features character vector containing the features to be calculated.
#'@param id_cols variable(s) to group by prior to calculating the summary
#'  statistics
#'@param quiet Boolean to suppress function messages
#'@param ... additional parameters for the feature calculation functions
#'
#' @export

feature_aLaCarte <- function(comparisonData,features = "all",id_cols = NULL,quiet = FALSE,...){

  optionalParams <- list(...)

  aLaCarteFeatures <- data.frame(dummyCol = NA)

  if("registration" %in% features | features == "all"){

    if(any(c("cellHeightValues","alignedTargetCell") %in% names(comparisonData))){
      compData <- comparisonData %>%
        select(-c(cellHeightValues,alignedTargetCell))
    }

    registrationFeatures <- feature_registration_all(comparisonData = compData,
                                                     id_cols = id_cols)

    aLaCarteFeatures <- bind_cols(aLaCarteFeatures,registrationFeatures)

  }

  if("density" %in% features | features == "all"){

    if(any(c("cellHeightValues","alignedTargetCell") %in% names(comparisonData))){
      compData <- comparisonData %>%
        select(-c(cellHeightValues,alignedTargetCell))
    }

    # default to certain parameter values if not otherwise specified
    if(is.null(optionalParams$eps)){

      if(!quiet){message("Parameter 'eps' not specified. Defaulting to eps = 5.")}

      optionalParams$eps <- 5

    }
    if(is.null(optionalParams$minPts)){

      if(!quiet){message("Parameter 'minPts' not specified. Defaulting to minPts = 5.")}

      optionalParams$minPts <- 5

    }

    if(length(unique(compData$cellIndex)) > 1){

      densityFeatures <- feature_densityBased_all(comparisonData_cellBased = compData,
                                                  eps = optionalParams$eps,
                                                  minPts = optionalParams$minPts,
                                                  id_cols = id_cols)

      aLaCarteFeatures <- bind_cols(aLaCarteFeatures,
                                    densityFeatures)

    }
    else{

      if(!quiet){message("Only one cell found. Skipping the density-based feature calculation.")}

    }

  }

  if("diagnostic" %in% features | features == "all"){

    stopifnot("cellHeightValues" %in% names(comparisonData) & "alignedTargetCell" %in% names(comparisonData))

    diagnosticFeatures <- feature_visualDiagnostic_all(comparisonData = comparisonData %>%
                                                         group_by(direction,cellIndex) %>%
                                                         filter(fft_ccf == max(fft_ccf)) %>%
                                                         group_by(direction),
                                                       id_cols = id_cols)

    aLaCarteFeatures <- bind_cols(aLaCarteFeatures %>% select(-direction),
                                  diagnosticFeatures)

  }

  return(aLaCarteFeatures %>%
           select(-dummyCol) %>%
           select(c(direction,everything())))

}
