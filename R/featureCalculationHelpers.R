#'Calculate similarity features based on the output of the comparison procedure
#'
#'@name feature_aLaCarte
#'
#'@param comparisonData tibble such as one returned by the
#'  `comparison_cellBased()` or `comparison_fullScan()` functions that contains
#'  results from the cell-based or full scan comparison procedure
#'@param features character vector containing the features to be calculated.
#'@param id_cols variable(s) to group by prior to calculating the summary
#'  statistics
#'@param quiet Boolean to suppress function messages
#'@param ... additional parameters for the feature calculation functions. See
#'  notes for possible additional parameters.
#'
#'@note Each additional parameter should be passed as a single argument.
#'  Possible parameters are "eps" and "minPts" used in the density-based feature
#'  calculation and "threshold" used in the visual diagnostic feature
#'  calculation. See the documentation for feature_densityBased_all or
#'  feature_visualDiagnostic_all to learn more about these parameters.
#'
#'@export

feature_aLaCarte <- function(comparisonData,features = "all",id_cols = NULL,quiet = FALSE,...){

  optionalParams <- list(...)

  aLaCarteFeatures <- data.frame(direction = c("reference_vs_target","target_vs_reference"))

  if("registration" %in% features | all(features == "all")){

    # these two columns contain x3ps and really slow down computation of
    # features. They aren't needed for this step
    if(any(c("cellHeightValues","alignedTargetCell") %in% names(comparisonData))){
      compData <- comparisonData %>%
        dplyr::select(-c(cellHeightValues,alignedTargetCell))
    }
    else{
      compData <- comparisonData
    }

    registrationFeatures <- feature_registration_all(comparisonData = compData,
                                                     id_cols = id_cols)

    aLaCarteFeatures <- bind_cols(aLaCarteFeatures,
                                  registrationFeatures %>%
                                    dplyr::select(-direction))

  }

  # these two columns contain x3ps and really slow down computation of
  # features. They aren't needed for this step
  if("density" %in% features | all(features == "all")){

    if(any(c("cellHeightValues","alignedTargetCell") %in% names(comparisonData))){
      compData <- comparisonData %>%
        dplyr::select(-c(cellHeightValues,alignedTargetCell))
    }
    else{
      compData <- comparisonData
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

  if("visual" %in% features | all(features == "all")){

    stopifnot("cellHeightValues" %in% names(comparisonData) & "alignedTargetCell" %in% names(comparisonData))

    if(is.null(optionalParams$threshold)){

      if(!quiet){message("Parameter 'threshold' not specified. Defaulting to threshold = 1.")}

      optionalParams$threshold <- 1

    }
    stopifnot(is.function(optionalParams$threshold) | is.numeric(optionalParams$threshold))

    diagnosticFeatures <- feature_visualDiagnostic_all(comparisonData = comparisonData %>%
                                                         group_by(direction,cellIndex) %>%
                                                         filter(fft_ccf == max(fft_ccf)) %>%
                                                         group_by(direction),
                                                       threshold = optionalParams$threshold,
                                                       id_cols = id_cols)

    aLaCarteFeatures <- bind_cols(aLaCarteFeatures,
                                  diagnosticFeatures %>%
                                    dplyr::select(-direction))

  }

  return(aLaCarteFeatures %>%
           dplyr::select(c(direction,everything())))

}
