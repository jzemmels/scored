#'Estimate the registration between two cartridge case scans using a cell-based
#'or full scan comparison procedure
#'
#'The `comparison_cellBased` function uses a cell-based comparison procedure
#'implemented in the `cmcR::comparison_allTogether()`. The `comparison_fullScan`
#'estimates the registration by comparing the full scans.
#'
#'@param reference an x3p object
#'@param target another x3p object
#'@param thetas a numeric vector of arbitrary length containing rotation values
#'  to be considered in the registration procedure
#'@param numCells a numeric vector of length 2 containing the number of cells to
#'  divide the c(rows,cols) of a cartridge case surface matrix into.
#'@param returnX3Ps A Boolean to return the cellHeightValues and
#'  alignedTargetCells for each cell index. Note that setting this argument to
#'  TRUE is required to calculate the visual diagnostic features
#'@param maxMissingProp maximum proportion of missing values allowed for each
#'  cell/region.
#'@param sideLengthMultiplier ratio between the target region and reference cell
#'  side lengths. For example, sideLengthMultiplier = 3 implies each region will
#'  be 9 times larger than its paired reference cell.
#'
#'@note The distinction between the "reference" and "target" scans is arbitrary,
#'  yet necessary for keeping track of the comparison direction in which a
#'  particular observation was calculated.
#'
#'@seealso `cmcR::comparison_allTogether()`
#'
#' @examples
#' data("K013sA1","K013sA2")
#'
#' compData_cellBased <- comparison_cellBased(reference = K013sA1,
#'                                            target = K013sA2,
#'                                            thetas = c(-3,0,3))
#'
#'compData_cellBased
#'
#' compData_fullScan <- comparison_fullScan(reference = K013sA1,
#'                                          target = K013sA2,
#'                                          thetas = c(-3,0,3))
#'
#'compData_fullScan
#'
#'@rdname comparisonHelpers
#'@export
comparison_cellBased <- function(reference,target,
                                 direction = "one",
                                 thetas = seq(-30,30,by = 3),
                                 numCells = c(8,8),
                                 maxMissingProp = .99,sideLengthMultiplier = 3,
                                 returnX3Ps = TRUE){

  # you may want to compare the scans using a different theta grid in the two
  # directions
  stopifnot(direction %in% c("one","both"))

  if(direction == "both"){

    if(is.list(thetas) & length(thetas) == 2){
      thetas_refToTarget <- thetas[[1]]
      thetas_targetToRef <- thetas[[2]]
    }
    else{
      thetas_refToTarget <- thetas
      thetas_targetToRef <- thetas
    }

    stopifnot("When direction = 'both,' thetas should be a single numeric vector or a list of two numeric vectors for each direction" = is.vector(thetas_refToTarget) & is.vector(thetas_targetToRef))


    ret <- dplyr::bind_rows(purrr::map_dfr(thetas_refToTarget,
                                           ~ {

                                             cmcR::comparison_allTogether(reference,target,theta = .,
                                                                          numCells = numCells,
                                                                          returnX3Ps = returnX3Ps,
                                                                          sideLengthMultiplier = sideLengthMultiplier,
                                                                          maxMissingProp = maxMissingProp)

                                           }) %>%
                              dplyr::mutate(direction = "reference_vs_target"),
                            purrr::map_dfr(thetas_targetToRef,
                                           ~ {

                                             cmcR::comparison_allTogether(target,reference,theta = .,
                                                                          numCells = numCells,
                                                                          returnX3Ps = returnX3Ps,
                                                                          sideLengthMultiplier = sideLengthMultiplier,
                                                                          maxMissingProp = maxMissingProp)

                                           }) %>%
                              dplyr::mutate(direction = "target_vs_reference"))
  }
  else{

    stopifnot("When direction = 'one,' thetas should be a single numeric vector" = is.vector(thetas))

    ret <- purrr::map_dfr(thetas,
                          ~ {

                            cmcR::comparison_allTogether(reference,target,theta = .,
                                                         numCells = numCells,
                                                         returnX3Ps = returnX3Ps,
                                                         sideLengthMultiplier = sideLengthMultiplier,
                                                         maxMissingProp = maxMissingProp)

                          })


  }

  return(ret)

}

#'@rdname comparisonHelpers
#' @export
comparison_fullScan <- function(reference,target,thetas = seq(-30,30,by = 3),returnX3Ps = TRUE){

  dplyr::bind_rows(purrr::map_dfr(thetas,
                                  ~ {

                                    cmcR::comparison_allTogether(reference,target,theta = .,
                                                                 numCells = c(1,1),returnX3Ps = returnX3Ps,
                                                                 sideLengthMultiplier = 1.1,
                                                                 maxMissingProp = .99)

                                  }) %>%
                     dplyr::mutate(direction = "reference_vs_target"),
                   purrr::map_dfr(thetas,
                                  ~ {

                                    cmcR::comparison_allTogether(target,reference,theta = .,
                                                                 numCells = c(1,1),returnX3Ps = returnX3Ps,
                                                                 sideLengthMultiplier = 1.1,
                                                                 maxMissingProp = .99)

                                  }) %>%
                     dplyr::mutate(direction = "target_vs_reference"))

}
