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

                          }) %>%
      dplyr::mutate(direction = "reference_vs_target")


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

#' @rdname comparisonHelpers
#' @export
#'
#' @param ... either a sequence of x3p objects separated by commas like
#'   'x3p1,x3p2,x3p3,...', a single list of x3p objects separated by commas like
#'   'list(x3p1,x3p2,x3p3,...)', or a vector of x3p file paths like
#'   'c("path/to/file1.x3p", "path/to/file2.x3p", "path/to/file3.x3p")'
#' @param x3p_labels (optional) character vector of x3p labels
#' @param standardize_resolutions whether to force resolutions of all scans
#'   equal. If FALSE, then function will throw error when scans aren't same
#'   resolution.

comparison_aces <- function(...,x3p_labels = NULL,standardize_resolutions = FALSE){

  input <- list(...)

  # return a list of labeled x3p objects
  x3pList <- prepare_x3ps(input, x3p_labels = x3p_labels)

  # create a list of all pairwise comparisons (excluding replicates & self
  # comparisons)
  x3pComparisons <-
    tidyr::expand_grid(reference = names(x3pList),
                       target = names(x3pList)) %>%
    dplyr::left_join(data.frame(scanName = names(x3pList),
                                scanInd = 1:length(x3pList)),
                     by = c("reference" = "scanName")) %>%
    dplyr::rename(referenceInd = scanInd) %>%
    dplyr::left_join(data.frame(scanName = names(x3pList),
                                scanInd = 1:length(x3pList)),
                     by = c("target" = "scanName")) %>%
    dplyr::rename(targetInd = scanInd) %>%
    dplyr::filter(referenceInd < targetInd) %>%
    dplyr::select(-c(referenceInd,targetInd)) %>%
    dplyr::mutate(comparisonName = paste0(reference,"_vs_",target))

  comparison_fullScan <- x3pComparisons %>%
    purrr::pmap_dfr(function(...){
      aces_registration_fullScan(x3pList = x3pList,x3pComparison = tibble::tibble(...))
    })

  ##### seems to work up to here...
  # return(comparison_fullScan)

  # TODO:
  # - cell-based comparison based on full scan registration
  # - full scan and cell-based feature calculation
  # - return summarized feature values in a tibble (add metadata too?)
  # NEEDED FEATURES:
  # - should default to scaling all scans to the resolution used in dissertation, but allow users to turn off this setting/set their own resolution
  # - messages about function progress & "quiet" argument
  # - include option to compute similarity score using predict(), with caveat about equal resolutions
}

# function to put x3ps into a standardized format for use in other functions
prepare_x3ps <- function(x3pFiles,x3p_labels = NULL,standardize_resolutions = FALSE){

  # x3pFiles <- list(...)

  ####### Attempt to load x3p objects into a list

  # input type #1: pass an arbitrary number of x3p objects separated by commas
  # to ... (nothing needs to be done here)
  if(all(purrr::map_chr(x3pFiles,class) == "x3p")){
    x3pList <- x3pFiles
  }
  # input type #2: pass one list() of x3p objects to ...
  else if(length(x3pFiles) == 1 & all(purrr::map_chr(x3pFiles[[1]],class) == "x3p")){
    x3pList <- x3pFiles[[1]]
  }
  # input type #3: pass a vector of file paths
  else if(all(purrr::map_chr(x3pFiles[[1]],class) == "character")){

    stopifnot("Provide a vector of 2 or more file paths for valid comparison" = length(x3pFiles[[1]]) > 1)

    x3pList <- purrr::map(x3pFiles[[1]],
                          function(fileName){

                            assertthat::assert_that(file.exists(fileName))

                            return(x3ptools::read_x3p(fileName))

                          })

    # if no labels provided, guess the x3p labels using the file names
    if(is.null(x3p_labels)){
      x3p_labels <- stringr::str_remove(basename(x3pFiles[[1]]),"\\.x3p")
    }
  }
  else{
    stop("Incorrect input type. See ?comparison_aces for more information.")
  }

  # last resort: label each x3p as x3p1, x3p2, etc.
  if(is.null(x3p_labels)){
    x3p_labels <- paste0("x3p",seq_along(x3pList))
  }
  stopifnot("x3p_labels should be same length as the number of input x3ps." = length(x3p_labels) == length(x3pList))

  ####### Check that x3ps are ready for comparison

  # check that scan resolutions are all equal
  if(standardize_resolutions){
    # force the resolutions to be equal
    x3pList <- x3p_standardize_resolutions(x3pList)
  }
  else{
    # or throw error
    assertthat::assert_that(are_equal(purrr::map_dbl(x3pList,~ .$header.info$incrementY)) & are_equal(purrr::map_dbl(x3pList,~ .$header.info$incrementX)),
                            msg = "Scan resolutions are not equal. Either make sure resolutions are equal, or set 'standardize_resolutions = TRUE'.")
  }

  return(purrr::set_names(x3pList,x3p_labels))

}

# function that changes the resolutions of all scans to the same resolution.
# Default to lowest resolution == maximum unit per pixel value
x3p_standardize_resolutions <- function(x3pList,FUN = max){

  maxX <- FUN(purrr::map_dbl(x3pList,~ .$header.info$incrementX))
  maxY <- FUN(purrr::map_dbl(x3pList,~ .$header.info$incrementY))

  stopifnot("Resolution information missing in one or more x3ps." = is.double(maxX) & is.double(maxY))

  return(purrr::map(x3pList,~ x3ptools::interpolate_x3p(resx = maxX, rexy = maxY)))

}

# checks if elements of a vector are all equal by checking each pair of elements
# adapted from: https://stackoverflow.com/a/27331553/14000041
are_equal <- function(x) {
  # more than one object required
  if (length(x) < 2) stop("More than one object required")

  # matrix of object name pairs
  pairs <- t(combn(x, 2))

  # if only two objects, return all.equal() for them
  if (nrow(pairs) == 1) return(all.equal(pairs[1,1], pairs[1,2]))

  eq.fun <- function(z, y) {
    all.eq <- all.equal(z, y)
    name <- paste0(z, " vs. ", y)
    return(list(all.eq, name))
  }

  # list of eq.fun object comparisons
  out <- vector(mode="list", length=nrow(pairs))

  for (w in 1:nrow(pairs)) {
    eq.list <- eq.fun(pairs[w, 1], pairs[w, 2])
    out[[w]] <- eq.list[[1]]
    names(out)[w] <- eq.list[[2]]
  }

  return(isTRUE(all(as.logical(unlist(out)))))
}

# function to perform the full scan registration using comparison_fullScan
aces_registration_fullScan <- function(x3pList,x3pComparison){

  reference <- x3pList[[which(x3pComparison$reference == names(x3pList))[1]]]
  target <- x3pList[[which(x3pComparison$target == names(x3pList))[1]]]

  fullScanComparison <-
    scored::comparison_fullScan(reference,target) %>%
    dplyr::group_by(direction) %>%
    dplyr::filter(fft_ccf == max(fft_ccf)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(comparisonName = x3pComparison$comparisonName) %>%
    dplyr::arrange(direction) %>%
    # need to rescale the surface matrix values back to microns:
    dplyr::mutate(cellHeightValues = purrr::map(cellHeightValues,
                                                function(dat){

                                                  if(!all(is.na(dat))){
                                                    dat$surface.matrix <- dat$surface.matrix*dat$cmcR.info$scaleByVal # convert to micron scale
                                                  }

                                                  return(dat)
                                                }),
                  alignedTargetCell = purrr::map(alignedTargetCell,
                                                 function(dat){

                                                   if(!all(is.na(dat))){
                                                     dat$surface.matrix <- dat$surface.matrix*dat$cmcR.info$scaleByVal # convert to micron scale
                                                   }

                                                   return(dat)
                                                 }))

  return(fullScanComparison)

}
