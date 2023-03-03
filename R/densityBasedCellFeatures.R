#'Calculate features using density-based clustering algorithms
#'
#'The `feature_densityBased_all` function calculates all of the density-based
#'features in one call. Calculate each feature individually using the
#'`feature_densityBased_thetaDiff`, `feature_densityBased_translationDiff`, and
#'`feature_densityBased_clusterSize` functions. The `densityBasedClusters` and
#'`estimatedRotation` functions are exported helpers.
#'
#'@param comparisonData_cellBased tibble such as one returned by the
#'  `comparison_cellBased()` function that contains results from the cell-based
#'  comparison procedure
#'@param x tibble column containing estimated horizontal translations
#'@param y tibble column containing estimated vertical translations
#'@param theta tibble column containing estimated rotations
#'@param direction tibble column containing characters "reference_vs_target" or
#'  "target_vs_reference" that specify the comparison direction associated with
#'  the observation
#'@param imputeVal value to return if the feature calculation results in a
#'  non-numeric (i.e., NA, NULL) value
#'@param eps a double representing the neighborhood radius used in the DBSCAN
#'  algorithm
#'@param minPts an integer representing the minimum neighborhood size used in
#'  the DBSCAN algorithm
#'@param method a character vector that specifies the algorithm used to find
#'  clusters. Must be either "dbscan" or "hdbscan" or "optics" - see the dbscan
#'  package for more information.
#'@param id_cols column names in the comparisonData tibble that uniquely
#'  identify each observation. These are returned along with the computed
#'  features
#'
#'@note The eps and minPts arguments are used differently depending on the
#'  method specified. The eps argument is passed to the eps parameter in the
#'  `dbscan::dbscan()` function if method == "dbscan" and to the eps_cl
#'  parameter in the `dbscan::extractDBSCAN()` function if method == "optics"
#'
#' @examples
#' data("K013sA1","K013sA2")
#'
#' compData <- comparison_cellBased(reference = K013sA1,
#'                                  target = K013sA2,
#'                                  thetas = c(-3,0,3))
#'
#' # calculate all density-based features in one call
#' feature_densityBased_all(compData,eps = 5,minPts = 5)
#'
#' # verify that we can calculate the same features using the individual
#' #   feature_densityBased_* functions
#' compData %>%
#'   dplyr::group_by(direction) %>%
#'   dplyr::filter(theta == estimatedRotation(x = x,y = y,theta = theta)) %>%
#'   dplyr::mutate(clust = densityBasedClusters(x = x,y = y,eps = 5,minPts = 5)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::summarise(thetaDiff = feature_densityBased_thetaDiff(estimatedThetas = theta,direction = direction),
#'                    translationDiff = feature_densityBased_translationDiff(x = x,y = y,cluster = clust,direction = direction),
#'                    clusterSize = feature_densityBased_clusterSize(cluster = clust,direction = direction))
#'
#'@seealso `dbscan::dbscan()`, `dbscan::hdbscan()`, `dbscan::optics()` and
#'  `dbscan::extractDBSCAN()`
#'
#'@rdname densityBasedFeatures
#'@export
feature_densityBased_all <- function(comparisonData_cellBased,eps,minPts,method = "dbscan",id_cols = NULL){

  comparisonData_cellBased %>%
    dplyr::group_by(direction) %>%
    dplyr::mutate(estimTheta = estimatedRotation(x=x,
                                                 y=y,
                                                 theta=theta)) %>%
    dplyr::filter(theta == estimTheta) %>%
    dplyr::mutate(cluster = densityBasedClusters(x=x,
                                                 y=y,
                                                 eps=eps,
                                                 minPts=minPts,
                                                 method = method)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(thetaDiff = feature_densityBased_thetaDiff(estimatedThetas = theta,
                                                             direction = direction),
                  translationDiff = feature_densityBased_translationDiff(x=x,
                                                                         y=y,
                                                                         cluster=cluster,
                                                                         direction=direction),
                  clusterSize = feature_densityBased_clusterSize(cluster=cluster,
                                                                 direction=direction),
                  clusterInd = !is.na(clusterSize)) %>%
    dplyr::select(id_cols,thetaDiff,translationDiff,clusterSize,clusterInd) %>%
    dplyr::distinct()

}

#' @rdname densityBasedFeatures
#' @export

feature_densityBased_thetaDiff <- function(estimatedThetas,direction,imputeVal = NA){

  theta1 <- unique(estimatedThetas[direction == "reference_vs_target"])
  theta2 <- unique(estimatedThetas[direction == "target_vs_reference"])

  if(any(!is.numeric(c(theta1,theta2))) | any(is.na(c(theta1,theta2)))){
    return(imputeVal)
  }

  return(abs(theta1 + theta2))

}

#' @rdname densityBasedFeatures
#' @export
feature_densityBased_translationDiff <- function(x,y,cluster,direction,imputeVal = NA){

  refVTargTrans <- data.frame(x=x,y=y,cluster=cluster,direction=direction) %>%
    dplyr::filter(cluster > 0 & direction == "reference_vs_target") %>%
    dplyr::summarise(estimTrans_x = mean(x),
                     estimTrans_y = mean(y))

  targVRefTrans <- data.frame(x=x,y=y,cluster=cluster,direction=direction) %>%
    dplyr::filter(cluster > 0 & direction == "target_vs_reference") %>%
    dplyr::summarise(estimTrans_x = mean(x),
                     estimTrans_y = mean(y))

  if(nrow(refVTargTrans) != 1 | nrow(targVRefTrans) != 1){
    return(imputeVal)
  }

  translationDiff <- sqrt((refVTargTrans$estimTrans_x + targVRefTrans$estimTrans_x)^2 +
                            (refVTargTrans$estimTrans_y + targVRefTrans$estimTrans_y)^2)

  if(!is.numeric(translationDiff) | is.na(translationDiff)){
    return(imputeVal)
  }

  return(translationDiff)

}

#' @rdname densityBasedFeatures
#' @export
feature_densityBased_clusterSize <- function(cluster,direction,imputeVal = NA){

  clusterSizes <- data.frame(direction = direction,cluster = cluster) %>%
    dplyr::filter(cluster > 0) %>%
    dplyr::group_by(direction) %>%
    dplyr::tally()

  if(nrow(clusterSizes) != 2){

    return(imputeVal)

  }

  return(mean(clusterSizes$n))

}


#' @rdname densityBasedFeatures
#' @export
densityBasedClusters <- function(x,y,eps,minPts,method = "dbscan"){

  stopifnot(method %in% c("dbscan","hdbscan","optics"))

  if(method == "dbscan"){
    clusterAssignments <- dbscan::dbscan(x = data.frame(x = x,y = y),
                                         eps = eps,minPts = minPts)
  }
  if(method == "hdbscan"){
    clusterAssignments <- dbscan::hdbscan(x = data.frame(x = x,y = y),minPts = minPts)
  }
  if(method == "optics"){
    opticsOrdering <- dbscan::optics(x = data.frame(x = x,y = y),minPts = minPts)
    clusterAssignments <- dbscan::extractDBSCAN(opticsOrdering,eps_cl = eps)
  }

  return(clusterAssignments$cluster)

}

#' @rdname densityBasedFeatures
#' @export
estimatedRotation <- function(x,y,theta){

  data.frame(x=x,y=y,theta=theta) %>%
    dplyr::group_by(theta) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(dat){

      # kde2d throws an error if there aren't 2+ points
      if(nrow(dat) > 1){

        if(diff(quantile(dat$x, c(0.25, 0.75))) == 0){
          closestToThirdQuartile <- dat$x[which.min(abs(dat$x - quantile(dat$x,.75)))]

          dat$x[dat$x == closestToThirdQuartile] <-
            dat$x[dat$x == closestToThirdQuartile] + rnorm(n = length(dat$x[dat$x == closestToThirdQuartile]))
        }
        if(diff(quantile(dat$y, c(0.25, 0.75))) == 0){
          closestToThirdQuartile <- dat$y[which.min(abs(dat$y - quantile(dat$y,.75)))]

          dat$y[dat$y == closestToThirdQuartile] <-
            dat$y[dat$y == closestToThirdQuartile] + rnorm(n = length(dat$y[dat$y == closestToThirdQuartile]))
        }

        densEstim <- MASS::kde2d(x = dat$x,y = dat$y)

        return(data.frame(highestDens = max(densEstim$z),
                          theta = unique(dat$theta)))

      }
      else{
        return(data.frame(highestDens = 0,
                          theta = unique(dat$theta)))
      }

    }) %>%
    dplyr::filter(highestDens == max(highestDens)) %>%
    dplyr::pull(theta) %>%
    mean()

}
