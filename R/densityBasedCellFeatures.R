#' Calculate clusters using density-based clustering methods
#' @name densityBasedClusters
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

#' Estimate aligning rotation based on high-density translation values
#' @name estimatedRotation
#' @export
estimatedRotation <- function(x,y,theta){

  data.frame(x=x,y=y,theta=theta) %>%
    dplyr::group_by(theta) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(dat){

      densEstim <- MASS::kde2d(x = dat$x,y = dat$y)

      data.frame(highestDens = max(densEstim$z),
                 theta = unique(dat$theta))

    }) %>%
    dplyr::filter(highestDens == max(highestDens)) %>%
    dplyr::pull(theta) %>%
    mean()

}

#' Compute the difference in density-estimated rotations between two comparison
#' directions
#' @name feature_densityBasedThetaDiff
#' @export

feature_densityBasedThetaDiff <- function(estimatedThetas,direction,imputeVal = 180){

  theta1 <- unique(estimatedThetas[direction == "reference vs. target"])
  theta2 <- unique(estimatedThetas[direction == "target vs. reference"])

  if(any(!is.numeric(c(theta1,theta2))) | any(is.na(c(theta1,theta2)))){
    return(imputeVal)
  }

  return(abs(theta1 + theta2))

}

#' Compute the difference in density-estimated translations between two
#' comparison directions
#' @name feature_densityBasedTranslationDiff
#' @export
feature_densityBasedTranslationDiff <- function(x,y,cluster,direction,imputeVal = 1000){

  refVTargTrans <- data.frame(x=x,y=y,cluster=cluster,direction=direction) %>%
    filter(cluster > 0 & direction == "reference vs. target") %>%
    summarise(estimTrans_x = mean(x),
              estimTrans_y = mean(y))

  targVRefTrans <- data.frame(x=x,y=y,cluster=cluster,direction=direction) %>%
    filter(cluster > 0 & direction == "target vs. reference") %>%
    summarise(estimTrans_x = mean(x),
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

#' Compute the average density-based cluster sizes between two comparison
#' directions
#' @name feature_densityBasedClusterSize
#' @export
feature_densityBasedClusterSize <- function(cluster,direction,imputeVal = 0){

  clusterSizes <- data.frame(direction = direction,cluster = cluster) %>%
    filter(cluster > 0) %>%
    group_by(direction) %>%
    tally()

  if(nrow(clusterSizes) != 2){

    return(imputeVal)

  }

  return(mean(clusterSizes$n))

}

#' Compute all three density-based features
#' @name feature_densityBasedAll
#' @export

feature_densityBasedAll <- function(comparisonData_cellBased,eps,minPts,id_cols = NULL){

  comparisonData %>%
    group_by(direction) %>%
    mutate(estimTheta = estimatedRotation(x=x,
                                          y=y,
                                          theta=theta)) %>%
    filter(theta == estimTheta) %>%
    mutate(cluster = densityBasedClusters(x=x,
                                          y=y,
                                          eps=eps,
                                          minPts=minPts)) %>%
    ungroup() %>%
    mutate(thetaDiff = feature_densityBasedThetaDiff(estimatedThetas = theta,
                                                     direction = direction),
           translationDiff = feature_densityBasedTranslationDiff(x=x,
                                                                 y=y,
                                                                 cluster=cluster,
                                                                 direction=direction),
           clusterSize = feature_densityBasedClusterSize(cluster=cluster,
                                                         direction=direction)) %>%
    select(id_cols,thetaDiff,translationDiff,clusterSize) %>%
    distinct()

}
