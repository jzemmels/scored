#' Compare two cartridge case scans using the cell-based comparison procedure
#' @name comparison_cellBased
#' @export

comparison_cellBased <- function(reference,target,thetas = seq(-30,30,by = 3),
                                 numCells = c(8,8),returnX3Ps = TRUE,
                                 maxMissingProp = .99,sideLengthMultiplier = 3){

  bind_rows(map_dfr(thetas,
                    ~ {

                      cmcR::comparison_allTogether(reference,target,theta = .,
                                                   numCells = numCells,returnX3Ps = returnX3Ps,
                                                   sideLengthMultiplier = sideLengthMultiplier,
                                                   maxMissingProp = maxMissingProp)

                    }) %>%
              mutate(direction = "reference vs. target"),
            map_dfr(thetas,
                    ~ {

                      cmcR::comparison_allTogether(target,reference,theta = .,
                                                   numCells = numCells,returnX3Ps = returnX3Ps,
                                                   sideLengthMultiplier = sideLengthMultiplier,
                                                   maxMissingProp = maxMissingProp)

                    }) %>%
              mutate(direction = "target vs. reference"))

}

#' Compare two cartridge case scans by estimating the alignment of the full
#' scans
#' @name comparison_fullScan
#' @export
comparison_fullScan <- function(reference,target,thetas = seq(-30,30,by = 3)){

  bind_rows(map_dfr(thetas,
                    ~ {

                      cmcR::comparison_allTogether(reference,target,theta = .,
                                                   numCells = c(1,1),returnX3Ps = TRUE,
                                                   sideLengthMultiplier = 1.1,
                                                   maxMissingProp = .99)

                    }) %>%
              mutate(direction = "reference vs. target"),
            map_dfr(thetas,
                    ~ {

                      cmcR::comparison_allTogether(target,reference,theta = .,
                                                   numCells = c(1,1),returnX3Ps = TRUE,
                                                   sideLengthMultiplier = 1.1,
                                                   maxMissingProp = .99)

                    }) %>%
              mutate(direction = "target vs. reference"))

}
