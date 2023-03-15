
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scored <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

An R package to compare cartridge cases and compute similarity scores
based on their breech face impressions.

## Installation

You can install the development version of scored from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jzemmels/scored")
```

## Feature Calculation

As an example, consider two cartridge cases fired from the same Ruger
SR9 semiautomatic 9-mm handgun. Learn more about the collection of these
cartridge cases [here](https://www.ojp.gov/pdffiles1/nij/249874.pdf).
The cartridge cases are uniquely identified as “K013sA1” and “K013sA2.”
We assume that the markings on these cartridge cases left by the handgun
during the firing process are similar.

``` r
library(scored)

library(cmcR)
library(impressions)
library(tidyverse)

data("K013sA1","K013sA2")
```

Below is a visual of the two cartridge case scans. Note that these scans
have already undergone some preprocessing to emphasize the breech face
impression markings. The similarity between these cartridge cases is not
immediately apparent. We can calculate similarity features between these
two scans using functions available in the `scored` package.

``` r
x3pPlot(K013sA1,K013sA2)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Cell-Based Features

``` r
comparisonData <- comparison_cellBased(reference = K013sA1,target = K013sA2,
                                       numCells = c(8,8),
                                       thetas = seq(-30,30,by = 3),
                                       direction = "both",
                                       returnX3Ps = TRUE)

comparisonData %>%
  group_by(direction) %>%
  feature_aLaCarte(features = "all",eps = 5,minPts = 5)
#> Parameter 'threshold' not specified. Defaulting to threshold = 1.
#>             direction   ccfMean      ccfSD pairwiseCompCorAve pairwiseCompCorSD
#> 1 reference_vs_target 0.3015478 0.08546018          0.5411682         0.1543828
#> 2 target_vs_reference 0.2488325 0.07746423          0.5513807         0.1503855
#>   xTransSD yTransSD thetaRotSD thetaDiff translationDiff clusterSize clusterInd
#> 1 39.20518  37.7141   19.54130         0        1.159502          11       TRUE
#> 2 31.77217  26.2262   14.59369         0        1.159502          11       TRUE
#>   neighborhoodSizeAve_ave neighborhoodSizeAve_sd neighborhoodSizeSD_ave
#> 1                54.71608               22.58417               135.9947
#> 2                54.36036               20.13450               138.9213
#>   neighborhoodSizeSD_sd differenceCor_ave differenceCor_sd filteredRatio_ave
#> 1              41.95552         0.4287395        0.1986363          2.126379
#> 2              35.80590         0.3636990        0.2136124          2.218079
#>   filteredRatio_sd
#> 1         2.013289
#> 2         1.405319
```

## Full-Scan Features

``` r
comparisonDat_fullScan_estimRotation <- comparison_fullScan(K013sA1,K013sA2,
                                                            returnX3Ps = TRUE,
                                                            thetas = -3)

comparisonDat_fullScan_estimRotation %>%
  group_by(direction) %>%
  feature_aLaCarte(features = "all",eps = 5,minPts = 5)
#> Only one cell found. Skipping the density-based feature calculation.
#> Parameter 'threshold' not specified. Defaulting to threshold = 1.
#>             direction    ccfMean ccfSD pairwiseCompCorAve pairwiseCompCorSD
#> 1 reference_vs_target 0.09550923    NA          0.1401430                NA
#> 2 target_vs_reference 0.27187163    NA          0.4096494                NA
#>   xTransSD yTransSD thetaRotSD neighborhoodSizeAve_ave neighborhoodSizeAve_sd
#> 1       NA       NA         NA                74.39877                     NA
#> 2       NA       NA         NA                97.35443                     NA
#>   neighborhoodSizeSD_ave neighborhoodSizeSD_sd differenceCor_ave
#> 1               883.0723                    NA       -0.03441979
#> 2               822.4063                    NA        0.08575254
#>   differenceCor_sd filteredRatio_ave filteredRatio_sd
#> 1               NA          2.534994               NA
#> 2               NA          4.858481               NA
```
