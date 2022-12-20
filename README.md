
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
                                       returnX3Ps = TRUE)

# debugonce(feature_visualDiagnostic_neighborhoodSizeSummary)

comparisonData %>%
  group_by(direction) %>%
  feature_aLaCarte(features = "all",eps = 5,minPts = 5)
#>              direction   ccfMean      ccfSD pairwiseCompCorMean
#> 1 reference vs. target 0.3015478 0.08546018           0.5411682
#> 2 target vs. reference 0.2488325 0.07746423           0.5513807
#>   pairwiseCompCorSD xTransSD yTransSD thetaRotSD thetaDiff translationDiff
#> 1         0.1543828 39.20518  37.7141   19.54130         0        1.159502
#> 2         0.1503855 31.77217  26.2262   14.59369         0        1.159502
#>   clusterSize neighborhoodSizeAve_ave neighborhoodSizeAve_sd
#> 1          11                62.12405               35.13812
#> 2          11                49.99204               22.39620
#>   neighborhoodSizeSD_ave neighborhoodSizeSD_sd differenceCor_ave
#> 1               126.3500              37.51384         0.4159597
#> 2               128.0725              39.25245         0.3394139
#>   differenceCor_sd filteredRatio_ave filteredRatio_sd
#> 1        0.2150909          3.210994         2.560793
#> 2        0.2187713          2.964965         2.040393
```

## Full-Scan Features

``` r
comparisonDat_fullScan_estimRotation <- comparison_fullScan(K013sA1,K013sA2,
                                                            returnX3Ps = TRUE,
                                                            thetas = -3)

comparisonDat_fullScan_estimRotation %>%
  group_by(direction) %>%
  feature_aLaCarte(features = "all",eps = 5,minPts = 5) %>%
  select(direction,ccfMean,pairwiseCompCorMean,
         neighborhoodSizeAve_ave,neighborhoodSizeSD_ave,differenceCor_ave)
#> Only one cell found. Skipping the density-based feature calculation.
#>              direction    ccfMean pairwiseCompCorMean neighborhoodSizeAve_ave
#> 1 reference vs. target 0.09550923           0.1401430                74.79141
#> 2 target vs. reference 0.27187163           0.4096494                96.41975
#>   neighborhoodSizeSD_ave differenceCor_ave
#> 1               887.4545       -0.03427995
#> 2               824.3021        0.08756745
```
