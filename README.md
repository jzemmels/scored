
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scored

<!-- badges: start -->
<!-- badges: end -->

An R package to compare cartridge cases and compute similarity scores
between cartridge cases based on their breech face impressions.

## Installation

You can install the development version of scored from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jzemmels/scored")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(scored)

library(cmcR)
library(tidyverse)

data("K013sA1")
data("K013sA2")
```

``` r
comparisonData <- comparison_cellBased(reference = K013sA1,target = K013sA2,
                                       thetas = seq(-30,30,by = 3),
                                       returnX3Ps = FALSE)

comparisonData
#> # A tibble: 1,638 x 10
#>    cellIndex     x     y fft_ccf pairwis~1 theta refMi~2 targM~3 joint~4 direc~5
#>    <chr>     <dbl> <dbl>   <dbl>     <dbl> <dbl>   <dbl>   <dbl>   <dbl> <chr>  
#>  1 1, 2         24   -26   0.136     0.506   -30    2454   18481    2275 refere~
#>  2 1, 3         72   -37   0.148     0.388   -30    1150   16152    1109 refere~
#>  3 1, 4        -58     6   0.116     0.204   -30    1103   16846     531 refere~
#>  4 1, 5        -69   -25   0.209     0.431   -30    1450   19207    1351 refere~
#>  5 1, 6        -25   -13   0.294     0.519   -30    1751   21076    1336 refere~
#>  6 1, 7         33    15   0.234     0.483   -30    2401   22759    2101 refere~
#>  7 2, 1        -25   -49   0.161     0.550   -30    2498   17790      99 refere~
#>  8 2, 2          6   -63   0.134     0.176   -30     378   13973      54 refere~
#>  9 2, 3         40     8   0.174     0.602   -30    1912   13375       0 refere~
#> 10 2, 7         49    28   0.229     0.536   -30    2086   22466    1624 refere~
#> # ... with 1,628 more rows, and abbreviated variable names 1: pairwiseCompCor,
#> #   2: refMissingCount, 3: targMissingCount, 4: jointlyMissing, 5: direction
#> # i Use `print(n = ...)` to see more rows
```

``` r
comparisonData %>%
  feature_densityBasedAll(eps = 5,minPts = 5)
#> # A tibble: 1 x 3
#>   thetaDiff translationDiff clusterSize
#>       <dbl>           <dbl>       <dbl>
#> 1         0            1.16          11
```
