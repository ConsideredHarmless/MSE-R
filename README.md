# Maximum Score Estimator

<!-- badges: start -->
<!-- badges: end -->

The **maxscoreest** R package solves the *pairwise maximum score estimation*
problem.

The code builds upon Jeremy Fox’s theoretical work on the “pairwise maximum
score estimator” (Fox 2010; Fox 2016) and the original Match Estimation toolkit
(Santiago and Fox, 2009) which can be downloaded from <http://fox.web.rice.edu/>.

To understand the present code the user needs to be familiar with the maximum
score estimator and formal matching games. To ease the exposition, this
documentation and the code itself follow closely the terminology used by Jeremy
Fox. Unless stated otherwise, please refer back to the original sources for
definitions and technical details accessible via the links at the bottom of this
document.

## Authors

This package was designed by Theodore Chronis, Christina Tatli, and Panaghis
Mavrokefalos, in collaboration with Denisa Mindruta.

## Installation

You can install the development version of **maxscoreest** from
[GitHub](https://github.com/) with:

```r
install.packages("devtools")
devtools::install_github("ConsideredHarmless/MSE-R")
```

## Example

A demonstration using synthetic data:

```r
library(maxscoreest)
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")
data <- importMatched(filename)
ineqmembers <- Cineqmembers(data$mate)
dataArray <- CdataArray(data$distanceMatrices, ineqmembers)
coefficient1 <- 1
bounds <- makeBounds(data$noAttr, 100)
optimParams <- list(lower=bounds$lower, upper=bounds$upper, NP=50, F=0.6, CR=0.5,
                    itermax=100, trace=FALSE, reltol=1e-3)
optimizeScoreArgs <- list(dataArray = dataArray,
                          coefficient1 = coefficient1,
                          optimParams = optimParams,
                          getIneqSat = TRUE,
                          permuteInvariant = TRUE)
optResult <- do.call(optimizeScoreFunction, optimizeScoreArgs)
print(optResult)
print(calcPerMarketStats(optResult$ineqSat, makeGroupIDs(ineqmembers)))
```

## References

- David Santiago and Fox, Jeremy. “A Toolkit for Matching Maximum Score
Estimation and Point and Set Identified Subsampling Inference”. 2009. Last
accessed from 
<http://fox.web.rice.edu/computer-code/matchestimation-452-documen.pdf>
- Fox, Jeremy, “Estimating Matching Games with Transfers,” 2016. Last accessed
from <http://fox.web.rice.edu/working-papers/fox-matching-maximum-score.pdf>
- Fox J. 2010. Identification in matching games. Quantitative Economics 1:
203–254
