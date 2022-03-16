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
devtools::install_github("ConsideredHarmless/MSE-R", ref = "package")
```

## Example

A demonstration using synthetic data:

```r
library(maxscoreest)
filename <- system.file("extdata", "precomp_testdata.dat", package = "maxscoreest")
matchedData <- importMatched(filename)
ineqmembers <- Cineqmembers(matchedData$mate)
dataArray <- CdataArray(matchedData$distanceMatrices, ineqmembers)
bounds <- makeBounds(matchedData$noAttr, 100)
optimParams <- getDefaultOptimParams()
set.seed(42)
optResult <- optimizeScoreFunction(
    dataArray = dataArray,
    bounds = bounds,
    optimParams = optimParams,
    getIneqSat = TRUE,
    permuteInvariant = TRUE
)
print(optResult$optArg)
print(optResult$optVal)
print(calcPerMarketStats(optResult$ineqSat, makeGroupIDs(ineqmembers)))
```

For an in-depth look, you can read the vignettes provided with the package:

```r
browseVignettes("maxscoreest")
```

or, using the RStudio browser,

```r
vignette("matched", package = "maxscoreest")
vignette("unmatched", package = "maxscoreest")
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
