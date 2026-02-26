
<!-- README.md is generated from README.Rmd. Please edit that file -->

# miapack

<!-- badges: start -->

[![R-CMD-check](https://github.com/stmcg/miapack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stmcg/miapack/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stmcg/miapack/graph/badge.svg)](https://app.codecov.io/gh/stmcg/miapack)
<!-- badges: end -->

The `miapack` package implements methods to estimate conditional outcome
means in settings with missingness-not-at-random and incomplete
auxiliary variables. Specifically, this package implements the
marginalization over incomplete auxiliaries (MIA) method proposed by
[Mathur et al. (2026)](doi.org/10.13140/RG.2.2.30750.19524).

## Installation

You can install the development version of `miapack` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stmcg/miapack")
```

## Example

We first load the package.

``` r
library(miapack)
```

#### Data Set

We will use the example dataset `dat.sim` included in the package. The
dataset contains 9,297 observations with a continuous outcome `Y`, a
binary auxiliary variable `W`, and binary predictors `X1` and `X2`. The
first 10 rows of `dat.sim` are:

``` r
dat.sim[1:10,]
#>           Y X1 X2  W
#> 1        NA  0 NA  0
#> 2        NA  1  1 NA
#> 3  6.066826  1  1  1
#> 4  6.113787  1  1  1
#> 5        NA  1  1 NA
#> 6        NA NA NA  0
#> 7        NA NA NA  0
#> 8        NA  1 NA  0
#> 9  6.439700  1  1 NA
#> 10 6.859992  1 NA  1
```

#### MIA Method

The MIA method estimates the conditional outcome mean
$\mu_{\text{MIA}}(x)$, which is identified by
$$\int_{w} E [ Y | X=x, W=w, M=1 ] p( w | X=x, R_W = R_X = 1 ) dw$$
where $R_W$ and $R_X$ are indicators of non-missing values of $W$ and
$X$, respectively, and $M$ is an indicator of a complete case pattern
(i.e., $Y$, $X$, and $W$ are non-missing). The MIA method estimates the
identifying functional by fitting models for the conditional mean of $Y$
and conditional density of $W$ and performing Monte Carlo integration to
compute the integral.

The function `mia` implements the MIA method to obtain point estimates
of the identifying functionals of $\mu_{\text{MIA}}(x_1)$ and
$\mu_{\text{MIA}}(x_2)$ as well as contrasts between them (differences,
ratios). This function requires specifying the following regression
models:

- `Y_model`: Formula for the outcome model
- `W_model`: Formula for the auxiliary model when the auxiliary variable
  is univariate, or a list of formulas for each component of the
  auxiliary variable

It also requires specifying the names of the variable(s) $X$ by
`X_names` and their values $x_1$ and $x_2$ by `X_values_1` and
`X_values_2`, respectively.

An application of `mia` to estimate
$\mu_{\text{MIA}}(x_{1,1} = 0, x_{1,2} = 1)$ and
$\mu_{\text{MIA}}(x_{2,1} = 0, x_{2,2} = 0)$ as well as their difference
is given below. Note that we set a random number seed because the
function involves performing Monte Carlo integration.

``` r
set.seed(1234)
res <- mia(data = dat.sim,
           X_names = c("X1", "X2"), 
           X_values_1 = c(0, 1), X_values_2 = c(0, 0),
           Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
res
#> MIA METHOD FOR CONDITIONAL MEAN ESTIMATION
#> ==========================================
#> 
#> Setting:
#>   Outcome variable type:       continuous
#>   Auxiliary variable(s) type:  binary (W)
#> 
#> Results:
#>   Predictor values:            X1=0, X2=1
#>   Mean estimate:               2.1335
#> 
#>   Predictor values:            X1=0, X2=0
#>   Mean estimate:               -0.1636
#> 
#>   Mean difference estimate:    2.2971
```

We can obtain 95% confidence intervals around our estimates by applying
the `get_CI` function to the output of the `mia` function. The `get_CI`
function performs nonparametric bootstrap. Here, we use the percentile
method with 100 bootstrap replicates for ease of computation.

``` r
get_CI(res, n_boot = 100, type = 'perc')
#> BOOTSTRAP CONFIDENCE INTERVALS FOR MIA METHOD
#> =============================================
#> 
#> Setting:
#>   Confidence level:        0.95
#>   Interval type:           perc
#>   Number of replicates:    100
#> 
#> Results:
#>   Predictor values:        X1=0, X2=1
#>   CI for mean:             (2.0350, 2.2495)
#> 
#>   Predictor values:        X1=0, X2=0
#>   CI for mean:             (-0.2638, -0.0588)
#> 
#>   CI for difference:       (2.1565, 2.4539)
```
