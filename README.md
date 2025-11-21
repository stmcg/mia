
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iai

<!-- badges: start -->

[![R-CMD-check](https://github.com/stmcg/iai/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stmcg/iai/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stmcg/iai/graph/badge.svg)](https://app.codecov.io/gh/stmcg/iai)
<!-- badges: end -->

The `iai` package implements methods to estimate conditional outcome
means in settings with missingness-not-at-random and incomplete
auxiliary variables. Specifically, this package implements the AF4
method in Mathur et al. (In preparation).

## Installation

You can install the development version of `iai` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stmcg/iai")
```

## Example

We first load the package.

``` r
library(iai)
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
#> 5  6.113787  1  1  1
#> 6        NA  1  1 NA
#> 7        NA NA NA  0
#> 8        NA NA NA  0
#> 9        NA  1 NA  0
#> 10 6.439700  1  1 NA
#> 11 6.859992  1 NA  1
```

#### AF4 Method

The AF4 method estimates $E_{AF4}[ Y | X=x ]$, which is identified by
$\int_{W} E [ Y | X=x, W, M=1 ] p( W | X=x, R_W = R_X = 1 ) dW$ where
$R_W$ and $R_X$ are indicators of non-missing values of $W$ and $X$,
respectively, and $M$ is an indicator of a complete case pattern (i.e.,
$Y$, $X$, and $W$ are non-missing). The AF4 method estimates
$E_{AF4}[ Y | X=x]$ by fitting models for the conditional mean of $Y$
and conditional density of $W$ and performing Monte Carlo integration to
compute the integral.

The function `af4` implements the AF4 method to estimate
$E_{AF4}[ Y | X=x_1 ]$ and, optionally, $E_{AF4}[ Y | X=x_2]$ as well as
contrasts between $E_{AF4}[ Y | X=x_1 ]$ and $E_{AF4}[ Y | X=x_2 ]$
(differences, ratios). This function requires specifying the following
models:

- `Y_model`: Formula for the outcome model
- `W_model`: Formula for the auxiliary model when the auxiliary variable
  is univariate, or a list of formulas for each component of the
  auxiliary variable

It also requires specifying the names of the variable(s) $X$ by
`X_names` and their values $x_1$ and $x_2$ by `X_values_1` and
`X_values_2`, respectively.

An application of `af4` to estimate $E_{AF4} [ Y | X=(0, 1)^\top ]$ and
$E_{AF4} [ Y | X = (0, 0)^\top ]$ as well as their difference is given
below:

``` r
set.seed(1234)
res <- af4(data = dat.sim,
           X_names = c("X1", "X2"), 
           X_values_1 = c(0, 1), X_values_2 = c(0, 0),
           Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
res
#> AF4 METHOD FOR CONDITIONAL MEAN ESTIMATION
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
the `get_CI` function to the output of the `af4` function. The `get_CI`
function performs nonparametric bootstrap. Here, we use the percentile
method with 100 bootstrap replicates (for ease of computation):

``` r
get_CI(res, n_boot = 100, type = 'perc')
#> BOOTSTRAP CONFIDENCE INTERVALS FOR AF4 METHOD
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
