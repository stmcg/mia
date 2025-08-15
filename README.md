
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iai

<!-- badges: start -->
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

We will use the example dataset `dat.sim` included the package. The
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

The function `af4` implements the AF4 method. This function requires
specifying the following models:

- `Y_model`: Formula for the outcome model
- `W_model`: Formula for the auxiliary model

It also requires specifying the names of the variable(s) $X$ by
`X_names` and their values $x$ in $E_{AF4} [ Y | X=x ]$ by `X_values`.

An application of `af4` to estimate $E_{AF4} [ Y | X_1=0, X_2 = 1 ]$ is
given below:

``` r
set.seed(1234)
res <- af4(data = dat.sim,
           X_names = c("X1", "X2"), X_values = c(0, 1),
           Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
```

The estimate of $E_{AF4} [ Y | X_1=0, X_2 = 1 ]$ is given below:

``` r
res$mean_est
#> [1] 2.133519
```
