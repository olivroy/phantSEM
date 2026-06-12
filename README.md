
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phantSEM

<!-- badges: start -->

[![R-CMD-check](https://github.com/argeorgeson/phantSEM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/argeorgeson/phantSEM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of phantSEM is to make it easier to create phantom variables,
which are variables that were not observed, for the purpose of
sensitivity analyses for structural equation models. The package allows
a user to test different combinations of covariances between the phantom
variable(s) and observed variables.

## Installation

You can install phantSEM from CRAN:

``` r
install.packages("phantSEM")
```

You can install the development version of phantSEM from
[GitHub](https://github.com/argeorgeson/phantSEM) with:

``` r
# install.packages("devtools")
devtools::install_github("argeorgeson/phantSEM")
```

## Example

This is an example that shows you how to use the package. Assume that
you have a simple mediation model with three variables, X, M2, and Y2.
You can use the phantSEM package to create phantom variables for
baseline observations of M and Y.

``` r
library(phantSEM)
## basic example code
covmatrix <- matrix(c(
  0.25, 0.95, 0.43,
  0.95, 8.87, 2.66,
  0.43, 2.66, 10.86
), nrow = 3, byrow = TRUE)
colnames(covmatrix) <- c("X", "M2", "Y2")

# lavann syntax for observed model
observed <- " M2 ~ X
              Y2 ~ M2+X "

# lavaan output
obs_output <- lavaan::sem(model = observed, sample.cov = covmatrix, sample.nobs = 200)

summary(obs_output)
#> Length  Class   Mode 
#>      1 lavaan     S4
```

``` r
# lavaan syntax for phantom variable model
phantom <- " M2 ~ M1 + Y1 + a*X
                Y2 ~ M1 + Y1 + b*M2 + cp*X "

Step1 <- SA_step1(
  lavoutput = obs_output,
  mod_obs = observed,
  mod_phant = phantom
)
#> Here are the phantom covariance matrix parameters (copy and paste and add values/names for step2):
#> 
#> 
#> phantom_assignment <-("CovM1M2"= ,
#> "CovM1X"= ,
#> "CovM1Y2"= ,
#> "CovY1M1"= ,
#> "CovY1M2"= ,
#> "CovY1X"= ,
#> "CovY1Y2"= ,
#> "VarM1"= ,
#> "VarY1"= )
#> 
#>  Choose the names of the phantom covariances that you want to fix to single values and put in a vector. These will be used for the fixed_names argument in the SA_step2 function.  The phantom covariance parameters that you want to vary should be put in a list and used as the test_names argument.
#> Here are the observed covariance matrix parameters:
#> CovXM2,CovXY2,CovY2M2
#> Choose which values you want to use for your fixed parameters and put their names in a vector (fixed_values). Make sure the order is the same for both vectors.
```

``` r

phantom_assignment <- list(
  "CovM1X" = 0,
  "CovY1M1" = "CovY2M2",
  "CovY1X" = 0,
  "VarM1" = 1,
  "VarY1" = 1,
  "CovM1M2" = seq(0, .6, .1),
  "CovY1Y2" = "CovM1M2",
  "CovY1M2" = seq(-.6, .6, .1),
  "CovM1Y2" = "CovY1M2"
)
Step2 <- SA_step2(
  phantom_assignment = phantom_assignment,
  step1 = Step1
)
Step3 <- SA_step3(
  step2 = Step2,
  n = 200
)

b_results <- ghost_par_ests(
  step3 = Step3,
  parameter_label = "b",
  remove_NA = TRUE
)

head(b_results)
#>            rep lhs op rhs label       est         se         z       pvalue
#> reptemp      1  Y2  ~  M2     b 1.1355601 0.09909786 11.458977 0.000000e+00
#> reptemp.7    8  Y2  ~  M2     b 0.6082658 0.10339672  5.882834 4.032996e-09
#> reptemp.8    9  Y2  ~  M2     b 1.1274865 0.08421692 13.387885 0.000000e+00
#> reptemp.14  15  Y2  ~  M2     b 0.3983970 0.10131046  3.932436 8.408924e-05
#> reptemp.15  16  Y2  ~  M2     b 0.6894810 0.09548591  7.220762 5.169198e-13
#> reptemp.16  17  Y2  ~  M2     b 1.1244882 0.07796970 14.422119 0.000000e+00
#>             ci.lower  ci.upper
#> reptemp    0.9413319 1.3297883
#> reptemp.7  0.4056119 0.8109196
#> reptemp.8  0.9624244 1.2925486
#> reptemp.14 0.1998321 0.5969618
#> reptemp.15 0.5023321 0.8766300
#> reptemp.16 0.9716704 1.2773060
```

## Help

If you encounter errors in the package, please submit an issue with a
minimal reproducible example on Github or email the package maintainer.
